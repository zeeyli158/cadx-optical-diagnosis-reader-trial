library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)
library(ggplot2)
library(patchwork)

input_file <- file.path("data", "rjai_total.xlsx")
table_dir <- file.path("results", "tables")
figure_dir <- file.path("results", "figures")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

raw_data <- read_excel(input_file, sheet = "Sheet1")

paper_levels <- c("卷1", "卷2", "卷3")
group_levels <- c("Group A", "Group B", "Group C")
paper_to_group <- c(
  "卷1" = "Group A",
  "卷2" = "Group B",
  "卷3" = "Group C"
)
rectosigmoid_sites <- c("Rectum", "Sigmoid colon")

physician_columns <- names(raw_data) %>%
  str_subset("^卷\\s*[123]_")

wilson_ci <- function(events, total, confidence_level = 0.95) {
  z <- qnorm(1 - (1 - confidence_level) / 2)
  proportion <- events / total
  denominator <- 1 + z^2 / total
  centre <- (proportion + z^2 / (2 * total)) / denominator
  half_width <- z * sqrt(
    proportion * (1 - proportion) / total + z^2 / (4 * total^2)
  ) / denominator

  c(
    lower = max(0, centre - half_width),
    upper = min(1, centre + half_width)
  )
}

format_rate_ci <- function(events, total) {
  interval <- 100 * wilson_ci(events, total)

  sprintf(
    "%.1f%% (%.1f%%–%.1f%%)",
    100 * events / total,
    interval["lower"],
    interval["upper"]
  )
}

format_p <- function(p) {
  ifelse(
    is.na(p),
    "—",
    ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 2))
  )
}

calculate_soda_metrics <- function(data) {
  true_positive <- sum(data$prediction == 2 & data$gold == 2)
  true_negative <- sum(data$prediction == 1 & data$gold == 1)
  false_positive <- sum(data$prediction == 2 & data$gold == 1)
  false_negative <- sum(data$prediction == 1 & data$gold == 2)

  tibble(
    sensitivity = if_else(
      true_positive + false_negative > 0,
      100 * true_positive / (true_positive + false_negative),
      NA_real_
    ),
    specificity = if_else(
      true_negative + false_positive > 0,
      100 * true_negative / (true_negative + false_positive),
      NA_real_
    )
  )
}

reader_long <- raw_data %>%
  transmute(
    lesion_id = as.character(new_file_name),
    gold = as.integer(gold),
    size_mm = as.numeric(Size),
    lesion_site = as.character(Location),
    across(all_of(physician_columns))
  ) %>%
  pivot_longer(
    cols = all_of(physician_columns),
    names_to = "physician_id",
    values_to = "response"
  ) %>%
  mutate(
    paper = str_extract(physician_id, "^卷\\s*[123]") %>%
      str_replace_all("\\s+", "") %>%
      factor(levels = paper_levels),
    group = recode(as.character(paper), !!!paper_to_group) %>%
      factor(levels = group_levels),
    prediction = as.integer(str_extract(as.character(response), "^[12]")),
    confidence = if_else(
      str_detect(str_to_lower(as.character(response)), "high") |
        str_detect(as.character(response), "高"),
      "high",
      "low"
    )
  )

calculate_strategy <- function(
    data,
    strategy,
    sensitivity_threshold,
    specificity_threshold,
    rectosigmoid_only = FALSE) {

  eligible_data <- data %>%
    filter(
      size_mm <= 5,
      confidence == "high",
      !rectosigmoid_only | lesion_site %in% rectosigmoid_sites
    )

  eligible_data %>%
    group_by(paper, group, physician_id) %>%
    group_modify(~ calculate_soda_metrics(.x)) %>%
    ungroup() %>%
    mutate(
      strategy = strategy,
      passed = case_when(
        is.na(sensitivity) | is.na(specificity) ~ NA_integer_,
        sensitivity >= sensitivity_threshold &
          specificity >= specificity_threshold ~ 1L,
        TRUE ~ 0L
      )
    )
}

soda_results <- bind_rows(
  calculate_strategy(
    reader_long,
    strategy = "Leave-in-situ",
    sensitivity_threshold = 90,
    specificity_threshold = 80,
    rectosigmoid_only = TRUE
  ),
  calculate_strategy(
    reader_long,
    strategy = "Resect-and-discard",
    sensitivity_threshold = 80,
    specificity_threshold = 80
  )
) %>%
  mutate(
    strategy = factor(
      strategy,
      levels = c("Leave-in-situ", "Resect-and-discard")
    ),
    group = factor(group, levels = group_levels)
  )

overall_p_value <- function(data) {
  analysis_data <- data %>%
    filter(!is.na(passed)) %>%
    mutate(passed = factor(passed, levels = c(0, 1)))

  fisher.test(table(analysis_data$group, analysis_data$passed))$p.value
}

table_s4 <- soda_results %>%
  filter(!is.na(passed)) %>%
  group_by(strategy, group) %>%
  summarise(
    events = sum(passed == 1),
    total = n(),
    result = format_rate_ci(events, total),
    .groups = "drop"
  ) %>%
  select(strategy, group, result) %>%
  pivot_wider(names_from = group, values_from = result) %>%
  mutate(
    `Overall P value` = map_dbl(
      strategy,
      ~ overall_p_value(filter(soda_results, strategy == .x))
    ),
    Outcome = paste0(strategy, " SODA pass rate, % (95% CI)")
  ) %>%
  transmute(
    Outcome,
    `Group A`,
    `Group B`,
    `Group C`,
    `Overall P value` = format_p(`Overall P value`)
  )

pairwise_results <- map_dfr(levels(soda_results$strategy), function(strategy_name) {
  strategy_data <- soda_results %>%
    filter(strategy == strategy_name, !is.na(passed))

  if (overall_p_value(strategy_data) >= 0.05) {
    return(tibble())
  }

  map_dfr(combn(group_levels, 2, simplify = FALSE), function(groups) {
    comparison_data <- strategy_data %>%
      filter(group %in% groups) %>%
      mutate(
        group = droplevels(group),
        passed = factor(passed, levels = c(0, 1))
      )

    fisher_result <- fisher.test(
      table(comparison_data$group, comparison_data$passed)
    )

    tibble(
      Outcome = paste0(strategy_name, " SODA pass rate"),
      Comparison = paste(groups, collapse = " vs "),
      `Unadjusted P value` = fisher_result$p.value
    )
  }) %>%
    mutate(
      `Holm-adjusted P value` = p.adjust(
        `Unadjusted P value`,
        method = "holm"
      )
    )
})

write_xlsx(
  list(
    `Table S4` = table_s4,
    `Pairwise comparisons` = pairwise_results
  ),
  file.path(table_dir, "TableS4.xlsx")
)

group_colours <- c(
  "Group A" = "#A9C5E8",
  "Group B" = "#B9DEC9",
  "Group C" = "#F2CBB7"
)

figure_data <- soda_results %>%
  filter(
    !is.na(sensitivity),
    !is.na(specificity),
    !is.na(passed)
  )

figure_theme <- theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 11, colour = "black"),
    axis.title = element_text(size = 13, colour = "black"),
    axis.text = element_text(size = 11, colour = "black"),
    axis.line = element_line(linewidth = 0.6),
    plot.tag = element_text(size = 13, face = "plain"),
    plot.tag.position = c(0.01, 0.98),
    plot.margin = margin(8, 10, 8, 8)
  )

make_soda_panel <- function(data, strategy_name, sensitivity_threshold) {
  ggplot(
    filter(data, strategy == strategy_name),
    aes(x = specificity, y = sensitivity, colour = group)
  ) +
    geom_vline(
      xintercept = 80,
      linetype = "dashed",
      linewidth = 0.6,
      colour = "grey45"
    ) +
    geom_hline(
      yintercept = sensitivity_threshold,
      linetype = "dashed",
      linewidth = 0.6,
      colour = "grey45"
    ) +
    geom_point(
      position = position_jitter(
        width = 0.8,
        height = 0.8,
        seed = 123456
      ),
      size = 1.5,
      alpha = 0.85
    ) +
    scale_colour_manual(values = group_colours) +
    scale_x_continuous(
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    coord_cartesian(xlim = c(0, 101), ylim = c(0, 100)) +
    labs(
      x = "Specificity (%)",
      y = "Sensitivity (%)"
    ) +
    figure_theme
}

panel_a <- make_soda_panel(
  figure_data,
  strategy_name = "Leave-in-situ",
  sensitivity_threshold = 90
)

panel_b <- make_soda_panel(
  figure_data,
  strategy_name = "Resect-and-discard",
  sensitivity_threshold = 80
)

figure_s5 <- panel_a + panel_b +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

print(table_s4, n = Inf, width = Inf)
print(pairwise_results, n = Inf, width = Inf)
print(figure_s5)

ggsave(
  file.path(figure_dir, "FigureS5.png"),
  figure_s5,
  width = 12.5,
  height = 6,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(figure_dir, "FigureS5.pdf"),
  figure_s5,
  width = 12.5,
  height = 6,
  bg = "white"
)
