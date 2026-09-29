library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
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

physician_columns <- names(raw_data) %>%
  str_subset("^卷\\s*[123]_")

normalize_histology <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))

  case_when(
    x %in% c("adenoma", "tubular adenoma") ~ "adenoma",
    x %in% c("hp", "hyperplastic polyp") ~ "HP",
    x %in% c(
      "ssp", "ssl", "sessile serrated lesion",
      "sessile serrated polyp"
    ) ~ "SSL",
    x %in% c(
      "adenoma_villous", "villous adenoma",
      "tubulovillous adenoma", "tubulovillous"
    ) ~ "adenoma_villous",
    x == "other" ~ "other",
    TRUE ~ "review"
  )
}

assign_interval <- function(histology, size_mm, guideline) {
  if (any(histology == "review", na.rm = TRUE)) {
    return("review")
  }

  adenoma <- histology %in% c("adenoma", "adenoma_villous")
  n_adenoma <- sum(adenoma, na.rm = TRUE)
  large_adenoma <- any(adenoma & size_mm >= 10, na.rm = TRUE)
  large_ssl <- any(histology == "SSL" & size_mm >= 10, na.rm = TRUE)

  high_risk <- if (guideline == "ESGE") {
    large_adenoma || n_adenoma >= 5 || large_ssl
  } else {
    large_adenoma ||
      n_adenoma >= 3 ||
      any(histology == "adenoma_villous", na.rm = TRUE) ||
      large_ssl
  }

  if_else(high_risk, "3 years", "10 years")
}

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

format_median_iqr <- function(x) {
  sprintf(
    "%.1f%% (%.1f%%–%.1f%%)",
    median(x),
    quantile(x, 0.25),
    quantile(x, 0.75)
  )
}

format_pass_rate <- function(events, total) {
  interval <- 100 * wilson_ci(events, total)

  sprintf(
    "%.1f%% (%.1f%%–%.1f%%)",
    100 * events / total,
    interval["lower"],
    interval["upper"]
  )
}

base_data <- raw_data %>%
  transmute(
    lesion_id = as.character(new_file_name),
    patient_id = as.character(ID),
    size_mm = as.numeric(Size),
    histology_gold = normalize_histology(Histology),
    across(all_of(physician_columns))
  )

reader_long <- base_data %>%
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
    optical_diagnosis = as.integer(str_extract(as.character(response), "^[12]")),
    confidence = if_else(
      str_detect(str_to_lower(as.character(response)), "high|hi") |
        str_detect(as.character(response), "高"),
      "high",
      "low"
    ),
    assigned_histology = case_when(
      size_mm <= 5 & confidence == "high" & optical_diagnosis == 1 ~ "HP",
      size_mm <= 5 & confidence == "high" & optical_diagnosis == 2 ~ "adenoma",
      TRUE ~ histology_gold
    )
  )

reconstruct_guideline <- function(guideline_name) {
  gold_intervals <- base_data %>%
    group_by(patient_id) %>%
    summarise(
      reference_interval = assign_interval(
        histology_gold,
        size_mm,
        guideline_name
      ),
      .groups = "drop"
    )

  reader_intervals <- reader_long %>%
    group_by(patient_id, physician_id, paper, group) %>%
    summarise(
      reconstructed_interval = assign_interval(
        assigned_histology,
        size_mm,
        guideline_name
      ),
      .groups = "drop"
    ) %>%
    left_join(gold_intervals, by = "patient_id") %>%
    filter(
      reconstructed_interval != "review",
      reference_interval != "review"
    ) %>%
    mutate(concordant = reconstructed_interval == reference_interval)

  physician_results <- reader_intervals %>%
    group_by(physician_id, paper, group) %>%
    summarise(
      total_patients = n(),
      concordant_patients = sum(concordant),
      agreement_pct = 100 * mean(concordant),
      reached_90_percent = agreement_pct >= 90,
      .groups = "drop"
    ) %>%
    mutate(Guideline = guideline_name)

  list(
    reference_intervals = gold_intervals,
    reader_intervals = reader_intervals,
    physician_results = physician_results
  )
}

esge_results <- reconstruct_guideline("ESGE")
asia_pacific_results <- reconstruct_guideline("Asia-Pacific")

physician_results <- bind_rows(
  esge_results$physician_results,
  asia_pacific_results$physician_results
)

table_long <- physician_results %>%
  group_by(Guideline, group) %>%
  summarise(
    physicians = n(),
    passed = sum(reached_90_percent),
    pass_rate = format_pass_rate(passed, physicians),
    agreement = format_median_iqr(agreement_pct),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(pass_rate, agreement),
    names_to = "outcome",
    values_to = "display"
  ) %>%
  mutate(
    outcome = recode(
      outcome,
      pass_rate = "Pass rate (≥90% surveillance interval agreement), % (95% CI)",
      agreement = "Surveillance interval agreement, median % (IQR)"
    ),
    group = factor(group, levels = group_levels),
    Guideline = factor(Guideline, levels = c("ESGE", "Asia-Pacific"))
  ) %>%
  arrange(outcome, group, Guideline) %>%
  pivot_wider(names_from = Guideline, values_from = display)

pass_rows <- table_long %>%
  filter(str_starts(outcome, "Pass rate")) %>%
  arrange(group) %>%
  mutate(outcome = c(
    "Pass rate (≥90% surveillance interval agreement), % (95% CI)",
    "",
    ""
  ))

agreement_rows <- table_long %>%
  filter(str_starts(outcome, "Surveillance interval agreement")) %>%
  arrange(group) %>%
  mutate(outcome = c(
    "Surveillance interval agreement, median % (IQR)",
    "",
    ""
  ))

table_s3 <- bind_rows(pass_rows, agreement_rows) %>%
  transmute(
    Outcome = outcome,
    `Trial group` = as.character(group),
    ESGE,
    `Asia-Pacific`
  )

print(table_s3, n = Inf, width = Inf)

write_xlsx(
  list(Table_S3 = table_s3),
  file.path(table_dir, "TableS3.xlsx")
)

soft_colours <- c(
  "Group A" = "#A9C5E8",
  "Group B" = "#B9DEC9",
  "Group C" = "#F2CBB7"
)

plot_theme <- theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, colour = "black"),
    axis.text.x = element_text(size = 11, colour = "black"),
    axis.text.y = element_text(size = 11, colour = "black"),
    plot.tag = element_text(size = 12, colour = "black"),
    plot.tag.position = c(0.01, 0.98),
    plot.title = element_text(size = 13, hjust = 0.5, colour = "black"),
    axis.line = element_line(linewidth = 0.6, colour = "black"),
    axis.ticks = element_line(linewidth = 0.5, colour = "black"),
    plot.margin = margin(8, 10, 8, 8)
  )

make_agreement_plot <- function(data, title, show_violin) {
  plot <- ggplot(
    data,
    aes(x = group, y = agreement_pct, fill = group, colour = group)
  )

  if (show_violin) {
    plot <- plot +
      geom_violin(
        width = 0.9,
        alpha = 0.25,
        linewidth = 0.4,
        trim = TRUE,
        scale = "width",
        adjust = 0.8
      )
  }

  plot +
    geom_boxplot(
      width = 0.20,
      outlier.shape = NA,
      fill = "white",
      colour = "grey25",
      linewidth = 0.5
    ) +
    geom_point(
      position = position_jitter(width = 0.08, height = 0, seed = 123456),
      size = 1.8,
      alpha = 0.80,
      stroke = 0
    ) +
    stat_summary(
      fun = median,
      geom = "point",
      shape = 23,
      size = 2.5,
      fill = "white",
      colour = "grey25",
      stroke = 0.7
    ) +
    geom_hline(
      yintercept = 90,
      linetype = "dashed",
      linewidth = 0.6,
      colour = "grey45"
    ) +
    annotate(
      "text",
      x = 3.25,
      y = 90.5,
      label = "90%",
      hjust = 0,
      vjust = -0.1,
      size = 4,
      colour = "grey35"
    ) +
    scale_fill_manual(values = soft_colours) +
    scale_colour_manual(values = soft_colours) +
    scale_y_continuous(
      breaks = seq(85, 100, by = 5),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.02, 0.04))
    ) +
    coord_cartesian(ylim = c(84.5, 101.8)) +
    labs(
      title = title,
      y = "Surveillance interval agreement (%)"
    ) +
    plot_theme
}

panel_a <- physician_results %>%
  filter(Guideline == "ESGE") %>%
  make_agreement_plot("ESGE", show_violin = FALSE)

panel_b <- physician_results %>%
  filter(Guideline == "Asia-Pacific") %>%
  make_agreement_plot("Asia-Pacific", show_violin = TRUE)

figure_s4 <- panel_a + panel_b +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A")

print(figure_s4)

ggsave(
  file.path(figure_dir, "FigureS4.png"),
  figure_s4,
  width = 13,
  height = 6.2,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(figure_dir, "FigureS4.pdf"),
  figure_s4,
  width = 13,
  height = 6.2,
  bg = "white"
)
