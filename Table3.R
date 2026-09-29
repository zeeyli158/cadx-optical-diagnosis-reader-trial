library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)

input_file <- file.path("data", "rjai_total.xlsx")
output_dir <- file.path("results", "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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

format_p <- function(p) {
  ifelse(
    is.na(p),
    "—",
    ifelse(p < 0.01, "<0.01", formatC(p, format = "f", digits = 2))
  )
}

format_median_iqr <- function(x) {
  x <- x[!is.na(x)]
  sprintf(
    "%.1f%% (%.1f%%–%.1f%%)",
    median(x),
    quantile(x, 0.25),
    quantile(x, 0.75)
  )
}

calculate_metrics <- function(data) {
  true_positive <- sum(data$prediction == 2 & data$gold == 2)
  true_negative <- sum(data$prediction == 1 & data$gold == 1)
  false_positive <- sum(data$prediction == 2 & data$gold == 1)
  false_negative <- sum(data$prediction == 1 & data$gold == 2)
  total <- true_positive + true_negative + false_positive + false_negative

  tibble(
    accuracy = 100 * (true_positive + true_negative) / total,
    sensitivity = if_else(
      true_positive + false_negative > 0,
      100 * true_positive / (true_positive + false_negative),
      NA_real_
    ),
    specificity = if_else(
      true_negative + false_positive > 0,
      100 * true_negative / (true_negative + false_positive),
      NA_real_
    ),
    ppv = if_else(
      true_positive + false_positive > 0,
      100 * true_positive / (true_positive + false_positive),
      NA_real_
    ),
    npv = if_else(
      true_negative + false_negative > 0,
      100 * true_negative / (true_negative + false_negative),
      NA_real_
    )
  )
}

long_data <- raw_data %>%
  select(gold, all_of(physician_columns)) %>%
  pivot_longer(
    cols = all_of(physician_columns),
    names_to = "doctor_col",
    values_to = "response"
  ) %>%
  mutate(
    paper = str_extract(doctor_col, "^卷\\s*[123]") %>%
      str_replace_all("\\s+", "") %>%
      factor(levels = paper_levels),
    group = recode(as.character(paper), !!!paper_to_group) %>%
      factor(levels = group_levels),
    prediction = as.integer(str_extract(as.character(response), "^[12]")),
    confidence = if_else(
      str_detect(str_to_lower(as.character(response)), "high|hi") |
        str_detect(as.character(response), "高"),
      "high",
      "low"
    ),
    gold = as.integer(gold)
  )

doctor_metrics_all <- long_data %>%
  group_by(paper, group, doctor_col) %>%
  group_modify(~ calculate_metrics(.x)) %>%
  ungroup()

doctor_high_rate <- long_data %>%
  group_by(paper, group, doctor_col) %>%
  summarise(
    high_rate = 100 * mean(confidence == "high"),
    .groups = "drop"
  )

doctor_metrics_high <- long_data %>%
  filter(confidence == "high") %>%
  group_by(paper, group, doctor_col) %>%
  group_modify(~ calculate_metrics(.x)) %>%
  ungroup()

analyse_metric <- function(data, value_column, section, metric) {
  analysis_data <- data %>%
    transmute(
      group = factor(group, levels = group_levels),
      value = .data[[value_column]]
    ) %>%
    filter(!is.na(value))

  overall_test <- kruskal.test(value ~ group, data = analysis_data)

  overall <- tibble(
    Section = section,
    Metric = metric,
    test = "Kruskal-Wallis",
    statistic = unname(overall_test$statistic),
    df = unname(overall_test$parameter),
    p_value = overall_test$p.value,
    p_display = format_p(overall_test$p.value)
  )

  pairwise <- tibble(
    Section = character(),
    Metric = character(),
    comparison = character(),
    test = character(),
    statistic = numeric(),
    p_value = numeric(),
    p_adjusted_holm = numeric(),
    p_adjusted_display = character()
  )

  if (overall_test$p.value < 0.05) {
    pairwise <- combn(group_levels, 2, simplify = FALSE) %>%
      map_dfr(function(groups) {
        subset_data <- analysis_data %>%
          filter(group %in% groups) %>%
          mutate(group = droplevels(group))

        wilcox_result <- wilcox.test(
          value ~ group,
          data = subset_data,
          exact = FALSE
        )

        tibble(
          Section = section,
          Metric = metric,
          comparison = paste(groups, collapse = " vs "),
          test = "Wilcoxon rank-sum",
          statistic = unname(wilcox_result$statistic),
          p_value = wilcox_result$p.value
        )
      }) %>%
      mutate(
        p_adjusted_holm = p.adjust(p_value, method = "holm"),
        p_adjusted_display = format_p(p_adjusted_holm)
      )
  }

  group_values <- analysis_data %>%
    group_by(group) %>%
    summarise(display = format_median_iqr(value), .groups = "drop") %>%
    pivot_wider(names_from = group, values_from = display)

  pairwise_display <- c(
    "Group A vs Group B" = "—",
    "Group A vs Group C" = "—",
    "Group B vs Group C" = "—"
  )

  if (nrow(pairwise) > 0) {
    pairwise_display[pairwise$comparison] <- pairwise$p_adjusted_display
  }

  row <- group_values %>%
    transmute(
      Section = section,
      Metric = metric,
      `Group A` = `Group A`,
      `Group B` = `Group B`,
      `Group C` = `Group C`,
      `Overall P value` = format_p(overall_test$p.value),
      `A vs B*` = unname(pairwise_display["Group A vs Group B"]),
      `A vs C*` = unname(pairwise_display["Group A vs Group C"]),
      `B vs C*` = unname(pairwise_display["Group B vs Group C"])
    )

  list(row = row, overall = overall, pairwise = pairwise)
}

all_polyps_results <- list(
  analyse_metric(doctor_metrics_all, "accuracy", "All polyps", "Accuracy, %"),
  analyse_metric(doctor_metrics_all, "sensitivity", "All polyps", "Sensitivity, %"),
  analyse_metric(doctor_metrics_all, "specificity", "All polyps", "Specificity, %"),
  analyse_metric(doctor_metrics_all, "ppv", "All polyps", "PPV, %"),
  analyse_metric(doctor_metrics_all, "npv", "All polyps", "NPV, %")
)

high_confidence_results <- list(
  analyse_metric(
    doctor_high_rate,
    "high_rate",
    "High-confidence diagnoses",
    "High-confidence rate, %"
  ),
  analyse_metric(
    doctor_metrics_high,
    "accuracy",
    "High-confidence diagnoses",
    "Accuracy, %"
  ),
  analyse_metric(
    doctor_metrics_high,
    "sensitivity",
    "High-confidence diagnoses",
    "Sensitivity, %"
  ),
  analyse_metric(
    doctor_metrics_high,
    "specificity",
    "High-confidence diagnoses",
    "Specificity, %"
  ),
  analyse_metric(
    doctor_metrics_high,
    "ppv",
    "High-confidence diagnoses",
    "PPV, %"
  ),
  analyse_metric(
    doctor_metrics_high,
    "npv",
    "High-confidence diagnoses",
    "NPV, %"
  )
)

section_row <- function(label) {
  tibble(
    Section = label,
    Metric = "",
    `Group A` = "",
    `Group B` = "",
    `Group C` = "",
    `Overall P value` = "",
    `A vs B*` = "",
    `A vs C*` = "",
    `B vs C*` = ""
  )
}

table3 <- bind_rows(
  section_row("All polyps"),
  map_dfr(all_polyps_results, "row") %>% mutate(Section = ""),
  section_row("High-confidence diagnoses"),
  map_dfr(high_confidence_results, "row") %>% mutate(Section = "")
)

overall_tests <- bind_rows(
  map(all_polyps_results, "overall"),
  map(high_confidence_results, "overall")
)

pairwise_tests <- bind_rows(
  map(all_polyps_results, "pairwise"),
  map(high_confidence_results, "pairwise")
)

print(table3, n = Inf, width = Inf)

write_xlsx(
  list(Table_3 = table3),
  file.path(output_dir, "Table3.xlsx")
)

write_xlsx(
  list(
    Overall_tests = overall_tests,
    Pairwise_tests_Holm = pairwise_tests
  ),
  file.path(output_dir, "Table3_pvalue.xlsx")
)
