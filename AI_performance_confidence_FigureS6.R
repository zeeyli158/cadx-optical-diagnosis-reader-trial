library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(pROC)
library(patchwork)
library(writexl)

input_file <- file.path("data", "rjai_total.xlsx")
table_dir <- file.path("results", "tables")
figure_dir <- file.path("results", "figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

ai_data <- read_excel(input_file, sheet = "Sheet1") %>%
  transmute(
    lesion_id = as.character(new_file_name),
    gold = as.integer(gold),
    ai_prediction = as.integer(AI_diagnosis),
    confidence = as.numeric(CI),
    correct = ai_prediction == gold,
    p_type2 = if_else(
      ai_prediction == 2L,
      confidence,
      1 - confidence
    )
  )

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

metric_row <- function(metric, numerator, denominator) {
  interval <- wilson_ci(numerator, denominator)

  tibble(
    metric,
    numerator,
    denominator,
    estimate_pct = 100 * numerator / denominator,
    ci_lower_pct = 100 * interval["lower"],
    ci_upper_pct = 100 * interval["upper"]
  )
}

average_precision <- function(truth, score) {
  positive_count <- sum(truth == 2L)

  if (positive_count == 0L || positive_count == length(truth)) {
    return(NA_real_)
  }

  tibble(truth, score) %>%
    group_by(score) %>%
    summarise(
      true_positive_added = sum(truth == 2L),
      false_positive_added = sum(truth == 1L),
      .groups = "drop"
    ) %>%
    arrange(desc(score)) %>%
    mutate(
      true_positive = cumsum(true_positive_added),
      false_positive = cumsum(false_positive_added),
      recall = true_positive / positive_count,
      precision = true_positive / (true_positive + false_positive),
      recall_increment = recall - lag(recall, default = 0)
    ) %>%
    summarise(value = sum(recall_increment * precision)) %>%
    pull(value)
}

# Standalone AI diagnostic performance

true_positive <- sum(ai_data$ai_prediction == 2L & ai_data$gold == 2L)
true_negative <- sum(ai_data$ai_prediction == 1L & ai_data$gold == 1L)
false_positive <- sum(ai_data$ai_prediction == 2L & ai_data$gold == 1L)
false_negative <- sum(ai_data$ai_prediction == 1L & ai_data$gold == 2L)

confusion_matrix <- tibble(
  reference = c("Gold type 2", "Gold type 1"),
  `AI type 2` = c(true_positive, false_positive),
  `AI type 1` = c(false_negative, true_negative),
  total = c(
    true_positive + false_negative,
    false_positive + true_negative
  )
)

diagnostic_metrics <- bind_rows(
  metric_row("Accuracy", true_positive + true_negative, nrow(ai_data)),
  metric_row("Sensitivity", true_positive, true_positive + false_negative),
  metric_row("Specificity", true_negative, true_negative + false_positive),
  metric_row(
    "Positive predictive value",
    true_positive,
    true_positive + false_positive
  ),
  metric_row(
    "Negative predictive value",
    true_negative,
    true_negative + false_negative
  )
)

roc_object <- pROC::roc(
  response = ai_data$gold,
  predictor = ai_data$p_type2,
  levels = c(1, 2),
  direction = "<",
  quiet = TRUE
)

roc_auc <- as.numeric(pROC::auc(roc_object))
roc_ci <- as.numeric(pROC::ci.auc(roc_object, method = "delong"))
pr_auc <- average_precision(ai_data$gold, ai_data$p_type2)

set.seed(123456)
pr_bootstrap <- replicate(2000, {
  bootstrap_index <- sample.int(nrow(ai_data), replace = TRUE)
  average_precision(
    ai_data$gold[bootstrap_index],
    ai_data$p_type2[bootstrap_index]
  )
})
pr_ci <- quantile(
  pr_bootstrap,
  probs = c(0.025, 0.975),
  na.rm = TRUE,
  names = FALSE
)

auc_metrics <- tibble(
  metric = c("AUC-ROC", "AUC-PR"),
  estimate = c(roc_auc, pr_auc),
  ci_lower = c(roc_ci[1], pr_ci[1]),
  ci_upper = c(roc_ci[3], pr_ci[2]),
  ci_method = c(
    "DeLong",
    "Nonparametric bootstrap, 2,000 resamples"
  )
)

# Confidence-score distribution and descriptive calibration

total_lesions <- nrow(ai_data)
overall_correct <- sum(ai_data$correct)
overall_accuracy_ci <- wilson_ci(overall_correct, total_lesions)

confidence_distribution <- ai_data %>%
  summarise(
    n_lesions = n(),
    correct_predictions = sum(correct),
    observed_accuracy_pct = 100 * mean(correct),
    accuracy_ci_lower_pct = 100 * overall_accuracy_ci["lower"],
    accuracy_ci_upper_pct = 100 * overall_accuracy_ci["upper"],
    mean_confidence_pct = 100 * mean(confidence),
    median_confidence_pct = 100 * median(confidence),
    q1_confidence_pct = 100 * quantile(confidence, 0.25),
    q3_confidence_pct = 100 * quantile(confidence, 0.75),
    minimum_confidence_pct = 100 * min(confidence),
    maximum_confidence_pct = 100 * max(confidence),
    mean_confidence_minus_accuracy_pct =
      mean_confidence_pct - observed_accuracy_pct
  )

high_confidence_data <- ai_data %>% filter(confidence >= 0.99)
high_confidence_ci <- wilson_ci(
  sum(high_confidence_data$correct),
  nrow(high_confidence_data)
)

high_confidence_summary <- high_confidence_data %>%
  summarise(
    confidence_interval = "0.99-1.00",
    confidence_cutoff = 0.99,
    n = n(),
    proportion_of_all_pct = 100 * n() / total_lesions,
    correct_predictions = sum(correct),
    mean_confidence_pct = 100 * mean(confidence),
    median_confidence_pct = 100 * median(confidence),
    observed_accuracy_pct = 100 * mean(correct),
    accuracy_ci_lower_pct = 100 * high_confidence_ci["lower"],
    accuracy_ci_upper_pct = 100 * high_confidence_ci["upper"],
    calibration_gap_pct = mean_confidence_pct - observed_accuracy_pct
  )

calibration_by_interval <- ai_data %>%
  mutate(
    confidence_interval = cut(
      confidence,
      breaks = c(0.50, 0.60, 0.70, 0.80, 0.90, 0.99, 1.000001),
      labels = c(
        "0.50-<0.60", "0.60-<0.70", "0.70-<0.80",
        "0.80-<0.90", "0.90-<0.99", "0.99-1.00"
      ),
      right = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  group_by(confidence_interval, .drop = FALSE) %>%
  summarise(
    n = n(),
    correct_predictions = sum(correct),
    mean_confidence = mean(confidence),
    observed_accuracy = mean(correct),
    .groups = "drop"
  ) %>%
  filter(n > 0) %>%
  mutate(
    proportion_of_all_pct = 100 * n / total_lesions,
    accuracy_ci_lower_pct = map2_dbl(
      correct_predictions,
      n,
      ~ 100 * wilson_ci(.x, .y)["lower"]
    ),
    accuracy_ci_upper_pct = map2_dbl(
      correct_predictions,
      n,
      ~ 100 * wilson_ci(.x, .y)["upper"]
    ),
    mean_confidence_pct = 100 * mean_confidence,
    observed_accuracy_pct = 100 * observed_accuracy,
    calibration_gap_pct = mean_confidence_pct - observed_accuracy_pct
  ) %>%
  select(
    confidence_interval,
    n,
    proportion_of_all_pct,
    correct_predictions,
    mean_confidence_pct,
    observed_accuracy_pct,
    accuracy_ci_lower_pct,
    accuracy_ci_upper_pct,
    calibration_gap_pct
  )

# Supplementary Figure 6: ROC and precision-recall curves

roc_data <- tibble(
  false_positive_rate = 1 - roc_object$specificities,
  true_positive_rate = roc_object$sensitivities
) %>%
  arrange(false_positive_rate, true_positive_rate)

pr_data <- tibble(
  truth = ai_data$gold,
  score = ai_data$p_type2
) %>%
  group_by(score) %>%
  summarise(
    true_positive_added = sum(truth == 2L),
    false_positive_added = sum(truth == 1L),
    .groups = "drop"
  ) %>%
  arrange(desc(score)) %>%
  mutate(
    true_positive = cumsum(true_positive_added),
    false_positive = cumsum(false_positive_added),
    recall = true_positive / sum(ai_data$gold == 2L),
    precision = true_positive / (true_positive + false_positive)
  ) %>%
  select(recall, precision) %>%
  bind_rows(tibble(recall = 0, precision = 1), .)

dark_blue <- "#4C78A8"
reference_grey <- "#808080"
type2_prevalence <- mean(ai_data$gold == 2L)

manuscript_theme <- theme_classic(base_size = 10, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 10, colour = "black"),
    axis.text = element_text(size = 9, colour = "black"),
    axis.line = element_line(linewidth = 0.45, colour = "black"),
    axis.ticks = element_line(linewidth = 0.45, colour = "black"),
    plot.margin = margin(8, 10, 8, 8)
  )

panel_a <- ggplot(
  roc_data,
  aes(x = false_positive_rate, y = true_positive_rate)
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    linewidth = 0.55,
    colour = reference_grey
  ) +
  geom_step(direction = "vh", linewidth = 1, colour = dark_blue) +
  annotate(
    "text",
    x = 0.98,
    y = 0.05,
    hjust = 1,
    vjust = 0,
    size = 3.2,
    family = "Arial",
    label = sprintf(
      "AUC-ROC = %.3f\n95%% CI: %.3f-%.3f",
      roc_auc, roc_ci[1], roc_ci[3]
    )
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0.01))
  ) +
  coord_equal() +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  manuscript_theme

panel_b <- ggplot(pr_data, aes(x = recall, y = precision)) +
  geom_hline(
    yintercept = type2_prevalence,
    linetype = "dashed",
    linewidth = 0.55,
    colour = reference_grey
  ) +
  geom_step(direction = "vh", linewidth = 1, colour = dark_blue) +
  annotate(
    "text",
    x = 0.03,
    y = 0.05,
    hjust = 0,
    vjust = 0,
    size = 3.2,
    family = "Arial",
    label = sprintf(
      "AUC-PR = %.3f\n95%% CI: %.3f-%.3f",
      pr_auc, pr_ci[1], pr_ci[2]
    )
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0.01))
  ) +
  coord_equal() +
  labs(x = "Recall", y = "Precision") +
  manuscript_theme

figure_s6 <- panel_a + panel_b +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(
      family = "Arial",
      face = "bold",
      size = 12,
      colour = "black"
    )
  )

print(confusion_matrix, n = Inf, width = Inf)
print(diagnostic_metrics, n = Inf, width = Inf)
print(auc_metrics, n = Inf, width = Inf)
print(confidence_distribution, n = Inf, width = Inf)
print(high_confidence_summary, n = Inf, width = Inf)
print(calibration_by_interval, n = Inf, width = Inf)
print(figure_s6)

write_xlsx(
  list(
    Confusion_matrix = confusion_matrix,
    Diagnostic_metrics = diagnostic_metrics,
    AUC_metrics = auc_metrics,
    Confidence_distribution = confidence_distribution,
    High_confidence_0.99 = high_confidence_summary,
    Calibration_by_interval = calibration_by_interval
  ),
  file.path(table_dir, "AI_performance_confidence.xlsx")
)

ggsave(
  file.path(figure_dir, "FigureS6.png"),
  figure_s6,
  width = 180,
  height = 85,
  units = "mm",
  dpi = 600,
  bg = "white"
)
ggsave(
  file.path(figure_dir, "FigureS6.tiff"),
  figure_s6,
  width = 180,
  height = 85,
  units = "mm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)
ggsave(
  file.path(figure_dir, "FigureS6.pdf"),
  figure_s6,
  width = 180,
  height = 85,
  units = "mm",
  bg = "white"
)
