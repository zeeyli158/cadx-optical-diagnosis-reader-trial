library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)
library(writexl)

reader_file <- file.path("data", "reader responses.xlsx")
polyp_file <- file.path("data", "rjai_total.xlsx")
followup_file <- file.path("results", "doctor_usfollow.xlsx")
output_dir <- file.path("results", "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

group_levels <- c("Group A", "Group B", "Group C")
experience_levels <- c("<=5 years", ">5 years")

normalise_text <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("＜", "<") %>%
    str_replace_all("＞", ">") %>%
    str_trim()
}

strip_option_letter <- function(x) {
  x <- normalise_text(x)
  if_else(
    !is.na(x) & str_detect(x, "^[A-Z]\\s+"),
    str_replace(x, "^[A-Z]\\s+", ""),
    x
  )
}

normalise_interval <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("—|–|至", "-")
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

format_p <- function(p) {
  if_else(
    is.na(p),
    "",
    if_else(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))
  )
}

format_n_pct_ci <- function(events, total, lower, upper) {
  sprintf(
    "%d/%d (%.1f%%; 95%% CI %.1f-%.1f)",
    events, total, 100 * events / total, 100 * lower, 100 * upper
  )
}

format_median_iqr <- function(x) {
  sprintf(
    "%.1f%% (%.1f-%.1f)",
    median(x, na.rm = TRUE),
    quantile(x, 0.25, na.rm = TRUE),
    quantile(x, 0.75, na.rm = TRUE)
  )
}

physician_characteristics <- read_excel(reader_file, sheet = "Sheet1") %>%
  distinct(ID, .keep_all = TRUE) %>%
  transmute(
    physician_id = as.character(as.integer(ID)),
    nbi_response = strip_option_letter(Years_of_experience_with_NBI),
    nbi_experience = case_when(
      nbi_response == ">5 years" ~ ">5 years",
      nbi_response %in% c(
        "None", "<1 year", "<1 years", "1-3 years", "3-5 years"
      ) ~ "<=5 years",
      TRUE ~ NA_character_
    ),
    nbi_experience = factor(nbi_experience, levels = experience_levels)
  )

doctor_interval_long <- read_excel(
  followup_file,
  sheet = "doctor_interval_long"
)

physician_pass <- doctor_interval_long %>%
  mutate(
    paper = str_replace_all(as.character(paper), "\\s+", ""),
    group = recode(
      paper,
      "卷1" = "Group A",
      "卷2" = "Group B",
      "卷3" = "Group C"
    ),
    group = factor(group, levels = group_levels),
    predicted_interval = normalise_interval(us_interval),
    reference_interval = normalise_interval(us_gold_recalc),
    interval_correct = predicted_interval == reference_interval
  ) %>%
  filter(
    !is.na(patient_id),
    !is.na(doctor_col),
    !is.na(group),
    !is.na(predicted_interval),
    predicted_interval != "",
    !is.na(reference_interval),
    reference_interval != "",
    reference_interval != "review"
  ) %>%
  group_by(group, doctor_col) %>%
  summarise(
    patient_count = n(),
    concordant_patients = sum(interval_correct),
    agreement_pct = 100 * mean(interval_correct),
    passed = as.integer(agreement_pct >= 90),
    .groups = "drop"
  ) %>%
  mutate(
    physician_id = str_extract(doctor_col, "\\d+$"),
    physician_id = as.character(as.integer(physician_id))
  ) %>%
  left_join(physician_characteristics, by = "physician_id") %>%
  filter(!is.na(nbi_experience))

pass_summary <- physician_pass %>%
  group_by(nbi_experience, group) %>%
  summarise(
    physicians = n(),
    events = sum(passed),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    interval = list(wilson_ci(events, physicians)),
    lower = interval[[1]],
    upper = interval[[2]],
    display = format_n_pct_ci(events, physicians, lower, upper)
  ) %>%
  ungroup()

pass_within_stratum <- physician_pass %>%
  group_split(nbi_experience) %>%
  map_dfr(function(data) {
    contingency_table <- table(data$group, data$passed)
    chi_square <- suppressWarnings(
      chisq.test(contingency_table, correct = FALSE)
    )
    p_value <- if (any(chi_square$expected < 5)) {
      fisher.test(contingency_table)$p.value
    } else {
      chi_square$p.value
    }

    tibble(
      nbi_experience = first(data$nbi_experience),
      p_value,
      p_display = format_p(p_value)
    )
  })

pass_model_main <- glm(
  passed ~ group + nbi_experience,
  family = binomial,
  data = physician_pass
)
pass_model_interaction <- glm(
  passed ~ group * nbi_experience,
  family = binomial,
  data = physician_pass
)
pass_lrt <- anova(pass_model_main, pass_model_interaction, test = "LRT")
pass_interaction_p <- pass_lrt$`Pr(>Chi)`[2]

risk_difference <- function(data, treatment_group) {
  counts <- data %>%
    filter(group %in% c("Group A", treatment_group)) %>%
    group_by(group) %>%
    summarise(
      events = sum(passed),
      total = n(),
      risk = mean(passed),
      .groups = "drop"
    )

  treatment <- counts %>% filter(group == treatment_group)
  control <- counts %>% filter(group == "Group A")
  test_result <- prop.test(
    c(treatment$events, control$events),
    c(treatment$total, control$total),
    correct = FALSE
  )

  tibble(
    comparison = paste(treatment_group, "vs Group A"),
    treatment_events = treatment$events,
    treatment_total = treatment$total,
    control_events = control$events,
    control_total = control$total,
    risk_difference_pct = 100 * (treatment$risk - control$risk),
    ci_lower_pct = 100 * test_result$conf.int[1],
    ci_upper_pct = 100 * test_result$conf.int[2],
    p_value = test_result$p.value
  )
}

pass_effects <- physician_pass %>%
  group_split(nbi_experience) %>%
  map_dfr(function(data) {
    bind_rows(
      risk_difference(data, "Group B"),
      risk_difference(data, "Group C")
    ) %>%
      mutate(nbi_experience = first(data$nbi_experience), .before = 1)
  }) %>%
  mutate(
    p_adjusted_holm = p.adjust(p_value, method = "holm"),
    `Risk difference (95% CI)` = sprintf(
      "%.1f percentage points (95%% CI %.1f to %.1f)",
      risk_difference_pct, ci_lower_pct, ci_upper_pct
    ),
    `P value` = format_p(p_value),
    `Holm-adjusted P value` = format_p(p_adjusted_holm)
  )

polyp_data <- read_excel(polyp_file, sheet = "Sheet1")
physician_columns <- names(polyp_data) %>% str_subset("^卷[123]_")

response_long <- polyp_data %>%
  select(
    lesion_id = new_file_name,
    AI_diagnosis,
    gold,
    all_of(physician_columns)
  ) %>%
  pivot_longer(
    cols = all_of(physician_columns),
    names_to = "physician_column",
    values_to = "response"
  ) %>%
  mutate(
    group = case_when(
      str_starts(physician_column, "卷1_") ~ "Group A",
      str_starts(physician_column, "卷2_") ~ "Group B",
      str_starts(physician_column, "卷3_") ~ "Group C"
    ),
    group = factor(group, levels = group_levels),
    physician_id = str_extract(physician_column, "\\d+$"),
    physician_id = as.character(as.integer(physician_id)),
    physician_prediction = as.integer(
      str_extract(as.character(response), "^[12]")
    ),
    ai_prediction = as.integer(AI_diagnosis),
    reference = as.integer(gold),
    physician_correct = physician_prediction == reference,
    ai_correct = ai_prediction == reference
  ) %>%
  filter(physician_prediction %in% c(1L, 2L)) %>%
  left_join(physician_characteristics, by = "physician_id") %>%
  filter(!is.na(nbi_experience))

physician_accuracy <- response_long %>%
  group_by(group, physician_column, physician_id, nbi_experience) %>%
  summarise(
    lesions = n(),
    correct = sum(physician_correct),
    accuracy_pct = 100 * mean(physician_correct),
    .groups = "drop"
  )

accuracy_summary <- physician_accuracy %>%
  group_by(nbi_experience, group) %>%
  summarise(
    physicians = n(),
    display = format_median_iqr(accuracy_pct),
    .groups = "drop"
  )

accuracy_within_stratum <- physician_accuracy %>%
  group_split(nbi_experience) %>%
  map_dfr(function(data) {
    test_result <- kruskal.test(accuracy_pct ~ group, data = data)
    tibble(
      nbi_experience = first(data$nbi_experience),
      p_value = test_result$p.value,
      p_display = format_p(p_value)
    )
  })

accuracy_model_main <- glm(
  cbind(correct, lesions - correct) ~ group + nbi_experience,
  family = quasibinomial,
  data = physician_accuracy
)
accuracy_model_interaction <- glm(
  cbind(correct, lesions - correct) ~ group * nbi_experience,
  family = quasibinomial,
  data = physician_accuracy
)
accuracy_f_test <- anova(
  accuracy_model_main,
  accuracy_model_interaction,
  test = "F"
)
accuracy_interaction_p <- accuracy_f_test$`Pr(>F)`[2]

pass_table <- pass_summary %>%
  select(nbi_experience, group, display) %>%
  pivot_wider(names_from = group, values_from = display) %>%
  left_join(
    pass_within_stratum %>% select(nbi_experience, p_display),
    by = "nbi_experience"
  ) %>%
  mutate(
    Outcome = "Surveillance interval pass rate, n/N (%; 95% CI)",
    `P for interaction` = if_else(
      row_number() == 1,
      format_p(pass_interaction_p),
      ""
    )
  )

accuracy_table <- accuracy_summary %>%
  select(nbi_experience, group, display) %>%
  pivot_wider(names_from = group, values_from = display) %>%
  left_join(
    accuracy_within_stratum %>% select(nbi_experience, p_display),
    by = "nbi_experience"
  ) %>%
  mutate(
    Outcome = "Diagnostic accuracy, median % (IQR)",
    `P for interaction` = if_else(
      row_number() == 1,
      format_p(accuracy_interaction_p),
      ""
    )
  )

main_table <- bind_rows(pass_table, accuracy_table) %>%
  rename(
    `NBI experience` = nbi_experience,
    `Within-stratum P value` = p_display
  ) %>%
  select(
    Outcome, `NBI experience`, `Group A`, `Group B`, `Group C`,
    `Within-stratum P value`, `P for interaction`
  )

interaction_tests <- tibble(
  Outcome = c(
    "Surveillance interval pass rate",
    "Physician-level diagnostic accuracy"
  ),
  Model = c("Logistic regression", "Quasibinomial regression"),
  df = c(pass_lrt$Df[2], accuracy_f_test$Df[2]),
  Statistic = c(pass_lrt$Deviance[2], accuracy_f_test$F[2]),
  `P value` = c(pass_interaction_p, accuracy_interaction_p)
)

incorrect_ai_following <- response_long %>%
  filter(group %in% c("Group B", "Group C"), !ai_correct) %>%
  mutate(followed_incorrect_ai = physician_prediction == ai_prediction) %>%
  group_by(group, physician_column, physician_id, nbi_experience) %>%
  summarise(
    followed_pct = 100 * mean(followed_incorrect_ai),
    .groups = "drop"
  )

incorrect_ai_summary <- bind_rows(
  incorrect_ai_following %>% mutate(display_group = as.character(group)),
  incorrect_ai_following %>% mutate(display_group = "Groups B+C combined")
) %>%
  group_by(display_group, nbi_experience) %>%
  summarise(
    physicians = n(),
    display = format_median_iqr(followed_pct),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = nbi_experience,
    values_from = c(physicians, display)
  )

incorrect_ai_tests <- c("Group B", "Group C", "Groups B+C combined") %>%
  map_dfr(function(group_to_test) {
    data <- if (group_to_test == "Groups B+C combined") {
      incorrect_ai_following
    } else {
      incorrect_ai_following %>%
        filter(as.character(group) == group_to_test)
    }

    test_result <- wilcox.test(
      followed_pct ~ nbi_experience,
      data = data,
      exact = FALSE
    )

    tibble(display_group = group_to_test, p_value = test_result$p.value)
  }) %>%
  mutate(
    p_adjusted_holm = p.adjust(p_value, method = "holm"),
    `P value` = format_p(p_value),
    `Holm-adjusted P value` = format_p(p_adjusted_holm)
  )

incorrect_ai_table <- incorrect_ai_summary %>%
  left_join(incorrect_ai_tests, by = "display_group") %>%
  transmute(
    Group = display_group,
    `Physicians with <=5 years, n` = `physicians_<=5 years`,
    `Physicians with >5 years, n` = `physicians_>5 years`,
    `Incorrect AI following, <=5 years, median % (IQR)` =
      `display_<=5 years`,
    `Incorrect AI following, >5 years, median % (IQR)` =
      `display_>5 years`,
    `P value`,
    `Holm-adjusted P value`
  )

pass_effects_export <- pass_effects %>%
  transmute(
    `NBI experience` = nbi_experience,
    Comparison = comparison,
    `Risk difference (95% CI)`,
    `P value`,
    `Holm-adjusted P value`
  )

print(main_table, n = Inf, width = Inf)
print(interaction_tests, n = Inf, width = Inf)
print(pass_effects_export, n = Inf, width = Inf)
print(incorrect_ai_table, n = Inf, width = Inf)

write_xlsx(
  list(
    Table_S5 = main_table,
    Interaction_tests = interaction_tests,
    Pass_effects = pass_effects_export,
    Incorrect_AI_following = incorrect_ai_table
  ),
  file.path(output_dir, "TableS5_experience_subgroup.xlsx")
)
