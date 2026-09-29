library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)

polyp_file <- file.path("data", "rjai_total.xlsx")
reader_file <- file.path("data", "reader responses.xlsx")
results_dir <- "results"
table_dir <- file.path(results_dir, "tables")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

raw_data <- read_excel(polyp_file, sheet = "Sheet1")
reader_data <- read_excel(reader_file, sheet = "Sheet1")

physician_columns <- names(raw_data) %>%
  str_subset("^卷\\s*[123]_")

group_levels <- c("卷1", "卷2", "卷3")

normalise_histology <- function(x) {
  case_when(
    x %in% c("adenoma", "Tubular adenoma") ~ "adenoma",
    x %in% c("HP", "Hyperplastic polyp") ~ "HP",
    x %in% c("SSP", "Sessile serrated lesion") ~ "SSP",
    x %in% c("adenoma_villous", "Tubulovillous adenoma") ~ "adenoma_villous",
    x %in% c("other", "Other") ~ "other",
    TRUE ~ "review"
  )
}

normalise_interval <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("—|–|至", "-")
}

assign_usmstf_interval <- function(
    n_adenoma_lt10, n_adenoma_ge10, n_adenoma_villous,
    n_ssp_lt10, n_ssp_ge10, n_hp_lt10, n_hp_ge10) {

  total_adenoma <- n_adenoma_lt10 + n_adenoma_ge10 + n_adenoma_villous
  candidates <- character()

  if (total_adenoma > 10) {
    candidates <- c(candidates, "1年")
  }
  if (n_adenoma_ge10 > 0 || n_adenoma_villous > 0 || n_ssp_ge10 > 0 ||
      between(n_adenoma_lt10, 5, 10) || between(n_ssp_lt10, 5, 10)) {
    candidates <- c(candidates, "3年")
  }
  if (between(n_adenoma_lt10, 3, 4) || between(n_ssp_lt10, 3, 4) ||
      n_hp_ge10 > 0) {
    candidates <- c(candidates, "3–5年")
  }
  if (between(n_ssp_lt10, 1, 2)) {
    candidates <- c(candidates, "5–10年")
  }
  if (between(n_adenoma_lt10, 1, 2)) {
    candidates <- c(candidates, "7–10年")
  }
  if (length(candidates) == 0) {
    return(ifelse(n_hp_lt10 <= 20, "10年", "review"))
  }

  interval_order <- c("10年", "7–10年", "5–10年", "3–5年", "3年", "1年")
  candidates[which.max(match(candidates, interval_order))]
}

wilson_ci <- function(events, total, conf_level = 0.95) {
  if (total == 0) {
    return(c(NA_real_, NA_real_))
  }

  z <- qnorm(1 - (1 - conf_level) / 2)
  proportion <- events / total
  denominator <- 1 + z^2 / total
  centre <- (proportion + z^2 / (2 * total)) / denominator
  half_width <- z * sqrt(
    proportion * (1 - proportion) / total + z^2 / (4 * total^2)
  ) / denominator

  c(max(0, centre - half_width), min(1, centre + half_width))
}

format_p <- function(p) {
  ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 2))
}

format_rate_ci <- function(estimate, lower, upper) {
  sprintf("%.1f%% (%.1f%%–%.1f%%)", estimate, lower, upper)
}

format_median_iqr <- function(median_value, q1, q3, percent_sign = TRUE) {
  if (percent_sign) {
    sprintf("%.1f%% (%.1f%%–%.1f%%)", median_value, q1, q3)
  } else {
    sprintf("%.1f (%.1f–%.1f)", median_value, q1, q3)
  }
}

lesion_data <- raw_data %>%
  transmute(
    patient_id = as.character(ID),
    lesion_id = as.character(new_file_name),
    histology_gold = normalise_histology(as.character(Histology)),
    size_mm = as.numeric(Size),
    location = as.character(Location),
    lesion_gold = as.integer(gold),
    across(all_of(physician_columns))
  )

reference_interval <- lesion_data %>%
  group_by(patient_id) %>%
  summarise(
    n_adenoma_lt10 = sum(histology_gold == "adenoma" & size_mm < 10),
    n_adenoma_ge10 = sum(histology_gold == "adenoma" & size_mm >= 10),
    n_adenoma_villous = sum(histology_gold == "adenoma_villous"),
    n_ssp_lt10 = sum(histology_gold == "SSP" & size_mm < 10),
    n_ssp_ge10 = sum(histology_gold == "SSP" & size_mm >= 10),
    n_hp_lt10 = sum(histology_gold == "HP" & size_mm < 10),
    n_hp_ge10 = sum(histology_gold == "HP" & size_mm >= 10),
    .groups = "drop"
  ) %>%
  mutate(
    us_gold_recalc = pmap_chr(
      list(
        n_adenoma_lt10, n_adenoma_ge10, n_adenoma_villous,
        n_ssp_lt10, n_ssp_ge10, n_hp_lt10, n_hp_ge10
      ),
      assign_usmstf_interval
    )
  )

lesion_final <- lesion_data %>%
  pivot_longer(
    cols = all_of(physician_columns),
    names_to = "doctor_col",
    values_to = "response"
  ) %>%
  mutate(
    answer = as.integer(str_extract(as.character(response), "^[12]")),
    confidence = case_when(
      str_detect(str_to_lower(as.character(response)), "high|hi") ~ "high",
      str_detect(as.character(response), "高") ~ "high",
      TRUE ~ "low"
    ),
    paper = str_extract(doctor_col, "^卷\\s*[123]") %>%
      str_replace_all("\\s+", "") %>%
      factor(levels = group_levels),
    doctor_id = str_remove(doctor_col, "^卷\\s*[123]_"),
    assigned_histology = case_when(
      size_mm <= 5 & confidence == "high" & answer == 1 ~ "HP",
      size_mm <= 5 & confidence == "high" & answer == 2 ~ "adenoma",
      TRUE ~ histology_gold
    ),
    classification_source = if_else(
      size_mm <= 5 & confidence == "high",
      "optical diagnosis",
      "histopathology"
    )
  )

doctor_interval_long <- lesion_final %>%
  group_by(patient_id, doctor_col, doctor_id, paper) %>%
  summarise(
    n_adenoma_lt10 = sum(assigned_histology == "adenoma" & size_mm < 10),
    n_adenoma_ge10 = sum(assigned_histology == "adenoma" & size_mm >= 10),
    n_adenoma_villous = sum(assigned_histology == "adenoma_villous"),
    n_ssp_lt10 = sum(assigned_histology == "SSP" & size_mm < 10),
    n_ssp_ge10 = sum(assigned_histology == "SSP" & size_mm >= 10),
    n_hp_lt10 = sum(assigned_histology == "HP" & size_mm < 10),
    n_hp_ge10 = sum(assigned_histology == "HP" & size_mm >= 10),
    n_lesions = n(),
    .groups = "drop"
  ) %>%
  mutate(
    us_interval = pmap_chr(
      list(
        n_adenoma_lt10, n_adenoma_ge10, n_adenoma_villous,
        n_ssp_lt10, n_ssp_ge10, n_hp_lt10, n_hp_ge10
      ),
      assign_usmstf_interval
    )
  ) %>%
  left_join(
    reference_interval %>% select(patient_id, us_gold_recalc),
    by = "patient_id"
  )

out_patient <- doctor_interval_long %>%
  select(patient_id, doctor_col, us_interval) %>%
  pivot_wider(names_from = doctor_col, values_from = us_interval) %>%
  left_join(
    reference_interval %>% select(patient_id, us_gold_recalc),
    by = "patient_id"
  ) %>%
  arrange(patient_id)

agreement_long <- doctor_interval_long %>%
  mutate(
    predicted_interval = normalise_interval(us_interval),
    reference_interval = normalise_interval(us_gold_recalc),
    concordant = as.integer(predicted_interval == reference_interval)
  )

doctor_agreement <- agreement_long %>%
  group_by(paper, doctor_col, doctor_id) %>%
  summarise(
    n_patients = n(),
    concordant_patients = sum(concordant),
    agreement_pct = 100 * concordant_patients / n_patients,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(wilson_ci(concordant_patients, n_patients)),
    agreement_ci_lower_pct = 100 * ci[[1]],
    agreement_ci_upper_pct = 100 * ci[[2]],
    pass01 = as.integer(agreement_pct >= 90)
  ) %>%
  ungroup() %>%
  select(-ci)

agreement_summary <- doctor_agreement %>%
  group_by(paper) %>%
  summarise(
    median_pct = median(agreement_pct),
    q1_pct = quantile(agreement_pct, 0.25),
    q3_pct = quantile(agreement_pct, 0.75),
    display = format_median_iqr(median_pct, q1_pct, q3_pct),
    .groups = "drop"
  )

agreement_p <- kruskal.test(agreement_pct ~ paper, data = doctor_agreement)$p.value

pass_summary <- doctor_agreement %>%
  group_by(paper) %>%
  summarise(
    total = n(),
    passed = sum(pass01),
    estimate_pct = 100 * passed / total,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(wilson_ci(passed, total)),
    lower_pct = 100 * ci[[1]],
    upper_pct = 100 * ci[[2]],
    display = format_rate_ci(estimate_pct, lower_pct, upper_pct)
  ) %>%
  ungroup() %>%
  select(-ci)

pass_matrix <- doctor_agreement %>%
  count(paper, pass01) %>%
  complete(paper, pass01 = 0:1, fill = list(n = 0)) %>%
  pivot_wider(names_from = pass01, values_from = n, names_prefix = "pass_") %>%
  select(pass_0, pass_1) %>%
  as.matrix()

pass_p <- chisq.test(pass_matrix, correct = FALSE)$p.value

npv_by_physician <- lesion_final %>%
  filter(
    location %in% c("Rectum", "Sigmoid colon"),
    size_mm <= 5,
    confidence == "high",
    lesion_gold %in% c(1, 2),
    answer %in% c(1, 2)
  ) %>%
  group_by(paper, doctor_col, doctor_id) %>%
  summarise(
    predicted_non_neoplastic = sum(answer == 1),
    true_negative = sum(answer == 1 & lesion_gold == 1),
    false_negative = sum(answer == 1 & lesion_gold == 2),
    npv_pct = if_else(
      predicted_non_neoplastic > 0,
      100 * true_negative / predicted_non_neoplastic,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(wilson_ci(true_negative, predicted_non_neoplastic)),
    npv_ci_lower_pct = 100 * ci[[1]],
    npv_ci_upper_pct = 100 * ci[[2]],
    pass01 = if_else(!is.na(npv_pct) & npv_pct >= 90, 1L, 0L)
  ) %>%
  ungroup() %>%
  select(-ci)

npv_summary <- npv_by_physician %>%
  filter(!is.na(npv_pct)) %>%
  group_by(paper) %>%
  summarise(
    median_pct = median(npv_pct),
    q1_pct = quantile(npv_pct, 0.25),
    q3_pct = quantile(npv_pct, 0.75),
    display = format_median_iqr(median_pct, q1_pct, q3_pct),
    .groups = "drop"
  )

npv_p <- kruskal.test(
  npv_pct ~ paper,
  data = npv_by_physician %>% filter(!is.na(npv_pct))
)$p.value

npv_pass_summary <- npv_by_physician %>%
  filter(!is.na(npv_pct)) %>%
  group_by(paper) %>%
  summarise(
    total = n(),
    passed = sum(pass01),
    estimate_pct = 100 * passed / total,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(wilson_ci(passed, total)),
    lower_pct = 100 * ci[[1]],
    upper_pct = 100 * ci[[2]],
    display = format_rate_ci(estimate_pct, lower_pct, upper_pct)
  ) %>%
  ungroup() %>%
  select(-ci)

npv_pass_matrix <- npv_by_physician %>%
  filter(!is.na(npv_pct)) %>%
  count(paper, pass01) %>%
  complete(paper, pass01 = 0:1, fill = list(n = 0)) %>%
  pivot_wider(names_from = pass01, values_from = n, names_prefix = "pass_") %>%
  select(pass_0, pass_1) %>%
  as.matrix()

npv_pass_p <- chisq.test(npv_pass_matrix, correct = FALSE)$p.value

make_table_row <- function(summary_data, outcome, p_value) {
  summary_data %>%
    select(paper, display) %>%
    pivot_wider(names_from = paper, values_from = display) %>%
    transmute(
      Outcome = outcome,
      `Group A` = 卷1,
      `Group B` = 卷2,
      `Group C` = 卷3,
      `Overall P value` = format_p(p_value)
    )
}

section_row <- function(label) {
  tibble(
    Outcome = label,
    `Group A` = "",
    `Group B` = "",
    `Group C` = "",
    `Overall P value` = ""
  )
}

table2 <- bind_rows(
  section_row("Resect-and-discard (USMSTF)"),
  make_table_row(
    pass_summary,
    "  Pass rate (≥90% surveillance interval agreement), % (95% CI)",
    pass_p
  ),
  make_table_row(
    agreement_summary,
    "  Surveillance interval agreement, median % (IQR)",
    agreement_p
  ),
  section_row("Leave-in-situ"),
  make_table_row(
    npv_pass_summary,
    "  Pass rate (NPV ≥90%), % (95% CI)",
    npv_pass_p
  ),
  make_table_row(
    npv_summary,
    "  NPV, median % (IQR)",
    npv_p
  )
)

agreement_pairwise <- combn(group_levels, 2, simplify = FALSE) %>%
  map_dfr(function(groups) {
    subset_data <- doctor_agreement %>%
      filter(paper %in% groups) %>%
      mutate(paper = droplevels(paper))
    test <- wilcox.test(agreement_pct ~ paper, data = subset_data, exact = FALSE)

    tibble(
      comparison = paste(groups, collapse = " vs "),
      p_value = test$p.value
    )
  }) %>%
  mutate(
    p_adjusted_holm = p.adjust(p_value, method = "holm")
  )

sensitivity_long <- agreement_long %>%
  mutate(
    discordant_7_10_vs_10 =
      (predicted_interval == "7-10年" & reference_interval == "10年") |
      (predicted_interval == "10年" & reference_interval == "7-10年"),
    predicted_sensitivity = if_else(
      predicted_interval %in% c("7-10年", "10年"),
      "7-10/10年",
      predicted_interval
    ),
    reference_sensitivity = if_else(
      reference_interval %in% c("7-10年", "10年"),
      "7-10/10年",
      reference_interval
    ),
    concordant_sensitivity = as.integer(
      predicted_sensitivity == reference_sensitivity
    )
  )

discordance_summary <- sensitivity_long %>%
  group_by(paper) %>%
  summarise(
    total_assignments = n(),
    discordant_primary = sum(predicted_interval != reference_interval),
    discordant_7_10_vs_10 = sum(discordant_7_10_vs_10),
    discordant_after_combination = sum(
      predicted_sensitivity != reference_sensitivity
    ),
    discordance_reduction_pct =
      100 * discordant_7_10_vs_10 / discordant_primary,
    .groups = "drop"
  ) %>%
  mutate(
    paper = recode(as.character(paper),
                   "卷1" = "Group A", "卷2" = "Group B", "卷3" = "Group C")
  )

doctor_agreement_sensitivity <- sensitivity_long %>%
  group_by(paper, doctor_col, doctor_id) %>%
  summarise(
    n_patients = n(),
    concordant_patients = sum(concordant_sensitivity),
    agreement_pct = 100 * concordant_patients / n_patients,
    pass01 = as.integer(agreement_pct >= 90),
    .groups = "drop"
  )

sensitivity_agreement_summary <- doctor_agreement_sensitivity %>%
  group_by(paper) %>%
  summarise(
    median_pct = median(agreement_pct),
    q1_pct = quantile(agreement_pct, 0.25),
    q3_pct = quantile(agreement_pct, 0.75),
    display = format_median_iqr(
      median_pct, q1_pct, q3_pct, percent_sign = FALSE
    ),
    .groups = "drop"
  )

sensitivity_agreement_p <- kruskal.test(
  agreement_pct ~ paper,
  data = doctor_agreement_sensitivity
)$p.value

sensitivity_pass_summary <- doctor_agreement_sensitivity %>%
  group_by(paper) %>%
  summarise(
    total = n(),
    passed = sum(pass01),
    estimate_pct = 100 * passed / total,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(wilson_ci(passed, total)),
    lower_pct = 100 * ci[[1]],
    upper_pct = 100 * ci[[2]],
    display = format_rate_ci(estimate_pct, lower_pct, upper_pct)
  ) %>%
  ungroup() %>%
  select(-ci)

table_s2 <- bind_rows(
  make_table_row(
    sensitivity_pass_summary,
    "Pass rate (≥90% surveillance interval agreement), % (95% CI)",
    NA_real_
  ) %>%
    mutate(`Overall P value` = "—"),
  make_table_row(
    sensitivity_agreement_summary,
    "Surveillance interval agreement, median % (IQR)",
    sensitivity_agreement_p
  )
)

doctor_basic <- reader_data %>%
  distinct(ID, .keep_all = TRUE) %>%
  transmute(
    doctor_id = as.character(as.integer(ID)),
    Group, Sex, Age, Province,
    Years_of_experience_in_digestive_endoscopy,
    Professional_title,
    Type_of_practice_institution,
    Hospital_level,
    Exclusive_endoscopy_practice,
    Total_number_of_colonosopies_performed,
    Years_of_experience_in_lower_gastrointestinal_endoscopy,
    Years_of_experience_with_NBI
  )

pass_rate_with_basic <- doctor_agreement %>%
  left_join(doctor_basic, by = "doctor_id")

npv_with_basic <- npv_by_physician %>%
  left_join(doctor_basic, by = "doctor_id")

overall_tests <- tibble(
  outcome = c(
    "Surveillance interval pass rate",
    "Surveillance interval agreement",
    "Rectosigmoid NPV pass rate",
    "Rectosigmoid NPV",
    "Sensitivity-analysis surveillance interval agreement"
  ),
  test = c(
    "Chi-square", "Kruskal-Wallis", "Chi-square",
    "Kruskal-Wallis", "Kruskal-Wallis"
  ),
  p_value = c(pass_p, agreement_p, npv_pass_p, npv_p, sensitivity_agreement_p)
)

print(table2, n = Inf)
print(table_s2, n = Inf)
print(discordance_summary, n = Inf)

write_xlsx(
  list(
    doctor_interval_long = doctor_interval_long,
    out_patient = out_patient,
    lesion_final = lesion_final
  ),
  file.path(results_dir, "doctor_usfollow.xlsx")
)

write_xlsx(
  list(Table_2 = table2),
  file.path(table_dir, "Table2.xlsx")
)

write_xlsx(
  list(
    Table_S2 = table_s2,
    Discordance_counts = discordance_summary
  ),
  file.path(table_dir, "TableS2_sensitivity_analysis.xlsx")
)

write_xlsx(
  list(pass_rate_with_basic = pass_rate_with_basic),
  file.path(results_dir, "pass_rate_with_basic.xlsx")
)

write_xlsx(
  list(npv_with_basic = npv_with_basic),
  file.path(results_dir, "npv_with_basic.xlsx")
)

write_xlsx(
  list(
    Overall_tests = overall_tests,
    Agreement_pairwise_Holm = agreement_pairwise
  ),
  file.path(table_dir, "Table2_tests.xlsx")
)
