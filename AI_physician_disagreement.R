library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(writexl)

input_file <- file.path("data", "rjai_total.xlsx")
output_dir <- file.path("results", "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

raw_data <- read_excel(input_file, sheet = "Sheet1")

physician_columns <- names(raw_data) %>%
  str_subset("^卷[23]_")

decision_data <- raw_data %>%
  select(
    lesion_id = new_file_name,
    gold,
    ai_prediction = AI_diagnosis,
    all_of(physician_columns)
  ) %>%
  pivot_longer(
    cols = all_of(physician_columns),
    names_to = "physician_id",
    values_to = "physician_response"
  ) %>%
  mutate(
    group = case_when(
      str_starts(physician_id, "卷2_") ~ "Group B",
      str_starts(physician_id, "卷3_") ~ "Group C"
    ),
    gold = as.integer(gold),
    ai_prediction = as.integer(ai_prediction),
    physician_prediction = as.integer(
      str_extract(as.character(physician_response), "^[12]")
    ),
    disagreement = physician_prediction != ai_prediction,
    correct_override =
      disagreement &
      ai_prediction != gold &
      physician_prediction == gold,
    incorrect_override =
      disagreement &
      ai_prediction == gold &
      physician_prediction != gold
  )

pooled_summary <- decision_data %>%
  group_by(group) %>%
  summarise(
    physicians = n_distinct(physician_id),
    lesions_per_physician = n_distinct(lesion_id),
    total_decisions = n(),
    disagreement_n = sum(disagreement),
    disagreement_pct = 100 * mean(disagreement),
    correct_override_n = sum(correct_override),
    correct_override_pct_disagreements =
      100 * correct_override_n / disagreement_n,
    incorrect_override_n = sum(incorrect_override),
    incorrect_override_pct_disagreements =
      100 * incorrect_override_n / disagreement_n,
    .groups = "drop"
  )

physician_level <- decision_data %>%
  group_by(group, physician_id) %>%
  summarise(
    total_decisions = n(),
    disagreement_n = sum(disagreement),
    disagreement_pct = 100 * mean(disagreement),
    correct_override_pct_disagreements = if_else(
      disagreement_n > 0,
      100 * sum(correct_override) / disagreement_n,
      NA_real_
    ),
    incorrect_override_pct_disagreements = if_else(
      disagreement_n > 0,
      100 * sum(incorrect_override) / disagreement_n,
      NA_real_
    ),
    .groups = "drop"
  )

median_iqr <- function(x) {
  tibble(
    median = median(x, na.rm = TRUE),
    q1 = quantile(x, 0.25, na.rm = TRUE),
    q3 = quantile(x, 0.75, na.rm = TRUE)
  )
}

physician_summary <- bind_rows(
  physician_level %>%
    group_by(group) %>%
    group_modify(
      ~ median_iqr(.x$disagreement_pct) %>%
        mutate(metric = "Physician-AI disagreement, % of all decisions")
    ),
  physician_level %>%
    group_by(group) %>%
    group_modify(
      ~ median_iqr(.x$correct_override_pct_disagreements) %>%
        mutate(metric = "Correct overrides, % of disagreements")
    ),
  physician_level %>%
    group_by(group) %>%
    group_modify(
      ~ median_iqr(.x$incorrect_override_pct_disagreements) %>%
        mutate(metric = "Incorrect overrides, % of disagreements")
    )
) %>%
  ungroup() %>%
  select(group, metric, median, q1, q3)

print(pooled_summary, n = Inf, width = Inf)
print(physician_summary, n = Inf, width = Inf)

write_xlsx(
  list(
    Pooled_summary = pooled_summary,
    Physician_summary = physician_summary
  ),
  file.path(output_dir, "Physician_AI_disagreement.xlsx")
)
