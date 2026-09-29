library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(writexl)

input_file <- file.path("data", "rjai_total.xlsx")
output_dir <- file.path("results", "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

normalize_histology <- function(x) {
  x <- str_trim(as.character(x))

  case_when(
    x %in% c("adenoma", "Tubular adenoma") ~ "adenoma",
    x %in% c("HP", "Hyperplastic polyp") ~ "HP",
    x %in% c("SSP", "Sessile serrated lesion") ~ "SSP",
    x %in% c("adenoma_villous", "Tubulovillous adenoma") ~
      "adenoma_villous",
    x %in% c("other", "Other") ~ "other",
    TRUE ~ "review"
  )
}

lesion_data <- read_excel(input_file, sheet = "Sheet1") %>%
  transmute(
    lesion_id = as.character(new_file_name),
    patient_id = as.character(姓名),
    reference_histology = normalize_histology(Histology),
    size_mm = as.numeric(Size),
    ai_prediction = as.integer(AI_diagnosis),
    confidence = as.numeric(CI),
    p_type2 = if_else(
      ai_prediction == 2L,
      confidence,
      1 - confidence
    )
  )

assign_usmstf_interval <- function(
    adenoma_lt10,
    adenoma_ge10,
    adenoma_villous,
    ssp_lt10,
    ssp_ge10,
    hp_lt10,
    hp_ge10
) {
  total_adenomas <- adenoma_lt10 + adenoma_ge10 + adenoma_villous
  candidates <- character()

  if (total_adenomas > 10) candidates <- c(candidates, "1 year")

  if (
    adenoma_ge10 > 0 || adenoma_villous > 0 || ssp_ge10 > 0 ||
      between(adenoma_lt10, 5, 10) || between(ssp_lt10, 5, 10)
  ) {
    candidates <- c(candidates, "3 years")
  }

  if (
    between(adenoma_lt10, 3, 4) || between(ssp_lt10, 3, 4) ||
      hp_ge10 > 0
  ) {
    candidates <- c(candidates, "3-5 years")
  }

  if (between(ssp_lt10, 1, 2)) {
    candidates <- c(candidates, "5-10 years")
  }

  if (between(adenoma_lt10, 1, 2)) {
    candidates <- c(candidates, "7-10 years")
  }

  if (length(candidates) == 0) {
    return(if_else(hp_lt10 <= 20, "10 years", "review"))
  }

  interval_order <- c(
    "10 years", "7-10 years", "5-10 years",
    "3-5 years", "3 years", "1 year"
  )
  candidates[which.max(match(candidates, interval_order))]
}

reconstruct_intervals <- function(data, diagnosis_column, output_column) {
  data %>%
    mutate(diagnosis = .data[[diagnosis_column]]) %>%
    group_by(patient_id) %>%
    summarise(
      adenoma_lt10 = sum(diagnosis == "adenoma" & size_mm < 10),
      adenoma_ge10 = sum(diagnosis == "adenoma" & size_mm >= 10),
      adenoma_villous = sum(diagnosis == "adenoma_villous"),
      ssp_lt10 = sum(diagnosis == "SSP" & size_mm < 10),
      ssp_ge10 = sum(diagnosis == "SSP" & size_mm >= 10),
      hp_lt10 = sum(diagnosis == "HP" & size_mm < 10),
      hp_ge10 = sum(diagnosis == "HP" & size_mm >= 10),
      .groups = "drop"
    ) %>%
    mutate(
      interval = pmap_chr(
        list(
          adenoma_lt10, adenoma_ge10, adenoma_villous,
          ssp_lt10, ssp_ge10, hp_lt10, hp_ge10
        ),
        assign_usmstf_interval
      )
    ) %>%
    rename(!!output_column := interval)
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

reference_intervals <- reconstruct_intervals(
  lesion_data,
  "reference_histology",
  "reference_interval"
)

summarize_agreement <- function(patient_results) {
  concordant_patients <- sum(patient_results$concordant)
  total_patients <- nrow(patient_results)
  interval <- wilson_ci(concordant_patients, total_patients)

  tibble(
    concordant_patients,
    total_patients,
    agreement_pct = 100 * concordant_patients / total_patients,
    ci_lower_pct = 100 * interval["lower"],
    ci_upper_pct = 100 * interval["upper"],
    reached_90_percent = agreement_pct >= 90
  )
}

# Analysis 1: change the P(type 2) decision threshold and apply AI to
# every eligible polyp measuring <=5 mm.

evaluate_decision_threshold <- function(threshold) {
  classified_lesions <- lesion_data %>%
    mutate(
      threshold_prediction = if_else(p_type2 >= threshold, 2L, 1L),
      surveillance_histology = case_when(
        size_mm <= 5 & threshold_prediction == 1L ~ "HP",
        size_mm <= 5 & threshold_prediction == 2L ~ "adenoma",
        TRUE ~ reference_histology
      )
    )

  ai_intervals <- reconstruct_intervals(
    classified_lesions,
    "surveillance_histology",
    "ai_interval"
  )

  patient_results <- reference_intervals %>%
    select(patient_id, reference_interval) %>%
    left_join(
      ai_intervals %>% select(patient_id, ai_interval),
      by = "patient_id"
    ) %>%
    filter(reference_interval != "review", ai_interval != "review") %>%
    mutate(concordant = reference_interval == ai_interval)

  eligible_lesions <- classified_lesions %>% filter(size_mm <= 5)

  summary <- summarize_agreement(patient_results) %>%
    mutate(
      decision_threshold = threshold,
      eligible_polyps = nrow(eligible_lesions),
      ai_used_polyps = nrow(eligible_lesions),
      coverage_pct = 100,
      predicted_type2_eligible = sum(
        eligible_lesions$threshold_prediction == 2L
      ),
      predicted_type1_eligible = sum(
        eligible_lesions$threshold_prediction == 1L
      ),
      .before = 1
    )

  list(summary = summary, patient_results = patient_results)
}

standalone_practical_thresholds <- c(
  seq(0.10, 0.90, by = 0.10), 0.95, 0.97, 0.99, 1.00
)
standalone_all_thresholds <- sort(unique(round(c(
  seq(0.01, 0.99, by = 0.01),
  lesion_data$p_type2
), 6)))

standalone_practical <- map_dfr(
  standalone_practical_thresholds,
  ~ evaluate_decision_threshold(.x)$summary
)
standalone_all <- map_dfr(
  standalone_all_thresholds,
  ~ evaluate_decision_threshold(.x)$summary
)
standalone_default <- evaluate_decision_threshold(0.50)
standalone_maximum <- max(standalone_all$agreement_pct)
standalone_best <- standalone_all %>%
  filter(near(agreement_pct, standalone_maximum)) %>%
  arrange(abs(decision_threshold - 0.50), decision_threshold)
standalone_reaching_90 <- standalone_all %>%
  filter(reached_90_percent)

# Analysis 2: use the original AI classification only when predicted-class
# confidence reaches the cutoff; otherwise retain histopathology.

evaluate_confidence_cutoff <- function(cutoff) {
  classified_lesions <- lesion_data %>%
    mutate(
      eligible_for_ai = size_mm <= 5,
      ai_used = eligible_for_ai & confidence >= cutoff,
      surveillance_histology = case_when(
        ai_used & ai_prediction == 1L ~ "HP",
        ai_used & ai_prediction == 2L ~ "adenoma",
        TRUE ~ reference_histology
      )
    )

  selective_intervals <- reconstruct_intervals(
    classified_lesions,
    "surveillance_histology",
    "selective_interval"
  )

  patient_results <- reference_intervals %>%
    select(patient_id, reference_interval) %>%
    left_join(
      selective_intervals %>% select(patient_id, selective_interval),
      by = "patient_id"
    ) %>%
    filter(reference_interval != "review", selective_interval != "review") %>%
    mutate(concordant = reference_interval == selective_interval)

  eligible_polyps <- sum(classified_lesions$eligible_for_ai)
  ai_used_polyps <- sum(classified_lesions$ai_used)

  summary <- summarize_agreement(patient_results) %>%
    mutate(
      confidence_cutoff = cutoff,
      eligible_polyps,
      ai_used_polyps,
      pathology_fallback_polyps = eligible_polyps - ai_used_polyps,
      coverage_pct = 100 * ai_used_polyps / eligible_polyps,
      .before = 1
    )

  list(
    summary = summary,
    patient_results = patient_results,
    lesion_results = classified_lesions
  )
}

selective_practical_cutoffs <- c(
  0.50, 0.60, 0.70, 0.80, 0.85, 0.90,
  0.95, 0.97, 0.98, 0.99, 1.00
)
selective_all_cutoffs <- sort(unique(round(c(
  seq(0.50, 1.00, by = 0.01),
  lesion_data$confidence
), 6)))

selective_practical <- map_dfr(
  selective_practical_cutoffs,
  ~ evaluate_confidence_cutoff(.x)$summary
)
selective_all <- map_dfr(
  selective_all_cutoffs,
  ~ evaluate_confidence_cutoff(.x)$summary
)
selective_reaching_90 <- selective_all %>%
  filter(reached_90_percent)
selective_best_coverage <- selective_reaching_90 %>%
  arrange(desc(coverage_pct), confidence_cutoff) %>%
  slice_head(n = 1)
selective_099 <- evaluate_confidence_cutoff(0.99)

print(standalone_default$summary, n = Inf, width = Inf)
print(standalone_practical, n = Inf, width = Inf)
print(standalone_best, n = Inf, width = Inf)
print(standalone_reaching_90, n = Inf, width = Inf)
print(selective_practical, n = Inf, width = Inf)
print(selective_best_coverage, n = Inf, width = Inf)

write_xlsx(
  list(
    Standalone_default = standalone_default$summary,
    Standalone_practical = standalone_practical,
    Standalone_best = standalone_best,
    Standalone_at_90 = standalone_reaching_90,
    Standalone_all = standalone_all,
    Selective_practical = selective_practical,
    Selective_best_coverage = selective_best_coverage,
    Selective_all = selective_all,
    Selective_0.99_patient = selective_099$patient_results,
    Selective_0.99_lesion = selective_099$lesion_results
  ),
  file.path(output_dir, "AI_surveillance_threshold_analyses.xlsx")
)
