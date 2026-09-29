library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(tibble)

reader_file <- file.path("data", "reader responses.xlsx")
polyp_file <- file.path("data", "rjai_total.xlsx")
output_dir <- file.path("results", "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

reader_data <- read_excel(reader_file, sheet = "Sheet1")
polyp_data <- read_excel(polyp_file, sheet = "Sheet1")

normalise_text <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("＜", "<") %>%
    str_replace_all("＞", ">") %>%
    str_trim()
}

physician_data <- reader_data %>%
  distinct(ID, .keep_all = TRUE) %>%
  transmute(
    group = factor(
      paste("Group", normalise_text(Group)),
      levels = c("Group A", "Group B", "Group C")
    ),
    age = as.numeric(Age),
    sex = str_to_lower(normalise_text(Sex)),
    digestive_experience = factor(
      normalise_text(Years_of_experience_in_digestive_endoscopy),
      levels = c("<3 years", "3-10 years", "10-20 years", ">20 years"),
      ordered = TRUE
    ),
    professional_title = factor(
      recode(
        normalise_text(Professional_title),
        "Chief Physician" = "Chief physician",
        "Associate Chief Physician" = "Associate chief physician",
        "Attending Physician" = "Attending physician",
        "Resident doctor" = "Resident"
      ),
      levels = c("Chief physician", "Associate chief physician",
                 "Attending physician", "Resident"),
      ordered = TRUE
    ),
    practice_institution = factor(
      recode(normalise_text(Type_of_practice_institution),
             "Public" = "Public hospital", "Private" = "Private hospital"),
      levels = c("Public hospital", "Private hospital")
    ),
    hospital_level = factor(
      normalise_text(Hospital_level),
      levels = c("Tertiary hospital", "Other")
    ),
    exclusive_endoscopy = factor(
      normalise_text(Exclusive_endoscopy_practice),
      levels = c("Yes", "No")
    ),
    colonoscopy_volume = factor(
      normalise_text(Total_number_of_colonosopies_performed),
      levels = c("<100", "100-500", "500-1000", "1000-2000",
                 "2000-3000", "3000-5000", "5000-10000", ">10000"),
      ordered = TRUE
    ),
    lower_gi_experience = factor(
      normalise_text(Years_of_experience_in_lower_gastrointestinal_endoscopy),
      levels = c("<1 year", "1-3 years", "3-5 years", ">5 years"),
      ordered = TRUE
    ),
    nbi_experience = factor(
      normalise_text(Years_of_experience_with_NBI),
      levels = c("<1 year", "1-3 years", "3-5 years", ">5 years"),
      ordered = TRUE
    )
  )

polyp_data <- polyp_data %>%
  transmute(
    Morphology = as.character(Morphology),
    Histology = as.character(Histology),
    Size_group = case_when(
      as.numeric(Size) <= 5 ~ "≤5 mm",
      as.numeric(Size) <= 9 ~ "6-9 mm",
      as.numeric(Size) >= 10 ~ "≥10 mm"
    ),
    Location = as.character(Location)
  )

format_p <- function(p) {
  ifelse(p < 0.01, "<0.01", formatC(p, format = "f", digits = 2))
}

format_n_pct <- function(n, denominator) {
  sprintf("%d (%.1f)", n, 100 * n / denominator)
}

format_mean_sd <- function(x) {
  sprintf("%.1f ± %.1f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

continuous_p <- function(data, variable) {
  kruskal.test(data[[variable]], data$group)$p.value
}

categorical_p <- function(data, variable) {
  tab <- table(data$group, data[[variable]])
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))

  if (any(chi$expected < 5)) {
    if (all(dim(tab) == c(2, 2))) {
      fisher.test(tab)$p.value
    } else {
      fisher.test(tab, simulate.p.value = TRUE, B = 10000)$p.value
    }
  } else {
    chi$p.value
  }
}

section_row <- function(label, p_value = "") {
  tibble(
    Characteristic = label,
    Overall = "",
    `Group A` = "",
    `Group B` = "",
    `Group C` = "",
    `P value` = p_value
  )
}

categorical_block <- function(
    data, variable, label, level_order, display_labels = level_order) {
  group_sizes <- table(data$group)
  rows <- lapply(seq_along(level_order), function(i) {
    level <- level_order[i]
    values <- as.character(data[[variable]])
    tibble(
      Characteristic = paste0("  ", display_labels[i]),
      Overall = format_n_pct(sum(values == level, na.rm = TRUE), nrow(data)),
      `Group A` = format_n_pct(
        sum(values[data$group == "Group A"] == level, na.rm = TRUE),
        group_sizes[["Group A"]]
      ),
      `Group B` = format_n_pct(
        sum(values[data$group == "Group B"] == level, na.rm = TRUE),
        group_sizes[["Group B"]]
      ),
      `Group C` = format_n_pct(
        sum(values[data$group == "Group C"] == level, na.rm = TRUE),
        group_sizes[["Group C"]]
      ),
      `P value` = ""
    )
  }) %>%
    bind_rows()

  bind_rows(section_row(label, format_p(categorical_p(data, variable))), rows)
}

group_sizes <- table(physician_data$group)

table1 <- bind_rows(
  tibble(
    Characteristic = "Number of participating physicians, n",
    Overall = as.character(nrow(physician_data)),
    `Group A` = as.character(group_sizes[["Group A"]]),
    `Group B` = as.character(group_sizes[["Group B"]]),
    `Group C` = as.character(group_sizes[["Group C"]]),
    `P value` = ""
  ),
  tibble(
    Characteristic = "Age, years",
    Overall = format_mean_sd(physician_data$age),
    `Group A` = format_mean_sd(physician_data$age[physician_data$group == "Group A"]),
    `Group B` = format_mean_sd(physician_data$age[physician_data$group == "Group B"]),
    `Group C` = format_mean_sd(physician_data$age[physician_data$group == "Group C"]),
    `P value` = format_p(continuous_p(physician_data, "age"))
  ),
  tibble(
    Characteristic = "Male sex, n (%)",
    Overall = format_n_pct(sum(physician_data$sex == "male"), nrow(physician_data)),
    `Group A` = format_n_pct(
      sum(physician_data$sex[physician_data$group == "Group A"] == "male"),
      group_sizes[["Group A"]]
    ),
    `Group B` = format_n_pct(
      sum(physician_data$sex[physician_data$group == "Group B"] == "male"),
      group_sizes[["Group B"]]
    ),
    `Group C` = format_n_pct(
      sum(physician_data$sex[physician_data$group == "Group C"] == "male"),
      group_sizes[["Group C"]]
    ),
    `P value` = format_p(categorical_p(physician_data, "sex"))
  ),
  categorical_block(
    physician_data, "digestive_experience", "Experience in digestive endoscopy",
    c("<3 years", "3-10 years", "10-20 years", ">20 years"),
    c("<3 years", "3–10 years", "10–20 years", ">20 years")
  ),
  categorical_block(
    physician_data, "professional_title", "Professional title",
    c("Chief physician", "Associate chief physician", "Attending physician", "Resident")
  ),
  categorical_block(
    physician_data, "practice_institution", "Type of practice institution",
    c("Public hospital", "Private hospital")
  ),
  categorical_block(
    physician_data, "hospital_level", "Hospital level",
    c("Tertiary hospital", "Other")
  ),
  categorical_block(
    physician_data, "exclusive_endoscopy", "Exclusive endoscopy practice",
    c("Yes", "No")
  ),
  categorical_block(
    physician_data, "colonoscopy_volume", "Total colonoscopies performed",
    c("<100", "100-500", "500-1000", "1000-2000", "2000-3000",
      "3000-5000", "5000-10000", ">10000"),
    c("<100", "100–500", "500–1000", "1000–2000", "2000–3000",
      "3000–5000", "5000–10000", ">10000")
  ),
  categorical_block(
    physician_data, "lower_gi_experience", "Experience in lower GI endoscopy",
    c("<1 year", "1-3 years", "3-5 years", ">5 years"),
    c("<1 year", "1–3 years", "3–5 years", ">5 years")
  ),
  categorical_block(
    physician_data, "nbi_experience", "Experience with NBI",
    c("<1 year", "1-3 years", "3-5 years", ">5 years"),
    c("<1 year", "1–3 years", "3–5 years", ">5 years")
  )
)

polyp_block <- function(data, variable, label, level_order, display_labels = level_order) {
  rows <- lapply(seq_along(level_order), function(i) {
    level <- level_order[i]
    tibble(
      Characteristic = paste0("  ", display_labels[i]),
      Value = format_n_pct(sum(data[[variable]] == level, na.rm = TRUE), nrow(data))
    )
  }) %>%
    bind_rows()

  bind_rows(tibble(Characteristic = label, Value = ""), rows)
}

table_s1 <- bind_rows(
  tibble(Characteristic = "Number of polyps, n", Value = as.character(nrow(polyp_data))),
  polyp_block(polyp_data, "Morphology", "Morphology", c("Ip", "Is", "Isp", "IIa")),
  polyp_block(
    polyp_data, "Histology", "Histology",
    c("Tubular adenoma", "Hyperplastic polyp", "Sessile serrated lesion",
      "Tubulovillous adenoma", "Other"),
    c("Tubular adenoma", "Hyperplastic polyp", "Sessile serrated lesion",
      "Tubulovillous adenoma", "Other (inflammatory, lymphoid tissue hyperplasia, etc.)")
  ),
  polyp_block(
    polyp_data, "Size_group", "Size, mm",
    c("≤5 mm", "6-9 mm", "≥10 mm"),
    c("≤5 mm", "6–9 mm", "≥10 mm")
  ),
  polyp_block(
    polyp_data, "Location", "Location",
    c("Cecum", "Ascending colon", "Transverse colon", "Descending colon",
      "Sigmoid colon", "Rectum")
  )
)

print(table1, n = Inf)
print(table_s1, n = Inf)

write_xlsx(list(Table_1 = table1), file.path(output_dir, "Table1.xlsx"))
write_xlsx(list(Table_S1 = table_s1), file.path(output_dir, "TableS1.xlsx"))
