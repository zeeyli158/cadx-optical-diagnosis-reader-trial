library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

interval_file <- file.path("results", "doctor_usfollow.xlsx")
figure_dir <- file.path("results", "figures")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

patient_intervals <- read_excel(interval_file, sheet = "out_patient")

normalize_interval <- function(x) {
  x <- as.character(x) %>%
    str_trim() %>%
    str_replace_all("–|—|－|~|～|to", "-") %>%
    str_replace_all("\\s+", "")

  case_when(
    x %in% c("3年", "3y", "3yr", "3year", "3years") ~ "3y",
    x %in% c("3-5年", "3-5y", "3-5yr", "3-5year", "3-5years") ~ "3-5y",
    x %in% c("5年", "5y", "5yr", "5year", "5years") ~ "5y",
    x %in% c("5-10年", "5-10y", "5-10yr", "5-10year", "5-10years") ~ "5-10y",
    x %in% c("7-10年", "7-10y", "7-10yr", "7-10year", "7-10years") ~ "7-10y",
    x %in% c("10年", "10y", "10yr", "10year", "10years") ~ "10y",
    TRUE ~ NA_character_
  )
}

paper_levels <- c("卷1", "卷2", "卷3")
paper_labels <- c(
  "卷1" = "Group A",
  "卷2" = "Group B",
  "卷3" = "Group C"
)

interval_long <- patient_intervals %>%
  select(patient_id, us_gold_recalc, matches("^卷[123]_")) %>%
  pivot_longer(
    cols = matches("^卷[123]_"),
    names_to = "physician_id",
    values_to = "physician_interval"
  ) %>%
  mutate(
    paper = str_extract(physician_id, "^卷[123]"),
    reference_interval = normalize_interval(us_gold_recalc),
    reconstructed_interval = normalize_interval(physician_interval)
  ) %>%
  filter(
    !is.na(reference_interval),
    !is.na(reconstructed_interval)
  )

preferred_levels <- c("10y", "7-10y", "5-10y", "5y", "3-5y", "3y")
present_levels <- preferred_levels[
  preferred_levels %in%
    unique(c(
      interval_long$reference_interval,
      interval_long$reconstructed_interval
    ))
]
interval_levels <- rev(present_levels)

heatmap_data <- interval_long %>%
  count(
    paper,
    reference_interval,
    reconstructed_interval,
    name = "n"
  ) %>%
  complete(
    paper = paper_levels,
    reference_interval = interval_levels,
    reconstructed_interval = interval_levels,
    fill = list(n = 0)
  ) %>%
  group_by(paper) %>%
  mutate(
    total = sum(n),
    overall_proportion = n / total,
    label = if_else(
      n == 0,
      "",
      paste0(n, "\n(", sprintf("%.1f", 100 * overall_proportion), "%)")
    )
  ) %>%
  ungroup() %>%
  mutate(
    paper = factor(paper, levels = paper_levels),
    reference_interval = factor(
      reference_interval,
      levels = interval_levels
    ),
    reconstructed_interval = factor(
      reconstructed_interval,
      levels = interval_levels
    )
  )

maximum_proportion <- max(heatmap_data$overall_proportion, na.rm = TRUE)

figure_s3 <- ggplot(
  heatmap_data,
  aes(
    x = reconstructed_interval,
    y = reference_interval,
    fill = overall_proportion
  )
) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(
    aes(label = label),
    size = 4,
    colour = "#2F2F2F",
    lineheight = 0.95
  ) +
  facet_wrap(
    ~ paper,
    nrow = 1,
    labeller = labeller(paper = paper_labels)
  ) +
  scale_fill_gradientn(
    colours = c(
      "#D7EEF5",
      "#9ECAE1",
      "#4F81BD",
      "#FDD49E",
      "#FC8D59",
      "#D7301F"
    ),
    values = rescale(c(0, 0.01, 0.03, 0.07, 0.12, maximum_proportion)),
    limits = c(0, maximum_proportion),
    labels = percent_format(accuracy = 0.1),
    name = "Overall %"
  ) +
  labs(
    x = "Endoscopist-recommended surveillance interval",
    y = "Reference standard surveillance interval"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "plain", colour = "black"),
    axis.title = element_text(size = 14, face = "plain", colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
    axis.text.y = element_text(colour = "black"),
    panel.grid = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

print(figure_s3)

ggsave(
  file.path(figure_dir, "FigureS3.png"),
  figure_s3,
  width = 15,
  height = 5.8,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(figure_dir, "FigureS3.pdf"),
  figure_s3,
  width = 15,
  height = 5.8,
  bg = "white"
)
