library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

agreement_file <- file.path("results", "pass_rate_with_basic.xlsx")
npv_file <- file.path("results", "npv_with_basic.xlsx")
figure_dir <- file.path("results", "figures")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

agreement_data <- read_excel(
  agreement_file,
  sheet = "pass_rate_with_basic"
)

npv_data <- read_excel(
  npv_file,
  sheet = "npv_with_basic"
)

paper_levels <- c("卷1", "卷2", "卷3")
group_labels <- c("Group A", "Group B", "Group C")
group_colours <- c(
  "Group A" = "#A9C5E8",
  "Group B" = "#B9DEC9",
  "Group C" = "#F2CBB7"
)

agreement_plot_data <- agreement_data %>%
  filter(!is.na(agreement_pct), is.finite(agreement_pct)) %>%
  mutate(
    group = factor(paper, levels = paper_levels, labels = group_labels)
  )

npv_plot_data <- npv_data %>%
  filter(!is.na(npv_pct), is.finite(npv_pct)) %>%
  mutate(
    group = factor(paper, levels = paper_levels, labels = group_labels)
  )

figure_theme <- theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, colour = "black"),
    axis.text.x = element_text(
      size = 11,
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      colour = "black"
    ),
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.line = element_line(linewidth = 0.6),
    plot.tag = element_text(size = 12, face = "plain"),
    plot.tag.position = c(0.01, 0.98),
    plot.margin = margin(8, 10, 8, 8)
  )

make_panel <- function(data, outcome, y_axis_title) {
  ggplot(
    data,
    aes(x = group, y = .data[[outcome]], fill = group, colour = group)
  ) +
    geom_violin(
      width = 0.9,
      alpha = 0.35,
      linewidth = 0.4,
      trim = FALSE
    ) +
    geom_boxplot(
      width = 0.18,
      outlier.shape = NA,
      alpha = 0.9,
      colour = "grey20",
      fill = "white",
      linewidth = 0.5
    ) +
    geom_point(
      position = position_jitter(width = 0.10, height = 0, seed = 123456),
      size = 1.8,
      alpha = 0.8,
      stroke = 0
    ) +
    stat_summary(
      fun = median,
      geom = "point",
      shape = 23,
      size = 2,
      fill = "white",
      colour = "grey20",
      stroke = 0.6
    ) +
    geom_hline(
      yintercept = 90,
      linetype = "dashed",
      linewidth = 0.6,
      colour = "grey45"
    ) +
    annotate(
      "text",
      x = 3.28,
      y = 90.8,
      label = "90%",
      hjust = 0,
      vjust = -0.1,
      size = 4,
      colour = "grey35"
    ) +
    scale_fill_manual(values = group_colours) +
    scale_colour_manual(values = group_colours) +
    scale_y_continuous(
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.01, 0.03))
    ) +
    coord_cartesian(ylim = c(0, 105)) +
    labs(y = y_axis_title) +
    figure_theme
}

panel_a <- make_panel(
  agreement_plot_data,
  outcome = "agreement_pct",
  y_axis_title = "Surveillance interval agreement (%)"
)

panel_b <- make_panel(
  npv_plot_data,
  outcome = "npv_pct",
  y_axis_title = "NPV for diminutive rectosigmoid polyps (%)"
)

figure_2 <- panel_a + panel_b +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A")

print(figure_2)

ggsave(
  file.path(figure_dir, "Figure2.png"),
  figure_2,
  width = 13,
  height = 6.2,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(figure_dir, "Figure2.pdf"),
  figure_2,
  width = 13,
  height = 6.2,
  bg = "white"
)
