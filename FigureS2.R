library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(scales)

reader_file <- file.path("data", "reader responses.xlsx")
map_file <- file.path("data", "china.geojson")
figure_dir <- file.path("results", "figures")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

reader_data <- read_excel(reader_file, sheet = "Sheet1")
china_map <- st_read(map_file, quiet = TRUE)

province_crosswalk <- tibble::tribble(
  ~Province, ~map_name,
  "Beijing", "北京市",
  "Tianjin", "天津市",
  "Hebei", "河北省",
  "Shanxi", "山西省",
  "Inner Mongolia", "内蒙古自治区",
  "Liaoning", "辽宁省",
  "Jilin", "吉林省",
  "Heilongjiang", "黑龙江省",
  "Shanghai", "上海市",
  "Jiangsu", "江苏省",
  "Zhejiang", "浙江省",
  "Anhui", "安徽省",
  "Fujian", "福建省",
  "Jiangxi", "江西省",
  "Shandong", "山东省",
  "Henan", "河南省",
  "Hubei", "湖北省",
  "Hunan", "湖南省",
  "Guangdong", "广东省",
  "Guangxi", "广西壮族自治区",
  "Hainan", "海南省",
  "Chongqing", "重庆市",
  "Sichuan", "四川省",
  "Guizhou", "贵州省",
  "Yunnan", "云南省",
  "Tibet", "西藏自治区",
  "Shaanxi", "陕西省",
  "Gansu", "甘肃省",
  "Qinghai", "青海省",
  "Ningxia", "宁夏回族自治区",
  "Xinjiang", "新疆维吾尔自治区",
  "Hong Kong", "香港特别行政区",
  "Macao", "澳门特别行政区",
  "Taiwan", "台湾省"
)

province_counts <- reader_data %>%
  distinct(ID, .keep_all = TRUE) %>%
  count(Province, name = "participants") %>%
  left_join(province_crosswalk, by = "Province")

china_plot <- china_map %>%
  left_join(
    select(province_counts, map_name, participants),
    by = c("name" = "map_name")
  ) %>%
  mutate(participants = coalesce(participants, 0L))

maximum_count <- max(china_plot$participants, na.rm = TRUE)
anchor_values <- unique(sort(c(0, 1, 5, 10, 20, 40, maximum_count)))
anchor_positions <- rescale(
  anchor_values,
  to = c(0, 1),
  from = c(0, maximum_count)
)

map_colours <- c(
  "#FFFFFF",
  "#D7EEF5",
  "#9ECAE1",
  "#4F81BD",
  "#FDD49E",
  "#FC8D59",
  "#D7301F"
)[seq_along(anchor_positions)]

china_main <- filter(china_plot, name != "")
china_south_china_sea <- filter(china_plot, name == "")

figure_s2 <- ggplot() +
  geom_sf(
    data = china_main,
    aes(fill = participants),
    colour = "grey55",
    linewidth = 0.35
  ) +
  geom_sf(
    data = china_south_china_sea,
    fill = NA,
    colour = "grey55",
    linewidth = 0.35
  ) +
  scale_fill_gradientn(
    colours = map_colours,
    values = anchor_positions,
    limits = c(0, maximum_count),
    breaks = pretty(c(0, maximum_count), n = 5),
    name = "Participants"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

print(province_counts)
print(figure_s2)

ggsave(
  file.path(figure_dir, "FigureS2.png"),
  figure_s2,
  width = 10,
  height = 8,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(figure_dir, "FigureS2.pdf"),
  figure_s2,
  width = 10,
  height = 8,
  bg = "white"
)
