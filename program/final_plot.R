library(readr)
submission_stacking_multinom8 <- read_csv("submission_1/submission_stacking_multinom8.csv")
test_all <- readRDS("data/test_all.rds")

library(dplyr)

# 从 test_all 中提取经纬度列
coords <- test_all %>% select(longitude, latitude)

# 合并列
submission_with_coords <- bind_cols(submission_stacking_multinom8, coords)


library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# 加载坦桑尼亚地图
world <- ne_countries(scale = "medium", returnclass = "sf")
tanzania <- world %>% filter(admin == "United Republic of Tanzania")

# 假设 submission_with_coords 中已有 status_group, longitude, latitude
submission_map <- submission_with_coords %>%
  filter(longitude > 20)

# 转换为 sf 对象
submission_sf <- st_as_sf(submission_map, coords = c("longitude", "latitude"), crs = 4326)

# 可视化
p1 <- ggplot() +
  geom_sf(data = tanzania, fill = "gray95", color = "black") +
  geom_sf(data = submission_sf, aes(color = status_group), alpha = 0.7, size = 0.8) +
  scale_color_manual(
    values = c(
      "functional" = "#1f77b4",
      "functional needs repair" = "#ff7f0e",
      "non functional" = "#9467bd"
    )
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme_minimal() +
  theme(
    plot.margin = margin(0,0,0,0)
  ) +
  labs(
    color = "Status Group"
  )

ggsave("plot/final_prediction.png", plot = p1, width = 8, height = 9, dpi = 300)
