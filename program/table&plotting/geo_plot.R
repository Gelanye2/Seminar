# Contributed by: Haoran Ju
# Setup

source("setup.R")

library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(pROC)

# Load Data
data_all        <- readRDS("data/data_all.rds")
data_all_clean  <- readRDS("data/data_all_clean.rds")
train_all_clean <- readRDS("data/train_all_clean.rds")
test_all_clean  <- readRDS("data/test_all_clean.rds")

# Filter valid coordinates only
data_clean <- data_all %>%
  filter(longitude != 0, latitude != 0)

data_clean_sf <- st_as_sf(data_clean, coords = c("longitude", "latitude"), crs = 4326)

# Get Tanzania map
world    <- ne_countries(scale = "medium", returnclass = "sf")
tanzania <- world %>% filter(admin == "United Republic of Tanzania")

# Pump distribution map (train/test)
ggplot() +
  geom_sf(data = tanzania, fill = "gray95", color = "black") +
  geom_sf(data = data_clean_sf, aes(color = dataset), size = 0.6, alpha = 0.7) +
  scale_color_manual(values = c("train" = "steelblue", "test" = "tomato")) +
  facet_wrap(~dataset) +
  labs(x = "Longitude", y = "Latitude", color = "Dataset") +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "cm"),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
  ) +
  guides(color = guide_legend(override.aes = list(size = 4)))

ggsave("pre/pump_distribution.png", width = 10, height = 5.5, dpi = 300)

# Training set preparation
train_clean <- data_clean %>% filter(dataset == "train")
train_clean_sf <- st_as_sf(train_clean, coords = c("longitude", "latitude"), crs = 4326)

# Target variable distribution
train_clean %>%
  count(status_group) %>%
  mutate(prop = n / sum(n))

# Pump status distribution (spatial)
ggplot() +
  geom_sf(data = tanzania, fill = "gray95", color = "black") +
  geom_sf(data = train_clean_sf, aes(color = status_group), alpha = 0.7, size = 0.8) +
  theme_minimal() +
  labs(title = "Spatial Distribution of Pump Status", color = "Status Group")

# Region-level distribution (district_code)
ggplot() +
  geom_sf(data = tanzania, fill = "gray95", color = "black") +
  geom_sf(data = train_clean_sf, aes(color = as.factor(district_code)), alpha = 0.7, size = 0.8) +
  facet_wrap(~ region_code) +
  theme_minimal() +
  labs(
    title = "Spatial Distribution by Region Code (Colored by District Code)",
    color = "District Code"
  )

# Barplot: status by region
train_clean %>%
  group_by(region, status_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(region) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = reorder(region, -prop), y = prop, fill = status_group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Pump Status Proportions by Region",
    x = "Region", y = "Proportion"
  )

# Train-test split at district level
train_test_split <- data_all_clean %>%
  count(region_district, dataset) %>%
  tidyr::pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>%
  mutate(
    total      = train + test,
    test_ratio = round(test / total, 3)
  ) %>%
  arrange(desc(test_ratio))

# AUC Test: Can latitude/longitude separate test/train?
data_clean_coords <- data_all_clean %>%
  filter(longitude != 0 & latitude != 0) %>%
  mutate(is_test = ifelse(dataset == "test", 1, 0))

glm_model <- glm(is_test ~ latitude + longitude, data = data_clean_coords, family = binomial())
data_clean_coords$pred_prob <- predict(glm_model, type = "response")

auc_score <- roc(data_clean_coords$is_test, data_clean_coords$pred_prob)$auc
cat(sprintf("AUC (train vs test separability by coords): %.4f\n", auc_score))
# ~0.4978

