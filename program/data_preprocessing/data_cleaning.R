# ========================================================
# Data Cleaning
# Contributed by: Haoran Ju, Yuxin Liu, Gelan Ye
# ========================================================

source("setup.R")

# Load Raw Data
data_all <- readRDS("data/data_all.rds")

# Step 1: Check Coordinate Outliers
# How many entries have missing or zero coordinates
outliers <- data_all %>%
  filter(longitude == 0 | latitude == 0) %>%
  nrow()
# How many are in the training data
train_all <- data_all %>% filter(dataset == "train")
outliers_train <- train_all %>%
  filter(longitude == 0 | latitude == 0) %>%
  nrow()

outliers_train / nrow(train_all)  # ≈ 3%

# Step 2: Check Region/Region_Code Mapping (Many-to-Many)
# Region_code → multiple regions?
data_all %>%
  group_by(region_code) %>%
  summarise(n_region = n_distinct(region)) %>%
  filter(n_region > 1)

# Region → multiple region_codes?
data_all %>%
  group_by(region) %>%
  summarise(n_code = n_distinct(region_code)) %>%
  filter(n_code > 1)

# Full mapping table
table <- data_all %>%
  group_by(region_code, region) %>%
  tally()

# Step 3: One-to-One Region Mapping
region_name_map <- data_all %>%
  distinct(region) %>%
  arrange(region) %>%
  mutate(region_code_new = row_number())

data_all_clean <- data_all %>%
  left_join(region_name_map, by = "region") %>%
  mutate(region_code = region_code_new) %>%
  select(-region_code_new)

# Create unique identifier: region_district
data_all_clean <- data_all_clean %>%
  mutate(region_district = paste(region_code, district_code, sep = "_")) %>%
  relocate(region_district, .after = district_code)

# Overview of each region_district
region_district_summary <- data_all_clean %>%
  group_by(region_district) %>%
  summarise(
    n_lga = n_distinct(lga),
    n_ward = n_distinct(ward),
    n_obs = n()
  ) %>%
  arrange(desc(n_ward))

# Step 4: Visualize Sample Region (Many Wards)
world <- ne_countries(scale = "medium", returnclass = "sf")
tanzania <- world %>% filter(admin == "United Republic of Tanzania")

data_sf <- st_as_sf(data_all_clean, coords = c("longitude", "latitude"), crs = 4326)

data_sf %>%
  filter(region_district == "18_3") %>%
  ggplot() +
  geom_sf(data = tanzania, fill = "gray95", color = "black") +
  geom_sf(aes(color = ward, fill = ward), alpha = 0.6, size = 1) +
  theme_minimal() +
  labs(title = "Distribution in region_district with most wards")

# Step 5: Check Missing Coordinates
# These are highly concentrated → not suitable for imputation
table_outliers <- data_all_clean %>%
  filter(latitude == -2e-08 & longitude == 0) %>%
  count(lga) %>%
  arrange(desc(n))

# → Decision: Train submodels only for region_districts 14 + 18

# Step 6: Clean/Transform Columns (Yuxin Liu)
data_all_clean <- data_all_clean %>%
  mutate(
    year_recorded = as.integer(format(date_recorded, "%Y")),
    month_recorded = as.integer(format(date_recorded, "%m")),
    status_group = as.factor(status_group)
  ) %>%
  select(-date_recorded)

# Years in use (remove invalid years)
data_all_clean <- data_all_clean %>%
  mutate(
    construction_year = na_if(construction_year, 0),
    years_in_use = if_else(
      year_recorded - construction_year >= 0,
      year_recorded - construction_year,
      as.numeric(NA)
    )
  )

# Management cleaning
data_all_clean <- data_all_clean %>%
  mutate(
    management_other = case_when(
      str_detect(management, regex("^other - ", ignore_case = TRUE)) ~
        str_remove(management, regex("^other - ", ignore_case = TRUE)),
      TRUE ~ management
    ),
    source_group = if_else(source == "unknown", "other", as.character(source))
  )

# Rain season categorization
data_all_clean <- data_all_clean %>%
  mutate(
    rainy_season4 = case_when(
      month_recorded %in% 1:2        ~ "dry short",
      month_recorded %in% 3:5        ~ "wet long",
      month_recorded %in% 6:9        ~ "dry long",
      month_recorded %in% 10:12      ~ "wet short"
    )
  )

# Drop irrelevant/redundant columns
data_all_clean <- data_all_clean %>%
  select(-id, -quantity_group, -recorded_by, -amount_tsh,
         -wpt_name, -num_private, -subvillage, -ward, -region,
         -district_code, -lga, -region_code, -extraction_type_class,
         -extraction_type, -management, -management_group,
         -scheme_management, -payment, -quality_group,
         -source, -source_type, -source_class,
         -waterpoint_type_group, -month_recorded, -construction_year)

# Step 7: Train/Test Split and De-duplicate (Gelan Ye)
train_all_clean <- data_all_clean %>% filter(dataset == "train")

# -- De-duplicate training coordinates --
duplicates_raw <- train_all_clean %>%
  filter(longitude != 0 & latitude != 0) %>%
  group_by(longitude, latitude) %>%
  filter(n() > 1) %>%
  ungroup()

duplicates_cleaned <- duplicates_raw %>%
  distinct() %>%
  group_by(longitude, latitude) %>%
  filter(!(n() > 1 & (is.na(gps_height) & population == 0))) %>%
  filter(!(n() > 1 & is.na(permit))) %>%
  ungroup()

train_all_clean <- train_all_clean %>%
  anti_join(duplicates_raw, by = c("longitude", "latitude")) %>%
  bind_rows(duplicates_cleaned)

# -- Test set duplicates --
test_all_clean <- data_all_clean %>% filter(dataset == "test") %>%
  mutate(row_index = row_number())

duplicates_raw_test <- test_all_clean %>%
  filter(longitude != 0 & latitude != 0) %>%
  group_by(longitude, latitude) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(longitude, latitude)

# Manual replacement for known duplicated pairs
row1_index <- duplicates_raw_test$row_index[1]
row2_index <- duplicates_raw_test$row_index[2]
row7_index <- duplicates_raw_test$row_index[7]
row8_index <- duplicates_raw_test$row_index[8]

test_all_clean[row1_index, ] <- test_all_clean[row2_index, ]
test_all_clean[row8_index, ] <- test_all_clean[row7_index, ]
test_all_clean <- test_all_clean %>% select(-row_index)

# Merge datasets back
data_all_clean <- bind_rows(train_all_clean, test_all_clean)

# Step 8: Save Cleaned Data
saveRDS(data_all_clean, "data/data_all_clean.rds")
saveRDS(train_all_clean, "data/train_all_clean.rds")
saveRDS(test_all_clean, "data/test_all_clean.rds")

