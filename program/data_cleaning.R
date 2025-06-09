##########spatial cleaning
###Contributed by: Haoran Ju

data_all <- readRDS("data/data_all.rds")

###initial reason for imputing coordinates
outliers <- data_all %>%
  filter(longitude == 0 | latitude == 0) %>%
  select(id, longitude, latitude, dataset)  %>%
  nrow()

train_all <- data_all %>% filter(dataset == "train")
outliers_train <- train_all %>%
  filter(longitude == 0 | latitude == 0) %>%
  select(id, longitude, latitude, dataset) %>%
  nrow()
outliers_train/nrow(train_all)  #0.03


###reflection of region to region code: many to many
# if one region_code corresponds to multiple regions
data_all %>%
  group_by(region_code) %>%
  summarise(n_region = n_distinct(region)) %>%
  filter(n_region > 1)

# if one region corresponds to multiple region_codes
data_all %>%
  group_by(region) %>%
  summarise(n_code = n_distinct(region_code)) %>%
  filter(n_code > 1)
#view the elationship in table
table<- data_all %>%
  group_by(region_code, region) %>%
  tally()


###modify region_code, one to one reflection
region_name_map <- data_all %>%
  distinct(region) %>%
  arrange(region) %>%
  mutate(region_code_new = row_number())
data_all_clean <- data_all %>%
  left_join(region_name_map, by = "region") %>%
  mutate(region_code = region_code_new) %>%
  select(-region_code_new)

data_all_clean %>%
  count(district_code) %>%
  arrange(desc(n))

###if region_district is a reasonable combination for later processing - yes
data_all_clean <- data_all_clean %>%
  mutate(region_district = paste(region_code, district_code, sep = "_")) %>%
  relocate(region_district, .after = district_code)

region_district_summary <- data_all_clean %>%
  group_by(region_district) %>%
  summarise(
    n_lga = n_distinct(lga),
    n_ward = n_distinct(ward),
    n_obs = n()
  ) %>%
  arrange(desc(n_ward))

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

### check outliers in region_district: missing coordinates are concentrated,
### imputation not possible
table_outliers <- data_all_clean %>%
  filter(latitude == -2e-08 & longitude == 0) %>%
  count(lga) %>%
  arrange(desc(n))

###conclusion: decision for main model + submodel(only region 14 +18)


###cleaning other columns
#contributed by Yuxin Liu
data_all_clean <- data_all_clean %>%
  mutate(
    year_recorded = as.integer(format(date_recorded, "%Y")),
    month_recorded = as.integer(format(date_recorded, "%m")),
    status_group = as.factor(status_group)
  ) %>%
  select(-date_recorded)

# years_in_use (year_recorded - construction_year)
# delete construction_year and day_recorded
# keep month_recorded
data_all_clean <- data_all_clean %>%
  mutate(
    construction_year = na_if(construction_year, 0),
    years_in_use = if_else(
      year_recorded - construction_year >= 0,
      year_recorded - construction_year,
      as.numeric(NA)
    )
  )

# modify extraction_type, delete extraction_type_class, extraction_type_group
data_all_clean <- data_all_clean %>%
  mutate(
    extraction_type = if_else(
      extraction_type == "other - mkulima/shinyanga",
      "other",
      extraction_type)
  ) %>%
  mutate(
    extraction_type = case_when(
      str_detect(extraction_type, regex("^india mark (ii|iii)$", ignore_case = TRUE))
      ~ "india mark",
      str_detect(extraction_type, regex("^other - ", ignore_case = TRUE))
      ~ str_remove(extraction_type, regex("^other - ", ignore_case = TRUE)),
      TRUE ~ extraction_type
    )
  )

# keep management, delete management_group
data_all_clean <- data_all_clean %>%
  mutate(
    management = case_when(
      str_detect(management, regex("^other - ", ignore_case = TRUE)) ~
        str_remove(management, regex("^other - ", ignore_case = TRUE)),
      TRUE ~ management
    )
  )

# delete some columns
cols_remove <- c("id", "quantity_group", "recorded_by",
                 "amount_tsh", "wpt_name", "num_private", "extraction_type_class",
                 "extraction_type_group", "management_group", "payment",
                 "quality_group", "source_class", "source_type", "scheme_name", 
                 "construction_year", "waterpoint_type_group")
data_all_clean <- data_all_clean %>% select(-all_of(cols_remove))


######save the cleaned dataset
train_all_clean <- data_all_clean %>% filter(dataset == "train")
test_all_clean <- data_all_clean %>% filter(dataset == "test")
saveRDS(data_all_clean, "data/data_all_clean.rds")
saveRDS(train_all_clean, "data/train_all_clean.rds")
saveRDS(test_all_clean, "data/test_all_clean.rds")
