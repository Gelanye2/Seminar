##########spatial cleaning
###Contributed by: Haoran Ju
source("setup.R")
set.seed(7832)
###Contributed by: Haoran Ju, Gelan Ye

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

# select best features
best_feature <- function(data, base_col, group_col, class_col = NULL, other_col = NULL, 
         target_col = "status_group", folds = 5) {
  set.seed(7832)
  data <- data %>%
    mutate(across(where(is.character), as.factor))
  
  # --- This new section automatically defines base and candidate features ---
  
  # Step A: Consolidate all provided candidate columns into one vector
  candidate_features <- c(base_col, group_col, class_col, other_col)
  
  # Step B: Define all features in the dataset (excluding the target)
  all_features <- setdiff(names(data), target_col)
  
  # Step C: Define base features as all features MINUS the candidates
  base_features <- setdiff(all_features, candidate_features)
  print(candidate_features)
  # ---------------------------------------------------------------------
  
  results <- list()
  
  # --- Define mlr3 components once for efficiency ---
  graph <- po("encode") %>>% lrn("classif.lightgbm")
  glrn <- GraphLearner$new(graph)
  resamp <- rsmp("cv", folds = folds)
  msr_acc <- msr("classif.acc")
  
  # --- Loop through each candidate feature ---
  for (candidate in candidate_features) {
    
    message(paste("\nEvaluating feature variant:", candidate))
    
    # Define the exact feature set for this run: base features + current candidate
    current_feature_set <- c(base_features, candidate)
    
    # Create a task using only this specific set of features
    task <- TaskClassif$new(
      id = paste0("task_with_", candidate),
      backend = data,
      target = target_col
    )$select(cols = current_feature_set)
    
    # Run 5-fold cross-validation
    rr <- mlr3::resample(task, glrn, resamp, store_models = FALSE)
    
    # Aggregate accuracy and store the result
    acc <- rr$aggregate(msr_acc)
    results[[candidate]] <- acc
  }
  
  # --- Assemble, display, and return the final results table ---
  acc_table <- data.table(
    variant_tested = names(results),
    accuracy = unlist(results)
  )[order(-accuracy)]
  
  return(acc_table)
}

# use train3 instead of data_all_clean to test this function
train3 <<- data_all_clean
train3 <- train3 %>% filter(dataset == "train") %>% select(-id, -quantity_group, 
                                                           -recorded_by, -amount_tsh,
                                                           -wpt_name, num_private, -dataset,
                                                           -subvillage, -ward, -region, 
                                                           -district_code, -subvillage, -ward, 
                                                           -lga, -region_code, -longitude, -latitude)
# "extraction_type", "extraction_type_group", "extraction_type_class", "extraction_type_other"
train3 <- train3 %>%
  mutate(
    extraction_type_other = case_when(
      extraction_type %in% c("india mark ii", "india mark iii") ~ "india mark",
      extraction_type == "ksb" ~ "ksb",
      extraction_type == "other - rope pump" ~ "other rope pump",
      TRUE ~ extraction_type_group
    )
  )

best_feature(data = train3, base_col = "extraction_type", group_col = "extraction_type_group", 
                       class_col = "extraction_type_class", other_col = "extraction_type_other", folds = 5)
train3$extraction_type_class <- NULL
train3$extraction_type <- NULL
train3$extraction_type_other <- NULL
colnames(train3)

# "management", "management_group", "management_other"
train3 <- train3 %>%
  mutate(
    management_other = case_when(
      str_detect(management, regex("^other - ", ignore_case = TRUE)) ~
        str_remove(management, regex("^other - ", ignore_case = TRUE)),
      TRUE ~ management
    )
  )

best_feature(data = train3, base_col = "management", group_col = "management_group",
                       other_col = "management_other", folds = 5)
train3$management <- NULL
train3$management_group <- NULL
colnames(train3)

# "scheme_management", "scheme_name"
unique(data_all_clean[, c("scheme_management", "scheme_name")])
train3 <- train3 %>% select(-scheme_management)
# train3 <- best_feature(data = train3, base_col = "scheme_name", group_col = "scheme_management", folds = 5)
colnames(train3)

# "payment", "payment_type"
unique(train3[, c("payment", "payment_type")])
train3 <- train3 %>% select(-payment)
colnames(train3)

# "water_quality", "quality_group"
unique(data_all_clean[, c("water_quality", "quality_group")])
train3 <- train3 %>%
  mutate(
    water_group = if_else(water_quality == "fluoride abandoned", "fluoride", 
                          as.character(water_quality))
  )
best_feature(data = train3, base_col = "water_quality", group_col = "quality_group", 
             other_col = "water_group", folds = 5)
train3$water_group <- NULL
train3$quality_group <- NULL
colnames(train3)

# "source", "source_type", "source_class"
unique(data_all_clean[, c("source", "source_type", "source_class")])
train3 <- train3 %>%
  mutate(
    source_group = if_else(source == "unknown", "other", as.character(source))
  )
best_feature(data = train3, base_col = "source", group_col = "source_type", 
             class_col = "source_class", other_col = "source_group", folds = 5)
train3$source <- NULL
train3$source_type <- NULL
train3$source_class <- NULL
colnames(train3)

# "waterpoint_type", "waterpoint_type_group"
unique(data_all_clean[, c("waterpoint_type", "waterpoint_type_group")])
train3 <- train3 %>%
  mutate(
    waterpoint_group = if_else(waterpoint_type == "dam", "other", as.character(waterpoint_type))
  )
best_feature(data = train3, base_col = "waterpoint_type", group_col = "waterpoint_type_group", 
             other_col = "waterpoint_group", folds = 5)
train3$waterpoint_type_group <- NULL
train3$waterpoint_group <- NULL
colnames(train3)

# years_in_use, year_recorded
best_feature(data = train3, base_col = "year_recorded", group_col = "years_in_use", folds = 5)
train3$year_recorded <- NULL
colnames(train3)

# season
train3 <- train3 %>%
  mutate(
    season = case_when(
      month_recorded %in% c(12, 1, 2) ~ "winter",
      month_recorded %in% 3:5         ~ "spring",
      month_recorded %in% 6:8         ~ "summer",
      month_recorded %in% 9:11        ~ "autumn"
    ),
    
    rainy_season4 = case_when(
      month_recorded %in% 1:2        ~ "dry short",
      month_recorded %in% 3:5         ~ "wet long",
      month_recorded %in% 6:9         ~ "dry long",
      month_recorded %in% 10:12       ~ "wet short"
    ),
    
    rainy_season2 = case_when(
      month_recorded %in% 6:10 ~ "dry_season",
      TRUE                     ~ "wet_season" 
    )
  )
best_feature(data = train3, base_col = "month_recorded", group_col = "season", 
             other_col = "rainy_season4", class_col = "rainy_season2", folds = 5)
train3$month_recorded <- NULL
train3$rainy_season4 <- NULL
train3$season <- NULL

train3$status_group <- as.factor(train3$status_group)
train3 <- train3 %>%
  mutate(across(where(is.character), as.factor))
task <- as_task_classif(train3, target = "status_group", id = "water_wells_baseline")
learner <- lrn("classif.lightgbm", predict_type = "prob")
resampling_cv5 <- rsmp("cv", folds = 5)
rr <- mlr3::resample(
  task = task,
  learner = learner,
  resampling = resampling_cv5,
  store_models = FALSE
)
accuracy_score <- rr$aggregate(msr("classif.acc"))

# delete some columns
colnames(data_all_clean)
cols_remove <- c("id", "quantity_group", "recorded_by",
                 "amount_tsh", "wpt_name", "num_private", "extraction_type_class",
                 "extraction_type_group", "management_group", "payment",
                 "quality_group", "source_class", "source_type", "scheme_name",
                 "construction_year", "waterpoint_type_group")

data_all_clean <- data_all_clean %>% select(-all_of(cols_remove))

# keep only gps height > 0
data_all_clean <- data_all_clean %>%
  mutate(gps_height = ifelse(gps_height <= 0, NA, gps_height))

train_all_clean <- data_all_clean %>% filter(dataset == "train")

###### Clean the duplicate rows
## for train dataset
# 1. Identify all duplicate coordinates in original data
duplicates_raw <- train_all_clean %>%
  filter(longitude != 0 & latitude != 0) %>%
  group_by(longitude, latitude) %>%
  filter(n() > 1) %>%
  ungroup()

# 2. Clean
duplicates_cleaned <- duplicates_raw %>%
  distinct() %>%
  group_by(longitude, latitude) %>%
  filter(!(n() > 1 & (is.na(gps_height) & population == 0))) %>%
  filter(!(n() > 1 & is.na(permit))) %>%
  ungroup()

# 3. Remove all duplicated coordinate rows from original
train_all_clean <- train_all_clean %>%
  anti_join(duplicates_raw, by = c("longitude", "latitude"))

# 4. Add back the cleaned version
train_all_clean <- bind_rows(train_all_clean, duplicates_cleaned)

## for test dataset
test_all_clean <- data_all_clean %>% filter(dataset == "test")
# Add row index to identify original rows
test_all_clean <- test_all_clean %>%
  mutate(row_index = row_number())

# Find duplicated rows (by coordinates)
duplicates_raw_test <- test_all_clean %>%
  filter(longitude != 0 & latitude != 0) %>%
  group_by(longitude, latitude) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(longitude, latitude)

# replace row 1 with row 2, and row 8 with row 7
row1_index <- duplicates_raw_test$row_index[1]
row2_index <- duplicates_raw_test$row_index[2]
row7_index <- duplicates_raw_test$row_index[7]
row8_index <- duplicates_raw_test$row_index[8]

# Overwrite in the original dataset using row_index
test_all_clean[row1_index, ] <- test_all_clean[row2_index, ]
test_all_clean[row8_index, ] <- test_all_clean[row7_index, ]
test_all_clean <- test_all_clean %>%
  select(-row_index)  # Remove the row_index column

data_all_clean <- bind_rows(train_all_clean, test_all_clean)

######save the cleaned dataset
saveRDS(data_all_clean, "data/data_all_clean.rds")
saveRDS(train_all_clean, "data/train_all_clean.rds")
saveRDS(test_all_clean, "data/test_all_clean.rds")

