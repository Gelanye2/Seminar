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
best_feature <- function(data, base_col, group_col, class_col = NULL, other_col = NULL, folds = 5) {
  set.seed(7832)
  target_col <- "status_group"
  
  # create other_col if requested
  if (!is.null(other_col)) {
    data <- data %>% mutate(!!other_col := !!sym(group_col))
  }
  
  # force character → factor
  data[] <- lapply(data, function(x) if (is.character(x)) factor(x) else x)
  
  # build feature‐drop lists
  all_feats <- setdiff(names(data), target_col)
  
  drop_raw  <- c(group_col, class_col, other_col) %>% na.omit()
  drop_grp  <- c(base_col, class_col, other_col) %>% na.omit()
  drop_oth  <- if (!is.null(other_col)) c(base_col, group_col, class_col) %>% na.omit() else NULL
  drop_cls  <- if (!is.null(class_col)) c(base_col, group_col, other_col) %>% na.omit() else NULL
  
  # assemble available feature sets
  feature_sets <- list(
    raw     = setdiff(all_feats, drop_raw),
    grouped = setdiff(all_feats, drop_grp)
  )
  if (!is.null(other_col)) {
    feature_sets$other <- setdiff(all_feats, drop_oth)
  }
  if (!is.null(class_col)) {
    feature_sets$classed <- setdiff(all_feats, drop_cls)
  }
  
  # evaluate each via 5-fold CV
  results <- lapply(names(feature_sets), function(name) {
    cols    <- feature_sets[[name]]
    df_sub  <- data[, c(cols, target_col)]
    task    <- TaskClassif$new(name, df_sub, target = target_col)
    learner <- lrn("classif.lightgbm", predict_type = "response")
    resamp  <- rsmp("cv", folds = folds)
    rr      <- resample(task, learner, resamp, store_models = FALSE)
    acc     <- rr$aggregate(msr("classif.acc"))
    list(name = name, acc = acc, data = df_sub)
  })
  
  # calculate accuracy
  acc_table <- do.call(rbind, lapply(results, function(x) {
    data.frame(variant = x$name, accuracy = x$acc, row.names = NULL)
  }))
  
  # find the best
  best_idx <- which.max(acc_table$accuracy)
  best_data <- results[[best_idx]]$data
  
  # report all and return
  message("Accuracies by variant:")
  print(acc_table)
  message(sprintf("Best variant = '%s' (ACC = %f)",
                  acc_table$variant[best_idx],
                  acc_table$accuracy[best_idx]))
  # pick best
  best <- Reduce(function(a, b) if (a$acc > b$acc) a else b, results)
  return(best$data)
}

# use train3 instead of data_all_clean to test this function
train3 <<- data_all_clean
train3 <- train3 %>% filter(dataset == "train") %>% select(-id, -quantity_group, 
                                                           -recorded_by, -amount_tsh,
                                                           -wpt_name, num_private, -dataset,
                                                           -subvillage, -ward)

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

train3 <- best_feature(data = train3, base_col = "extraction_type", group_col = "extraction_type_group", 
                       class_col = "extraction_type_class", other_col = "extraction_type_other", folds = 5)
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

train3 <- best_feature(data = train3, base_col = "management", group_col = "management_group",
                       other_col = "management_other", folds = 5)
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
train3 <- best_feature(data = train3, base_col = "water_quality", group_col = "quality_group", folds = 5)
colnames(train3)

# "source", "source_type", "source_class"
unique(data_all_clean[, c("source", "source_type", "source_class")])
train3 <- best_feature(data = train3, base_col = "source", group_col = "source_type", 
                       class_col = "source_class", folds = 5)
colnames(train3)

# "waterpoint_type", "waterpoint_type_group"
unique(data_all_clean[, c("waterpoint_type", "waterpoint_type_group")])
train3 <- best_feature(data = train3, base_col = "waterpoint_type", group_col = "waterpoint_type_group", 
                       folds = 5)

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

