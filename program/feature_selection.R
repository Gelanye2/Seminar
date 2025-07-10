source("setup.R")

fi_clean <- readRDS("data/fi_clean.rds")

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
train3 <- train3 %>% select(-scheme_name)
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
train3$rainy_season2 <- NULL
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
