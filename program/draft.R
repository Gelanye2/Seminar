########any ideas can be saved here.

###1 Idea of preprocessing and variable selection

#step 1:quick preselection emperical knowledge?
sapply(train_all, function(x) length(unique(x)))
sapply(train_clean, function(x) sum(is.na(x)))

table_funder <- train_clean %>%
  count(funder, sort = TRUE)

#step2: statistical test (but need to deal with NA values first)
sapply(train_clean, class)

#step3:simple models to test feature importance

###2 choose rd or lga or ward
data_model <- data_all_clean %>%
  filter(!is.na(status_group)) %>%
  mutate(
    rd_id = as.integer(as.factor(region_district)),
    lga_id = as.integer(as.factor(lga)),
    ward_id = as.integer(as.factor(ward)),
    y = as.integer(as.factor(status_group)) - 1
  )

X <- data_model[, c("rd_id", "lga_id", "ward_id")]
y <- data_model$y

bst <- xgboost(data = as.matrix(X), label = y, nrounds = 20, objective = "multi:softprob", num_class = 3, verbose = 0)
xgb.importance(model = bst)





#####
set.seed(7832)
lgr::get_logger("mlr3")$set_threshold("warn")

train_all_clean <- readRDS("data/train_all_clean.rds")
#

#select cols for training
train_model <- train_all_clean %>%
  select(-longitude, -latitude, -lga, -ward, -subvillage,-installer,-funder,-district_code, -region,-dataset,-year_recorded, -month_recorded
         ,-extraction_type_group, -waterpoint_type_group,-region_district,-region_code)
train_model$gps_height_missing <- is.na(train_model$gps_height)
train_model$population_missing <- is.na(train_model$population)
train_model$years_in_use_missing <- is.na(train_model$years_in_use)
#convert character to factor
train_model <- train_model %>%
  mutate(across(where(is.character), as.factor))
#create task
task <- TaskClassif$new(
  id = "waterpoints",
  backend = train_model,
  target = "status_group"
)
print(task)

#find missing values
missing_summary <- function(df) {
  miss_pct <- sapply(df, function(x) mean(is.na(x)))
  miss_pct[miss_pct > 0] %>% sort(decreasing = TRUE)
}
missing_summary(train_model)

#autoplot
autoplot(task$clone()$select(c("gps_height", "population","years_in_use")), type = "pairs")

###imputation
#select cols
years_features <- c(
                    "extraction_type",
                    "scheme_management",
                    "waterpoint_type_group",
                    "gps_height_missing",
                    "population_missing")


population_features <- c(
  "management",
  "waterpoint_type",
  "extraction_type",
  "source_class"
)

ranger_learner <- lrn("regr.ranger", num.trees = 5, max.depth =2)

#imputation rules
imp_years <-
  po("select", id = "sel_years", selector = selector_name(years_features)) %>>%
  po("imputelearner", id = "imp_years", learner = ranger_learner$clone(deep = TRUE),
     affect_columns = selector_name("years_in_use"))

imp_pop <-
  po("select", id = "sel_pop", selector = selector_name(population_features)) %>>%
  po("imputelearner", id = "imp_pop", learner = ranger_learner$clone(deep = TRUE),
     affect_columns = selector_name("population"))

imp_scheme <- po("imputemode", id = "imp_scheme", affect_columns = selector_name("scheme_management"))
imp_public <- po("imputemode", id = "imp_public", affect_columns = selector_name("public_meeting"))
imp_permit <- po("imputemode", id = "imp_permit", affect_columns = selector_name("permit"))

impute_graph <-
  imp_years %>>%
  imp_pop %>>%
  imp_scheme %>>%
  imp_public %>>%
  imp_permit

#
base_learner <- lrn("classif.rpart", predict_type = "prob")
graph <- impute_graph %>>% base_learner
graph_learner = as_learner(graph)
rr = resample(task, graph_learner, rsmp("holdout"))
rr$aggregate()

na_cols <- names(train_model)[sapply(train_model, function(x) any(is.na(x)))]
train_model_dropped <- train_model %>% select(-all_of(na_cols))
task_dropped <- TaskClassif$new(
  id = "waterpoints_dropna",
  backend = train_model_dropped,
  target = "status_group"
)
rr_drop <- resample(task_dropped, learner, rsmp("holdout"))
rr_drop$aggregate()


# modify extraction_type, delete extraction_type_class, extraction_type_group
# data_all_clean <- data_all_clean %>%
#   mutate(
#     extraction_type = if_else(
#       extraction_type == "other - mkulima/shinyanga",
#       "other",
#       extraction_type)
#   ) %>%
#   mutate(
#     extraction_type = case_when(
#       str_detect(extraction_type, regex("^india mark (ii|iii)$", ignore_case = TRUE)) 
#       ~ "india mark",
#       str_detect(extraction_type, regex("^other - ", ignore_case = TRUE)) 
#       ~ str_remove(extraction_type, regex("^other - ", ignore_case = TRUE)),
#       TRUE ~ extraction_type
#     )
#   )
points_sf <- data_all_buffer %>%
  filter(longitude != 0, latitude != 0) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32736)

####grid methods for neighborhood
#create 1km grid
grid_1km <- st_make_grid(points_sf, cellsize = 1000, square = TRUE) %>%
  st_sf(grid_id = 1:length(.))

#involve the point into the grid
points_with_grid <- st_join(points_sf, grid_1km, join = st_within)

#mark if the point is functional or not
points_with_grid$func_flag <- data_all_buffer$status_group[
  which(data_all_buffer$longitude != 0 & data_all_buffer$latitude != 0)
] == "functional"

grid_stats <- points_with_grid %>%
  group_by(grid_id) %>%
  summarise(
    grid_func_ratio = mean(func_flag, na.rm = TRUE),
    grid_point_count = n()
  )

#contain neighborhood information into origin dataset
grid_stats_df <- grid_stats %>% st_drop_geometry()
points_with_grid <- left_join(points_with_grid, grid_stats_df, by = "grid_id")
data_all_buffer$grid_func_ratio_1k <- NA_real_
valid_rows <- which(data_all_buffer$longitude != 0 & data_all_buffer$latitude != 0)
data_all_buffer$grid_func_ratio_1k[valid_rows] <- points_with_grid$grid_func_ratio
data_all_buffer$grid_point_count_1k <- NA_integer_
data_all_buffer$grid_point_count_1k[valid_rows] <- points_with_grid$grid_point_count

#Add count and smoothed data
alpha <- 1
beta <- 2
smooth_rows <- !is.na(data_all_buffer$grid_func_ratio_1k) &
  !is.na(data_all_buffer$grid_point_count_1k)

func_count <- data_all_buffer$grid_func_ratio_1k[smooth_rows] *
  data_all_buffer$grid_point_count_1k[smooth_rows]

total_count <- data_all_buffer$grid_point_count_1k[smooth_rows]

data_all_buffer$grid_func_ratio_smooth_1k <- NA_real_
data_all_buffer$grid_func_ratio_smooth_1k[smooth_rows] <-
  (func_count + alpha) / (total_count + beta)



mapping_extraction <- unique(data_all_clean[, c("extraction_type", 
                                                "extraction_type_group", 
                                                "extraction_type_class")])

data_all_clean <- data_all_clean %>%
  mutate(
    extraction_type_other = case_when(
      extraction_type %in% c("india mark ii", "india mark iii") ~ "india mark",
      extraction_type == "ksb" ~ "ksb",
      extraction_type == "other - rope pump" ~ "other rope pump",
      TRUE ~ extraction_type_group
    )
  )

train3 <<- data_all_clean
train3 <- train3 %>% filter(dataset == "train") %>% select(-id, -quantity_group, 
                                                           -recorded_by, -amount_tsh,
                                                           -wpt_name, num_private, -dataset,
                                                           -subvillage, -ward)
train3[] <- lapply(train3, function(x) if (is.character(x)) as.factor(x) else x)
train3_raw <- train3 %>% select(-extraction_type_class, -extraction_type_group, -extraction_type_other)
train3_grouped <- train3 %>% select(-extraction_type, -extraction_type_group, -extraction_type_other)
train3_other <- train3 %>% select(-extraction_type, -extraction_type_class, -extraction_type_group)
train3_classed <- train3 %>% select(-extraction_type, -extraction_type_class, -extraction_type_other)

task_raw     <- TaskClassif$new(id = "raw", backend = train3_raw, target = "status_group")
task_grouped <- TaskClassif$new(id = "grouped", backend = train3_grouped, target = "status_group")
task_other <- TaskClassif$new(id = "other", backend = train3_other, target = "status_group")
task_classed <- TaskClassif$new(id = "classed", backend = train3_classed, target = "status_group")

library(mlr3pipelines)
graph <- po("encode", method = "treatment") %>>% lrn("classif.xgboost")
learner <- GraphLearner$new(graph)
resampling <- rsmp("cv", folds = 5)

design <- benchmark_grid(
  tasks = list(task_raw, task_grouped, task_classed, task_other),
  learners = list(learner),
  resamplings = list(resampling)
)

bmr <- benchmark(design)

bmr$aggregate(msr("classif.acc"))

# keep management, delete management_group
mapping_management <- unique(data_all_clean[, c("management", "management_group")])

train3 <<- data_all_clean

train3 <- train3 %>%
  mutate(
    management_other = case_when(
      str_detect(management, regex("^other - ", ignore_case = TRUE)) ~
        str_remove(management, regex("^other - ", ignore_case = TRUE)),
      TRUE ~ management
    )
  )

unique(train3[, c("management", "management_group", "management_other")])

train3 <- train3 %>% filter(dataset == "train") %>% select(-id, -quantity_group, 
                                                           -recorded_by, -amount_tsh,
                                                           -wpt_name, num_private, -dataset,
                                                           -subvillage, -ward, -extraction_type, 
                                                           -extraction_type_group, -extraction_type_other)
train3[] <- lapply(train3, function(x) if (is.character(x)) as.factor(x) else x)
train3_raw <- train3 %>% select(-management_group, -management_other)
train3_grouped <- train3 %>% select(-management, -management_other)
train3_other <- train3 %>% select(-management_other, -management_group)

task_raw     <- TaskClassif$new(id = "raw", backend = train3_raw, target = "status_group")
task_grouped <- TaskClassif$new(id = "grouped", backend = train3_grouped, target = "status_group")
task_other <- TaskClassif$new(id = "other", backend = train3_other, target = "status_group")

learner <- lrn("classif.lightgbm")
resampling <- rsmp("cv", folds = 5)
design <- benchmark_grid(
  tasks = list(task_raw, task_grouped, task_other),
  learners = list(learner),
  resamplings = list(resampling)
)
bmr <- benchmark(design)
bmr$aggregate(msr("classif.acc"))

unique(train3[, c("scheme_management", "scheme_name")])
n_distinct(train3$scheme_management)

train3 <- train3 %>% select(-management_group, -management_other)
colnames(train3)
run_feature_set_benchmark <- function(data, target_col, feature_sets, learner_id = "classif.lightgbm", folds = 5) {
  # Step 1: Ensure all character columns are converted to factors
  data[] <- lapply(data, function(x) if (is.character(x)) as.factor(x) else x)
  
  # Step 2: Construct classification tasks
  tasks <- lapply(seq_along(feature_sets), function(i) {
    features <- feature_sets[[i]]
    task_data <- data[, c(features, target_col)]
    TaskClassif$new(id = paste0("task_", i), backend = task_data, target = target_col)
  })
  
  # Step 3: Setup learner and resampling
  learner <- lrn(learner_id, predict_type = "response")
  resampling <- rsmp("cv", folds = folds)
  
  # Step 4: Create benchmark grid and run
  design <- benchmark_grid(
    tasks = tasks,
    learners = list(learner),
    resamplings = list(resampling)
  )
  
  bmr <- benchmark(design)
  accs <- bmr$aggregate(msr("classif.acc"))
  return(accs)
}

scheme_raw <- setdiff(names(train3), c("scheme_management", "status_group"))
scheme_grouped <- setdiff(names(train3), c("scheme_name", "status_group"))

run_feature_set_benchmark(
  data = train3,
  target_col = "status_group",
  feature_sets = list(scheme_raw, scheme_grouped)
)

train3 <- train3 %>% select(-scheme_name)
colnames(train3)
n_distinct(train3$payment)
n_distinct(train3$payment_type)
unique(train3[, c("payment", "payment_type")])


train3 <- train3 %>% select(-payment)
colnames(train3)
n_distinct(train3$water_quality)
n_distinct(train3$quality_group)
unique(train3[, c("water_quality", "quality_group")])
quality_raw <- setdiff(names(train3), c("quality_group", "status_group"))
quality_grouped <- setdiff(names(train3), c("water_quality", "status_group"))

run_feature_set_benchmark(
  data = train3,
  target_col = "status_group",
  feature_sets = list(quality_raw, quality_grouped)
)

train3 <- train3 %>% select(-water_quality)
colnames(train3)
n_distinct(train3$source)
n_distinct(train3$source_type)
n_distinct(train3$source_class)
unique(train3[, c("source", "source_type", "source_class")])
source_raw <- setdiff(names(train3), c("source_type", "source_class", "status_group"))
source_type <- setdiff(names(train3), c("source", "source_class", "status_group"))
source_class <- setdiff(names(train3), c("source_type", "source", "status_group"))

run_feature_set_benchmark(
  data = train3,
  target_col = "status_group",
  feature_sets = list(source_raw, source_type, source_class)
)

train3 <- train3 %>% select(-source, -source_class)
colnames(train3)
n_distinct(train3$waterpoint_type)
n_distinct(train3$waterpoint_type_group)
unique(train3[, c("waterpoint_type", "waterpoint_type_group")])
waterpoint_raw <- setdiff(names(train3), c("waterpoint_type_group", "status_group"))
waterpoint_group <- setdiff(names(train3), c("waterpoint_type", "status_group"))

run_feature_set_benchmark(
  data = train3,
  target_col = "status_group",
  feature_sets = list(waterpoint_raw, waterpoint_group)
)

train3 <- train3 %>% select(-waterpoint_type)
colnames(train3)

data_all_clean <- data_all_clean %>%
  mutate(
    management_other = case_when(
      str_detect(management, regex("^other - ", ignore_case = TRUE)) ~
        str_remove(management, regex("^other - ", ignore_case = TRUE)),
      TRUE ~ management
    )
  )

# region_district: test
fi_boost %>%
  group_by(region_district) %>%
  summarise(
    n_total     = n(),
    n_missing   = sum(is.na(status_group)),
    pct_missing = mean(is.na(status_group))
  ) %>%
  arrange(n_total)

# --- stack -----
# 0. SETUP AND DATA LOADING
# ==================================
# Ensure required packages are loaded
.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
library(mlr3verse)
library(mlr3pipelines)
library(mlr3learners)
library(dplyr)
library(future)
plan(multisession, workers = 8)
future::plan(future.seed = TRUE)
set.seed(7832)

# fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed.rds") # all:train + test
#sub_imputed <- readRDS("data/sub_imputed.rds") # train + test without longtitude and latitude
test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")

# 1. TASK DEFINITION
# Use the imputed dataset for consistency
df_imp <- imputed
train_imp <- df_imp %>% filter(!is.na(status_group))
test_imp  <- test_full_imp

# train_imp <- fi_clean %>% filter(!is.na(status_group))
# test_imp  <- fi_clean %>% filter( is.na(status_group))

# Ensure target is a factor for classification
train_imp$status_group <- as.factor(train_imp$status_group)

# Create the training task
task <- TaskClassif$new(
  id      = "waterpoints_stacking",
  backend = train_imp,
  target  = "status_group"
)

# 2. DEFINE BASE LEARNERS
# --- Preprocessing Pipelines ---
# Function to treat near-zero longitude/latitude as NA
latlon_to_na <- function(v, thr = 3e-8) {
  v <- as.numeric(v)
  v[abs(v) < thr] <- NA_real_
  v
}

po_latlon_na <- po("colapply", id = "latlon_to_na",
                   param_vals = list(
                     applicator   = latlon_to_na,
                     affect_columns = selector_name(c("longitude", "latitude"))
                   ))

po_latlon_flag <- po("mutate", id = "latlon_flag",
                     param_vals = list(
                       mutation = list(
                         longitude_missing = ~ is.na(longitude),
                         latitude_missing  = ~ is.na(latitude)
                       )
                     ))

po_latlon_impute <- po("imputeconstant", id = "latlon_imputer",
                       param_vals = list(
                         affect_columns = selector_name(c("longitude", "latitude")),
                         constant       = -999 # Use a distinct constant for imputation
                       ))

po_char2fac <- po("colapply",
                  applicator = function(x) if (is.character(x)) as.factor(x) else x,
                  affect_columns = selector_type("character"))

base_core <- po_char2fac %>>%
  po("removeconstants") %>>%
  po("encode", method = "treatment")

# Combine all preprocessing steps into a single graph
base_pipeline <- po_latlon_na %>>%
  po_latlon_flag %>>%
  po_latlon_impute %>>%
  base_core


# Pipeline with scaling for distance-based models (kknn, svm)
# scale_pipeline <- base_pipeline %>>% po("scale")

# --- Create Learner Instances ---
# 1. XGBoost: Uses base pipeline
lrn_xgb <- as_learner(
  base_pipeline %>>%
    lrn("classif.xgboost",
        predict_type = "prob",
        nrounds = 1000L,
        eta = 0.1,
        max_depth = 6,
        subsample = 0.8,
        colsample_bytree = 0.8,
        nthread = 8
    )
)

# 2. Ranger: Tree-models are robust to scaling, but we use a minimal pipeline for consistency
lrn_ranger <- as_learner(
  base_pipeline %>>%
    lrn("classif.ranger",
        predict_type = "prob",
        num.trees = 500,
        mtry = floor(sqrt(ncol(train_imp) - 1)), # ncol-1 for target
        min.node.size = 1,
        num.threads = 8
    )
)

# 3. K-Nearest Neighbors: Uses the pipeline with scaling
# lrn_kknn <- as_learner(
#   scale_pipeline %>>%
#     lrn("classif.kknn",
#         predict_type = "prob",
#         k = 5,
#         distance = 2
#     )
# )

# 4. RPart: Uses base pipeline
# lrn_rpart <- as_learner(
#   base_pipeline %>>%
#     lrn("classif.rpart",
#         predict_type = "prob",
#         cp = 0.001,
#         minsplit = 10,
#         maxdepth = 30
#     )
# )

# Consolidate base learners into a list
base_learners <- list(
  lrn_xgb,
  lrn_ranger
  # lrn_kknn,
  # lrn_rpart
)

# 3. DEFINE AND EVALUATE STACKING MODELS
# Define candidate super learners
super_learners <- list(
  lrn("classif.multinom", id = "multinom", predict_type = "prob"),
  lrn("classif.ranger", id = "ranger", predict_type = "prob", num.trees = 100)
)

# Create a list of stacking learners, one for each super learner
stacked_learners <- lapply(super_learners, function(sl) {
  as_learner(
    ppl("stacking",
        base_learners = base_learners,
        super_learner = sl,
        method = "cv",
        folds = 5,
        use_features = FALSE # A robust starting point
    ),
    id = paste0("stack_", sl$id)
  )
})

# --- Run Benchmark to Select the Best Stacking Configuration ---
resampling <- rsmp("cv", folds = 5) # 3-fold CV for speed, 5 or 10 is more robust
design <- benchmark_grid(
  tasks = task,
  learners = stacked_learners,
  resamplings = resampling
)

# This step is computationally intensive
print("Starting stacking benchmark... this may take a while.")
bmr <- benchmark(design)
print("Benchmark finished.")

print("Benchmark Aggregated Results:")
print(bmr$aggregate(msrs(c("classif.acc", "classif.bacc"))))
# 0.8072171    0.6556998

# 4. FINAL MODEL TRAINING AND PREDICTION
bmr_aggr <- bmr$aggregate(msrs(c("classif.acc", "classif.bacc")))

# Find the ID of the best learner based on accuracy (classif.acc)
best_learner_id <- bmr_aggr[order(-classif.acc)]$learner_id[1]

# Retrieve the actual learner object from the benchmark design
final_learner <- bmr$learners[learner_id == best_learner_id][[1]]

print(paste("Final model selected based on benchmark:", final_learner$id))
print("Training final stacking model on full training data...")

final_learner$train(task)

print("Final model training complete.")
print("Predicting on the test set...")
final_predictions <- final_learner$predict_newdata(newdata = test_imp)
print("Prediction complete.")

# 5. GENERATE SUBMISSION FILE
result_stacking <- data.frame(
  id           = test_all$id,
  status_group = final_predictions$response,
  stringsAsFactors = FALSE
)

# Save results
saveRDS(result_stacking, "data/predictions_stacking.rds")
write.csv(result_stacking, "data/submission_stacking.csv", row.names = FALSE)

print("Submission file 'submission_stacking.csv' has been generated.")

