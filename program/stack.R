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

fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
#imputed <- readRDS("data/data_imputed.rds") # all:train + test
#sub_imputed <- readRDS("data/sub_imputed.rds") # train + test without longtitude and latitude
#test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")
xgb_params <- readRDS("result/xgb_tuning_params.rds")

# 1. TASK DEFINITION
# Use the imputed dataset for consistency
#df_imp <- imputed
#train_imp <- df_imp %>% filter(!is.na(status_group))
#test_imp  <- test_full_imp

# Split the data into training and testing sets.
train_raw <- fi_clean %>% filter(!is.na(status_group))
test_raw  <- fi_clean %>% filter(is.na(status_group))

# Ensure the target variable is a factor.
train_raw$status_group <- as.factor(train_raw$status_group)

# Create the classification task from the raw (non-imputed) training data.
task_raw <- TaskClassif$new(
  id      = "waterpoints_raw_task",
  backend = train_raw,
  target  = "status_group")

# 2. DEFINE BASE LEARNERS
# --- Define common preprocessing steps ---
po_char_to_factor <- po("colapply",
                        id = "char_to_factor",
                        applicator = function(x) if (is.character(x)) as.factor(x) else x,
                        affect_columns = selector_type("character"))

base_core_pipeline <- po_char_to_factor %>>%
  po("removeconstants", id = "remove_constants") %>>%
  po("encode", id = "one_hot_encode", method = "treatment")

# --- Define specific PipeOps for data cleaning and imputation ---
# PipeOp to convert near-zero latitude/longitude to NA.
latlon_to_na_func <- function(v, threshold = 3e-8) {
  v <- as.numeric(v); v[abs(v) < threshold] <- NA_real_; return(v)
}

po_latlon_na <- po("colapply", id = "latlon_to_na",
                      param_vals = list(
                        applicator   = latlon_to_na_func,
                        affect_columns = selector_name(c("longitude", "latitude"))))

po_latlon_flag <- po("mutate", id = "latlon_flag",
                     param_vals = list(
                       mutation = list(
                         longitude_missing = ~ is.na(longitude),
                         latitude_missing  = ~ is.na(latitude))))

po_latlon_impute <- po("imputeconstant", id = "latlon_imputer",
                       param_vals = list(
                         affect_columns = selector_name(c("longitude", "latitude")),
                         constant       = -999 ))

# --- Construct the two final, separate pipelines ---
# Pipeline A (for XGBoost): Converts invalid coordinates to NA and does NO imputation.
# It leaves all NA values for the XGBoost algorithm to handle.
pipeline_for_xgb <- po_latlon_na %>>%
  base_core_pipeline

# Pipeline B (for Ranger): This pipeline performs all imputation steps.
po_impute_median <- po("imputemedian", id = "impute_median_numeric",
                       affect_columns = selector_name(c("gps_height", "years_in_use")))

po_impute_mode <- po("imputemode", id = "impute_mode_factors",
                     affect_columns = selector_name(c("public_meeting", "permit", "installer", "funder", "scheme_name")))

# Chain all the steps for the Ranger pipeline together.
pipeline_for_ranger <- po_latlon_na %>>%
  po_latlon_flag %>>%
  po_latlon_impute %>>%
  po_impute_median %>>%              
  po_impute_mode %>>%    
  base_core_pipeline

# 3. GRAPH LEARNER DEFINITION
# Graph Learner 1: XGBoost with its minimal preprocessing pipeline.
lrn_xgb_graph <- as_learner(
  pipeline_for_xgb %>>%
    lrn("classif.xgboost", predict_type = "prob",
        nrounds = xgb_params$classif.xgboost.nrounds, 
        eta = xgb_params$classif.xgboost.eta, 
        max_depth = xgb_params$classif.xgboost.max_depth, 
        colsample_bytree = xgb_params$classif.xgboost.colsample_bytree,
        eval_metric = xgb_params$classif.xgboost.eval_metric,
        gamma = xgb_params$classif.xgboost.gamma,
        alpha = xgb_params$classif.xgboost.alpha,
        lambda = xgb_params$classif.xgboost.lambda,
        min_child_weight = xgb_params$classif.xgboost.min_child_weight,
        subsample = xgb_params$classif.xgboost.subsample,
        nthread = 8))
lrn_xgb_graph$id <- "xgboost.with_na"

# Graph Learner 2: Ranger with its FULL imputation pipeline.
lrn_ranger_graph <- as_learner(
  pipeline_for_ranger %>>%
    lrn("classif.ranger", predict_type = "prob", 
        num.trees = 1142,
        mtry = 4,
        max.depth = 60,
        min.node.size = 4,
        importance = "impurity",
        num.threads = 8))
lrn_ranger_graph$id <- "ranger.imputed"

# 4. STACKING ENSEMBLE DEFINITION
specialized_base_learners <- list(lrn_xgb_graph, lrn_ranger_graph)
super_learners <- list(
  lrn("classif.multinom", id = "multinom", predict_type = "prob"),
  lrn("classif.ranger", id = "ranger", predict_type = "prob", num.trees = 100))

stacked_learners <- lapply(super_learners, function(sl) {
  as_learner(
    ppl("stacking",
        base_learners = specialized_base_learners,
        super_learner = sl,
        method = "cv",
        folds = 5,
        use_features = FALSE 
    ),
    id = paste0("stack_", sl$id)
  )
})

# 5. BENCHMARKING (MODEL EVALUATION)
resampling_cv5 <- rsmp("cv", folds = 5)
design <- benchmark_grid(tasks = task_raw, learners = stacked_learners,
                         resamplings = resampling_cv5)
bmr <- benchmark(design)
print(bmr$aggregate(msrs(c("classif.acc", "classif.bacc"))))

bmr_aggr <- bmr$aggregate(msrs(c("classif.acc")))
best_learner_id <- bmr_aggr[order(-classif.acc)]$learner_id[1]
final_learner <- Filter(function(l) l$id == best_learner_id, stacked_learners)[[1]]

# 6. FINAL MODEL TRAINING AND PREDICTION
final_learner$train(task_raw)
final_predictions <- final_learner$predict_newdata(newdata = test_raw)

# 7. GENERATE SUBMISSION FILE
result_stacking <- data.frame(
  id           = test_all$id,
  status_group = final_predictions$response,
  stringsAsFactors = FALSE)
write.csv(result_stacking, "data/submission_stacking.csv", row.names = FALSE)

all_artifacts <- list(
  benchmark_result = bmr,
  benchmark_aggregation = bmr_aggr,
  best_learner_id = best_learner_id,
  final_trained_learner = final_learner,
  final_predictions_object = final_predictions,
  submission_dataframe = result_stacking)
saveRDS(all_artifacts, file = "data/all_artifacts.rds")

