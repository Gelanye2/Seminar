# 0. SETUP AND DATA LOADING
# ==================================
# Ensure required packages are loaded
library(mlr3)
library(mlr3pipelines)
library(mlr3learners)
library(dplyr)
library(future)
library(mlr3tuning)
library(mlr3mbo)
library(data.table)
library(MLmetrics)
library(purrr)
library(paradox)
library(rgenoud)
library(DiceKriging)

# Setup parallel processing and seed
plan(sequential)
set.seed(7832)

# Load data (ensure paths are correct)
imputed <- readRDS("data/data_imputed_scheme.rds")

# 1. TASK DEFINITION
# ==================================
# Use the imputed dataset for consistency
train_imp <- imputed %>% filter(!is.na(status_group))

# Ensure target is a factor for classification
train_imp$status_group <- as.factor(train_imp$status_group)

message("STEP 1: Defining tasks, pipelines, and learner structures...")
# Create the training task on the FULL dataset
task <- TaskClassif$new(
  id      = "waterpoints_stacking",
  backend = train_imp,
  target  = "status_group"
)

# 2. DEFINE BASE LEARNERS AND PIPELINE
# ==================================
# --- Preprocessing Pipeline (remains unchanged) ---
po_char2fac <- po("colapply", id = "char2factor",
                  param_vals = list(
                    applicator     = as.factor,
                    affect_columns = selector_type("character")
                  ))
base_pipeline <- po_char2fac %>>%
  po("removeconstants") %>>%
  po("encode", param_vals = list(method = "treatment"))

# --- Create Learner Instances ---
lrn_xgb <- as_learner(
  base_pipeline %>>%
    po("learner", learner = lrn("classif.xgboost", predict_type = "prob", nthread = 20), id = "learner")
)
lrn_xgb$id <- "xgboost.base"

lrn_ranger <- as_learner(
  base_pipeline %>>%
    po("learner", learner = lrn("classif.ranger", predict_type = "prob", num.threads = 20), id = "learner")
)
lrn_ranger$id <- "ranger.base"
base_learners <- list(lrn_xgb, lrn_ranger)

# --- Define Stacking Model ---
message("Defining Model with Multinom super learner...")
lrn_super_multinom <- lrn("classif.multinom", predict_type = "prob", MaxNWts = 5000)
stacked_learner <- as_learner(
  ppl("stacking",
      base_learners = base_learners,
      super_learner = lrn_super_multinom,
      method = "cv", folds = 5, use_features = FALSE
  ), id = "stack_multinom"
)

# 3. TUNE ON A SUBSET
# ==================================
message("STEP 2: Setting up for faster tuning using STRATIFIED SAMPLING...")
# Create a stratified subset for tuning
n_samples_per_group <- round(table(train_imp$status_group) * 0.25)
set.seed(7832)
train_imp_subset <- train_imp %>%
  group_by(status_group) %>%
  group_split() %>%
  map_dfr(function(df_group) {
    group_name <- df_group$status_group[1]
    n_to_sample <- n_samples_per_group[as.character(group_name)]
    slice_sample(df_group, n = n_to_sample, replace = FALSE)
  })
task_for_tuning <- TaskClassif$new(
  id      = "waterpoints_tuning_stratified",
  backend = train_imp_subset,
  target  = "status_group"
)

# Define a small search space
search_space_stack <- ps(
  # XGBoost parameters
  xgboost.base.learner.max_depth = p_int(lower = 4, upper = 7),
  xgboost.base.learner.nrounds = p_int(lower = 900, upper = 1100),
  xgboost.base.learner.eta = p_dbl(lower = 0.01, upper = 0.05, logscale = TRUE),
  xgboost.base.learner.gamma = p_dbl(lower = 0, upper = 5),
  xgboost.base.learner.lambda = p_dbl(lower = 1, upper = 3, logscale = TRUE),
  xgboost.base.learner.subsample = p_dbl(lower = 0.7, upper = 0.8),
  xgboost.base.learner.colsample_bytree = p_dbl(lower = 0.7, upper = 0.8),
  
  # Ranger parameters
  ranger.base.learner.mtry = p_int(lower = 53, upper = 60),
  ranger.base.learner.min.node.size = p_int(lower = 3, upper = 5)
)

message("STEP 3: Starting holistic tuning on the data subset...")
# Configure and run the AutoTuner
at_small <- AutoTuner$new(
  learner = stacked_learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.acc"),
  search_space = search_space_stack,
  terminator = trm("evals", n_evals = 15),
  tuner = tnr("random_search"),
  store_models = TRUE
)
at_small$train(task_for_tuning)

message("Tuning complete.")
tuning_archive_dt <- as.data.table(at_small$archive)
sorted_tuning_archive <- tuning_archive_dt %>%
  arrange(desc(classif.acc))
print(head(sorted_tuning_archive))
write.csv(sorted_tuning_archive, "result/full_archive_sorted_by_acc.csv", row.names = FALSE)
saveRDS(at_small, "result/autotuner_final_object.rds")
