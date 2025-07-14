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
plan(multisession, workers = 30)
future::plan(future.seed = TRUE)
set.seed(7832)

# Load data (ensure paths are correct)
imputed <- readRDS("data/data_imputed_enhanced.rds")

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
latlon_to_na <- function(v, thr = 3e-8) {
  v <- as.numeric(v)
  v[abs(v) < thr] <- NA_real_
  v
}
po_latlon_na <- po("colapply", id = "latlon_to_na",
                   param_vals = list(
                     applicator     = latlon_to_na,
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
                         constant       = -999
                       ))
po_char2fac <- po("colapply", id = "char2factor",
                  param_vals = list(
                    applicator     = as.factor,
                    affect_columns = selector_type("character")
                  ))
base_pipeline <- po_latlon_na %>>%
  po_latlon_flag %>>%
  po_latlon_impute %>>%
  po_char2fac %>>%
  po("removeconstants") %>>%
  po("encode", param_vals = list(method = "treatment"))

# --- Create Learner Instances ---
lrn_xgb <- as_learner(
  base_pipeline %>>%
    po("learner", learner = lrn("classif.xgboost", predict_type = "prob", nthread = 10), id = "learner")
)
lrn_xgb$id <- "xgboost.base"

lrn_ranger <- as_learner(
  base_pipeline %>>%
    po("learner", learner = lrn("classif.ranger", predict_type = "prob", num.threads = 10), id = "learner")
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
  xgboost.base.learner.max_depth = p_int(lower = 6, upper = 8),
  xgboost.base.learner.nrounds = p_int(lower = 400, upper = 500),
  xgboost.base.learner.eta = p_dbl(lower = 0.01, upper = 0.05, logscale = TRUE),
  ranger.base.learner.mtry = p_int(lower = 3, upper = 5),
  ranger.base.learner.min.node.size = p_int(lower = 3, upper = 5)
)

message("STEP 3: Starting holistic tuning on the data subset...")
# Configure and run the AutoTuner
at_small <- AutoTuner$new(
  learner = stacked_learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.logloss"),
  search_space = search_space_stack,
  terminator = trm("evals", n_evals = 10),
  tuner = tnr("random_search")
)
at_small$train(task_for_tuning)

message("Tuning complete.")
print("Best found hyperparameters and performance on subset:")
print(at_small$tuning_result)

# 4. TRAIN FINAL MODELS & PRINT TRAINSET ACCURACY
# ==========================================================
message("\nSTEP 4: Training final models on FULL dataset and printing trainset accuracy...")

# --- Extract best parameters ---
best_params <- at_small$tuning_result$learner_param_vals

# --- Model 1: Stacking Model ---
message("... (1) Training final Stacking model.")
# The AutoTuner object `at_small` already contains the trained learner with the best parameters.
# We just need to retrain it on the full task.
stacked_learner_final <- at_small$learner
stacked_learner_final$train(task)

# Predict on the full training task and calculate accuracy
# Change predict type to "response" for accuracy calculation
stacked_learner_final$predict_type <- "response"
preds_stack_train <- stacked_learner_final$predict(task)
acc_stack <- preds_stack_train$score(msr("classif.acc"))
message(sprintf("Stacking Model Train ACC: %.4f", acc_stack))
cat("---\n")


# --- Model 2: Standalone XGBoost Model ---
message("... (2) Training final standalone XGBoost model.")
lrn_xgb_final <- lrn_xgb$clone(deep = TRUE)
xgb_params <- best_params[grepl("^xgboost\\.base", names(best_params))]
names(xgb_params) <- sub("^xgboost\\.base\\.", "", names(xgb_params))
lrn_xgb_final$param_set$values <- mlr3misc::insert_named(
  lrn_xgb_final$param_set$values,
  xgb_params
)
lrn_xgb_final$train(task)

# Predict on trainset and get accuracy
lrn_xgb_final$predict_type <- "response"
preds_xgb_train <- lrn_xgb_final$predict(task)
acc_xgb <- preds_xgb_train$score(msr("classif.acc"))
message(sprintf("Standalone XGBoost Train ACC: %.4f", acc_xgb))
cat("---\n")


# --- Model 3: Standalone Ranger Model ---
message("... (3) Training final standalone Ranger model.")
lrn_ranger_final <- lrn_ranger$clone(deep = TRUE)
ranger_params <- best_params[grepl("^ranger\\.base", names(best_params))]
names(ranger_params) <- sub("^ranger\\.base\\.", "", names(ranger_params))
lrn_ranger_final$param_set$values <- mlr3misc::insert_named(
  lrn_ranger_final$param_set$values,
  ranger_params
)
lrn_ranger_final$train(task)

# Predict on trainset and get accuracy
lrn_ranger_final$predict_type <- "response"
preds_ranger_train <- lrn_ranger_final$predict(task)
acc_ranger <- preds_ranger_train$score(msr("classif.acc"))
message(sprintf("Standalone Ranger Train ACC: %.4f", acc_ranger))
cat("---\n")


# --- Model 4: Blending Model ---
message("... (4) Calculating accuracy for Blending model.")
# Blending uses out-of-fold predictions. This is the best way to estimate its "train" accuracy.
cv5 <- rsmp("cv", folds = 5)
lrn_xgb_for_blend <- lrn_xgb_final$clone(deep = TRUE)
lrn_xgb_for_blend$predict_type = "prob"

lrn_ranger_for_blend <- lrn_ranger_final$clone(deep = TRUE)
lrn_ranger_for_blend$predict_type = "prob"

rr_xgb_final <- resample(task, lrn_xgb_for_blend, cv5, store_models = FALSE)
rr_ranger_final <- resample(task, lrn_ranger_for_blend, cv5, store_models = FALSE)

preds_xgb_oof <- rr_xgb_final$prediction()$prob[order(rr_xgb_final$prediction()$row_ids), ]
preds_ranger_oof <- rr_ranger_final$prediction()$prob[order(rr_ranger_final$prediction()$row_ids), ]

message("... searching for optimal blending weight 'w'.")
w_values <- seq(0, 1, by = 0.01)
truth_sorted <- rr_xgb_final$prediction()$truth[order(rr_xgb_final$prediction()$row_ids)]
true_matrix <- model.matrix(~ truth_sorted - 1)

logloss_scores <- sapply(w_values, function(w) {
  blended_preds_oof <- w * preds_xgb_oof + (1 - w) * preds_ranger_oof
  MLmetrics::LogLoss(y_pred = blended_preds_oof, y_true = true_matrix)
})
best_w <- w_values[which.min(logloss_scores)]
message(paste("... optimal weight for XGBoost (w) is:", round(best_w, 3)))

# Calculate accuracy based on the blended out-of-fold predictions
blended_preds_oof_prob <- best_w * preds_xgb_oof + (1 - best_w) * preds_ranger_oof
blended_response <- colnames(blended_preds_oof_prob)[apply(blended_preds_oof_prob, 1, which.max)]
blended_response_factor <- factor(blended_response, levels = levels(truth_sorted))

acc_blend <- mean(blended_response_factor == truth_sorted)
message(sprintf("Blending Model OOF Train ACC: %.4f", acc_blend))
cat("---\n")

message("All four model results printed successfully.")
