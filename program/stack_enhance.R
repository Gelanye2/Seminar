# 0. SETUP AND DATA LOADING
# ==================================
# Ensure required packages are loaded
library(mlr3pipelines)
library(mlr3learners)
library(dplyr)
library(future)
library(mlr3tuning) 
library(mlr3mbo)
library(data.table)
library(scoring)
library(purrr)
library(rgenoud)
library(DiceKriging)
plan(multisession, workers = 20)
future::plan(future.seed = TRUE)
set.seed(7832)

# fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed_enhanced.rds")
#sub_imputed <- readRDS("data/sub_imputed.rds") # train + test without longtitude and latitude
# test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")

# 1. TASK DEFINITION
# Use the imputed dataset for consistency
train_imp <- imputed %>% filter(!is.na(status_group))
test_imp  <- imputed %>% filter(is.na(status_group))

# Ensure target is a factor for classification
train_imp$status_group <- as.factor(train_imp$status_group)

message("STEP 1: Defining tasks, pipelines, and learner structures...")
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
  v[ abs(v) < thr ] <- NA_real_
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
# 1. XGBoost: Uses base pipeline
lrn_xgb <- as_learner(
  base_pipeline %>>%
    po("learner", learner = lrn("classif.xgboost", predict_type = "prob", nthread = 1), id = "learner")
)
lrn_xgb$id <- "xgboost.base"

# 2. Ranger: Tree-models are robust to scaling, but we use a minimal pipeline for consistency
lrn_ranger <- as_learner(
  base_pipeline %>>%
    po("learner", learner = lrn("classif.ranger", predict_type = "prob", num.threads = 1), id = "learner")
)
lrn_ranger$id <- "ranger.base"
base_learners <- list(lrn_xgb, lrn_ranger)

# --- MODEL : Stacking with Multinom Super Learner ---
message("Defining Model with Multinom super learner...")
# Multinom is a linear model, providing diversity to the tree-based Ranger
lrn_super_multinom <- lrn("classif.multinom", predict_type = "prob", MaxNWts = 5000)

stacked_learner <- as_learner(
  ppl("stacking",
      base_learners = base_learners, # Re-using the same base learners
      super_learner = lrn_super_multinom,
      method = "cv", folds = 5, use_features = FALSE
  ), id = "stack_multinom"
)

message("STEP 2: Setting up for faster tuning using STRATIFIED SAMPLING...")

# --- Step 1: Create a data subset via Stratified Sampling ---
# We stratify by the target variable 'status_group' to maintain class proportions.
# This ensures the subset is highly representative of the full dataset.
message("... creating a 15,000-row stratified subset from the training data.")

# Set the proportion of data to sample (15,000 / 59,400 ≈ 0.2525)
# Or simply define the number of samples per group
n_samples_per_group <- round(table(train_imp$status_group) * 0.25)

# A more robust dplyr way to do stratified sampling
set.seed(7832) # for reproducibility
train_imp_subset <- train_imp %>%
  group_by(status_group) %>%
  group_split() %>%
  map_dfr(function(df_group) {
    group_name <- df_group$status_group[1]
    n_to_sample <- n_samples_per_group[as.character(group_name)]
    slice_sample(df_group, n = n_to_sample, replace = FALSE)
  })

message(paste("Subset created with", nrow(train_imp_subset), "rows."))
print("Original data class distribution:")
print(prop.table(table(train_imp$status_group)))
print("Subset data class distribution:")
print(prop.table(table(train_imp_subset$status_group)))

# Create a NEW task based on the subset for tuning
task_for_tuning <- TaskClassif$new(
  id      = "waterpoints_tuning_stratified",
  backend = train_imp_subset,
  target  = "status_group"
)

message("... defining the hyperparameter search space for teamwork.")
search_space_stack = ps(
  # XGBoost parameters
  xgboost.base.learner.max_depth = p_int(lower = 4, upper = 9),
  xgboost.base.learner.nrounds = p_int(lower = 200, upper = 800),
  xgboost.base.learner.eta = p_dbl(lower = 0.01, upper = 0.2, logscale = TRUE),
  xgboost.base.learner.gamma = p_dbl(lower = 0, upper = 5),
  xgboost.base.learner.lambda = p_dbl(lower = 0.1, upper = 10, logscale = TRUE),
  xgboost.base.learner.subsample = p_dbl(lower = 0.6, upper = 0.95),
  xgboost.base.learner.colsample_bytree = p_dbl(lower = 0.4, upper = 0.8),
  
  # Ranger parameters
  ranger.base.learner.mtry = p_int(lower = 2, upper = 10),
  ranger.base.learner.min.node.size = p_int(lower = 1, upper = 20)
)

message("STEP 3: Starting holistic tuning on the data subset. This will take a while...")

# --- Step 3: Configure and run the AutoTuner ---
at_fast = AutoTuner$new(
  learner = stacked_learner,
  resampling = rsmp("cv", folds = 3),      # Outer 3-fold CV for robust performance estimate
  measure = msr("classif.logloss"),
  search_space = search_space_stack,
  terminator = trm("evals", n_evals = 50), # Try 50 different hyperparameter sets
  tuner = tnr("mbo")             # Random search is fast and effective
)

# This single line runs the entire tuning process on the 15k-row subset
at_fast$train(task_for_tuning)

message("Tuning complete.")
print("Best found hyperparameters and performance:")
print(at_fast$tuning_result)
saveRDS(at_fast$tuning_result, file = "result/tuning_result_stacking.rds")

message("STEP 4: Training the final model on the FULL dataset using the best found hyperparameters...")

# --- Step 4: Extract best parameters and set them on the original learner ---
best_params <- at_fast$tuning_result$learner_param_vals

message("... training final Stacking model.")
stacked_learner_final <- stacked_learner$clone(deep = TRUE)
stacked_learner_final$param_set$values <- mlr3misc::insert_named(
  stacked_learner_final$param_set$values,
  best_params
)
stacked_learner_final$train(task)
predictions_M <- stacked_learner_final$predict_newdata(test_imp)

saveRDS(predictions_M, file = "result/predictions_multinom_enhance.rds")
saveRDS(stacked_learner_final, file = "result/stack_multinom_enhance.rds")
message("Model (Multinom Super Learner) predictions saved.")

message("... training final standalone XGBoost model.")
lrn_xgb_final <- lrn_xgb$clone(deep = TRUE)
xgb_params <- best_params[grepl("^xgboost\\.base", names(best_params))]
names(xgb_params) <- sub("^xgboost\\.base\\.", "", names(xgb_params))
lrn_xgb_final$param_set$values <- mlr3misc::insert_named(
  lrn_xgb_final$param_set$values,
  xgb_params
)
lrn_xgb_final$train(task)
preds_xgb_final <- lrn_xgb_final$predict_newdata(test_imp)

message("... training final standalone Ranger model.")
lrn_ranger_final <- lrn_ranger$clone(deep = TRUE)
ranger_params <- best_params[grepl("^ranger\\.base", names(best_params))]
names(ranger_params) <- sub("^ranger\\.base\\.", "", names(ranger_params))
lrn_ranger_final$param_set$values <- mlr3misc::insert_named(
  lrn_ranger_final$param_set$values,
  ranger_params
)
lrn_ranger_final$train(task)
preds_ranger_final <- lrn_ranger_final$predict_newdata(test_imp)

# --- BLENDING: FIND WEIGHTS & PREDICT ----
message("Executing Blending...")
cv5 <- rsmp("cv", folds = 5)
rr_xgb_final <- resample(task, lrn_xgb_final, cv5, store_models = FALSE)
rr_ranger_final <- resample(task, lrn_ranger_final, cv5, store_models = FALSE)

preds_xgb_oof <- rr_xgb_final$prediction()$prob[order(rr_xgb_final$prediction()$row_id), ]
preds_ranger_oof <- rr_ranger_final$prediction()$prob[order(rr_ranger_final$prediction()$row_id), ]

message("... searching for optimal blending weight 'w'.")
w_values <- seq(0, 1, by = 0.01)
logloss_scores <- numeric(length(w_values))
truth_sorted <- rr_xgb_final$prediction()$truth[order(rr_xgb_final$prediction()$row_id)]
true_matrix <- model.matrix(~ truth_sorted - 1)

for (i in seq_along(w_values)) {
  w <- w_values[i]
  blended_preds_oof <- w * preds_xgb_oof + (1 - w) * preds_ranger_oof
  logloss_scores[i] <- score(y = true_matrix, phat = blended_preds_oof)
}
best_w <- w_values[which.min(logloss_scores)] 
message(paste("... optimal weight for XGBoost (w) is:", round(best_w, 3)))

# --- Apply best weight to test set predictions ---
blended_preds_test_prob <- best_w * preds_xgb_final$prob + (1 - best_w) * preds_ranger_final$prob
blended_response <- colnames(blended_preds_test_prob)[apply(blended_preds_test_prob, 1, which.max)]

# --- Generate Submission File from Stacking Model ---
message("Generating submission from the stacking model (Multinom super learner)...")
submission_xgb <- data.frame(
  id           = test_all$id,
  status_group = preds_xgb_final$response,
  stringsAsFactors = FALSE
)
write.csv(submission_xgb, "result/submission_xgb.csv", row.names = FALSE)
submission_ranger <- data.frame(
  id           = test_all$id,
  status_group = preds_ranger_final$response,
  stringsAsFactors = FALSE
)
write.csv(submission_ranger, "result/submission_ranger.csv", row.names = FALSE)
submission_stack <- data.frame(
  id           = test_all$id,
  status_group = predictions_M$response,
  stringsAsFactors = FALSE
)
write.csv(submission_stack, "result/submission_stack.csv", row.names = FALSE)
submission_blend <- data.frame(
  id           = test_all$id,
  status_group = blended_response,
  stringsAsFactors = FALSE
)
write.csv(submission_blend, "result/submission_blend.csv", row.names = FALSE)
message("Method M submission file created.")
message("All 4 submission files (xgb, ranger, blending, stacking) created successfully in 'result/' folder.")


