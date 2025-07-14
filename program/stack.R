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
plan(multisession, workers = 8)
future::plan(future.seed = TRUE)
set.seed(7832)

# fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed_scheme.rds")
#sub_imputed <- readRDS("data/sub_imputed.rds") # train + test without longtitude and latitude
# test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")
log_params <- data.table(
  nrounds          = 482L,
  eta_log        = -3.007022,
  alpha_log      = -3.815309,
  lambda_log     =  0.2757714,
  colsample_bytree = 0.7244709,
  gamma            = 0.09409377,
  max_depth        = 13L,
  min_child_weight = 1.109858,
  subsample        = 0.8931599
)

param.set <- copy(log_params)[, `:=`(
  eta    = exp(eta_log),
  alpha  = exp(alpha_log),
  lambda = exp(lambda_log)
)][, c("eta_log", "alpha_log", "lambda_log") := NULL]

# 1. TASK DEFINITION
# Use the imputed dataset for consistency
train_imp <- imputed %>% filter(!is.na(status_group))
test_imp  <- imputed %>% filter(is.na(status_group))

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

base_pipeline <- ppl("greplicate",
                     po_latlon_na %>>%
                       po_latlon_flag %>>%
                       po_latlon_impute %>>%
                       po_char2fac %>>%
                     po("removeconstants") %>>%
                       po("encode", param_vals = list(method = "treatment")),
                     1
)
# --- Create Learner Instances ---
# 1. XGBoost: Uses base pipeline
lrn_xgb <- as_learner(
  base_pipeline %>>%
    lrn("classif.xgboost",
        predict_type = "prob",
        nrounds = param.set$nrounds,
        eta = param.set$eta,
        max_depth = param.set$max_depth,
        subsample = param.set$subsample,
        colsample_bytree = param.set$colsample_bytree,
        alpha = param.set$alpha,
        lambda = param.set$lambda,
        gamma = param.set$gamma,
        min_child_weight = param.set$min_child_weight,
        nthread = 8
    )
)
lrn_xgb$id <- "xgboost.base"
# 2. Ranger: Tree-models are robust to scaling, but we use a minimal pipeline for consistency
lrn_ranger <- as_learner(
  base_pipeline %>>%
    lrn("classif.ranger",
        predict_type = "prob",
        num.trees = 1311,
        mtry = 4, 
        min.node.size = 7,
        max.depth = 48,
        num.threads = 8
    )
)
lrn_ranger$id <- "ranger.base"
base_learners <- list(lrn_xgb, lrn_ranger)

# --- MODEL A: Stacking with Ranger Super Learner ---
message("Defining Model A with Ranger super learner...")
lrn_super_ranger <- lrn("classif.ranger", id = "ranger_super", predict_type = "prob", num.threads = 8)

stacked_learner_A <- as_learner(
  ppl("stacking",
      base_learners = base_learners,
      super_learner = lrn_super_ranger,
      method = "cv", folds = 5, use_features = FALSE
  ), id = "stack_ranger_super"
)

message("Training Model A...")
stacked_learner_A$train(task)
message("Predicting with Model A...")
predictions_A <- stacked_learner_A$predict_newdata(newdata = test_imp)
saveRDS(predictions_A, file = "result/predictions_A_ranger_scheme.rds")
saveRDS(stacked_learner_A, file = "result/model_A_stack_ranger_scheme.rds")
message("Model A (Ranger Super Learner) predictions saved.")

# --- Method A: Direct result from the best single model (assuming Model A) ---
message("Generating submission for Method A (direct result from Ranger super learner)...")
submission_method_A <- data.frame(
  id           = test_all$id,
  status_group = predictions_A$response,
  stringsAsFactors = FALSE
)
write.csv(submission_method_A, "result/submission_method_A_scheme.csv", row.names = FALSE)
message("Method A submission file created.")

# --- MODEL B: Stacking with Multinom Super Learner ---
message("Defining Model B with Multinom super learner...")
# Multinom is a linear model, providing diversity to the tree-based Ranger
lrn_super_multinom <- lrn("classif.multinom", predict_type = "prob", MaxNWts = 5000)

stacked_learner_B <- as_learner(
  ppl("stacking",
      base_learners = base_learners, # Re-using the same base learners
      super_learner = lrn_super_multinom,
      method = "cv", folds = 5, use_features = TRUE
  ), id = "stack_multinom_super"
)

message("Training Model B...")
stacked_learner_B$train(task)
message("Predicting with Model B...")
predictions_B <- stacked_learner_B$predict_newdata(newdata = test_imp)
saveRDS(predictions_B, file = "result/predictions_B_multinom_scheme.rds")
saveRDS(stacked_learner_B, file = "result/model_B_stack_multinom_scheme.rds")
message("Model B (Multinom Super Learner) predictions saved.")

# --- Method B: Direct result from the best single model (assuming Model A) ---
message("Generating submission for Method B (direct result from Multinom super learner)...")
submission_method_B <- data.frame(
  id           = test_all$id,
  status_group = predictions_B$response,
  stringsAsFactors = FALSE
)
write.csv(submission_method_B, "result/submission_method_B_scheme.csv", row.names = FALSE)
message("Method B submission file created.")


# --- Method C: Blending the predictions from Model A and Model B ---
message("Generating submission for Method (blending Ranger and Multinom super learners)...")

# Average the probabilities
blended_prob <- (predictions_A$prob + predictions_B$prob) / 2

# For each row, find the class with the highest average probability
final_blended_response <- colnames(blended_prob)[apply(blended_prob, 1, which.max)]

# Create the blended submission dataframe
submission_method_blended <- data.frame(
  id           = test_all$id,
  status_group = final_blended_response,
  stringsAsFactors = FALSE
)
write.csv(submission_method_blended, "result/submission_blended_scheme.csv", row.names = FALSE)
message("Method (blended) submission file created.")
message("All processes finished successfully.")

