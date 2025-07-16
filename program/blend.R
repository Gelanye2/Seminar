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
plan(multisession, workers = 16)
future::plan(future.seed = TRUE)
set.seed(7832)

# fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed_enhanced.rds")
#sub_imputed <- readRDS("data/sub_imputed.rds") # train + test without longtitude and latitude
# test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")
log_params <- data.table(
  nrounds          = 385L,
  eta_log        = 0.02869972,
  alpha_log      = 0.07850012,
  lambda_log     =  1.276562,
  colsample_bytree = 0.745822,
  gamma            = 0.2551381,
  max_depth        = 16L,
  min_child_weight = 1.581652,
  subsample        = 0.9090084
)

# param.set <- copy(log_params)[, `:=`(
#   eta    = exp(eta_log),
#   alpha  = exp(alpha_log),
#   lambda = exp(lambda_log)
# )][, c("eta_log", "alpha_log", "lambda_log") := NULL]

param.set <- copy(log_params)[, `:=`(
  eta    = eta_log,
  alpha  = alpha_log,
  lambda = lambda_log
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
        eval_metric = "mlogloss",
        nthread = 16
    )
)
lrn_xgb$id <- "xgboost.base"
# 2. Ranger: Tree-models are robust to scaling, but we use a minimal pipeline for consistency
lrn_ranger <- as_learner(
  base_pipeline %>>%
    lrn("classif.ranger",
        predict_type = "prob",
        num.trees = 873,
        mtry = 4, 
        min.node.size = 5,
        max.depth = 85,
        num.threads = 16
    )
)
lrn_ranger$id <- "ranger.base"
base_learners <- list(lrn_xgb, lrn_ranger)

# --- TRAIN & PREDICT: STANDALONE MODELS (XGBoost & Ranger) ----
message("Training standalone XGBoost model...")
lrn_xgb$train(task)
preds_xgb_test <- lrn_xgb$predict_newdata(test_imp)

message("Training standalone Ranger model...")
lrn_ranger$train(task)
preds_ranger_test <- lrn_ranger$predict_newdata(test_imp)

# --- BLENDING: FIND WEIGHTS & PREDICT ----
message("Executing Blending...")
cv5 <- rsmp("cv", folds = 5)
rr_xgb <- resample(task, lrn_xgb, cv5, store_models = TRUE)
rr_ranger <- resample(task, lrn_ranger, cv5, store_models = TRUE)

preds_xgb_oof <- rr_xgb$prediction()$prob[order(rr_xgb$prediction()$row_id), ]
preds_ranger_oof <- rr_ranger$prediction()$prob[order(rr_ranger$prediction()$row_id), ]
true_labels <- task$truth()

message("... searching for optimal blending weight 'w'.")
w_values <- seq(0, 1, by = 0.01)
accuracy_scores <- numeric(length(w_values)) 

class_levels <- task$class_names

for (i in seq_along(w_values)) {
  w <- w_values[i]
  
  blended_preds_oof <- w * preds_xgb_oof + (1 - w) * preds_ranger_oof
  blended_response_oof <- class_levels[apply(blended_preds_oof, 1, which.max)]
  accuracy_scores[i] <- mean(blended_response_oof == true_labels)
}
best_w <- w_values[which.max(accuracy_scores)]
message(paste("... optimal weight for XGBoost (w) is:", round(best_w, 3)))

# --- Apply best weight to test set predictions ---
blended_preds_test_prob <- best_w * preds_xgb_test$prob + (1 - best_w) * preds_ranger_test$prob
blended_response <- colnames(blended_preds_test_prob)[apply(blended_preds_test_prob, 1, which.max)]

# --- Generate Submission File from Blending Model ---
message("Generating submission from the stacking model (Multinom super learner)...")
submission_xgb <- data.frame(
  id           = test_all$id,
  status_group = preds_xgb_test$response,
  stringsAsFactors = FALSE
)
write.csv(submission_xgb, "result/submission_xgb.csv", row.names = FALSE)
submission_ranger <- data.frame(
  id           = test_all$id,
  status_group = preds_ranger_test$response,
  stringsAsFactors = FALSE
)
write.csv(submission_ranger, "result/submission_ranger.csv", row.names = FALSE)
submission_blend <- data.frame(
  id           = test_all$id,
  status_group = blended_response,
  stringsAsFactors = FALSE
)
write.csv(submission_blend, "result/submission_blend.csv", row.names = FALSE)
message("All submission files (xgb, ranger, blending) created successfully.")

