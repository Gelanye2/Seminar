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
plan(multisession, workers = 13)
future::plan(future.seed = TRUE)
set.seed(7832)

# fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("seminar/data/data_imputed_enhanced.rds")
#sub_imputed <- readRDS("data/sub_imputed.rds") # train + test without longtitude and latitude
# test_full_imp <- readRDS("seminar/data/test_full_imp.rds")
test_all <- readRDS("seminar/data/test_all.rds")
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
        nrounds = 480,
        eta = 0.047,
        max_depth = 8,
        eval_metric = "mlogloss",
        nthread = 3
    )
)
lrn_xgb$id <- "xgboost.base"
# 2. Ranger: Tree-models are robust to scaling, but we use a minimal pipeline for consistency
lrn_ranger <- as_learner(
  base_pipeline %>>%
    lrn("classif.ranger",
        predict_type = "prob",
        mtry = 3, 
        min.node.size = 5,
        num.threads = 3
    )
)
lrn_ranger$id <- "ranger.base"
base_learners <- list(lrn_xgb, lrn_ranger)

# --- MODEL : Stacking with Multinom Super Learner ---
message("Defining Model with Multinom super learner...")
# Multinom is a linear model, providing diversity to the tree-based Ranger
lrn_super_multinom <- lrn("classif.multinom", predict_type = "prob", MaxNWts = 5000)

stacked_learner_M <- as_learner(
  ppl("stacking",
      base_learners = base_learners, # Re-using the same base learners
      super_learner = lrn_super_multinom,
      method = "cv", folds = 5, use_features = FALSE
  ), id = "stack_multinom_super"
)

message("Training Model Multinom...")
stacked_learner_M$train(task)
message("Predicting with Model Multinom...")
predictions_M <- stacked_learner_M$predict_newdata(newdata = test_imp)
saveRDS(predictions_M, file = "seminar/result/predictions_multinom_enhance.rds")
saveRDS(stacked_learner_M, file = "seminar/result/stack_multinom_enhance.rds")
message("Model (Multinom Super Learner) predictions saved.")

submission_stack <- data.frame(
  id           = test_all$id,
  status_group = predictions_M$response,
  stringsAsFactors = FALSE
)
write.csv(submission_stack, "seminar/result/submission_stack.csv", row.names = FALSE)
message("Method M submission file created.")

# --- TRAIN & PREDICT: STANDALONE MODELS (XGBoost & Ranger) ----
message("Training base learners via resampling to get OOF predictions...")
cv5 <- rsmp("cv", folds = 5)
rr_xgb <- resample(task, lrn_xgb, cv5, store_models = FALSE)
rr_ranger <- resample(task, lrn_ranger, cv5, store_models = FALSE)

preds_xgb_oof <- rr_xgb$prediction()$prob[order(rr_xgb$prediction()$row_ids), ]
preds_ranger_oof <- rr_ranger$prediction()$prob[order(rr_ranger$prediction()$row_ids), ]
true_labels <- task$truth()

message("Training base learners on FULL data to get test predictions...")
lrn_xgb$train(task)
preds_xgb_test <- lrn_xgb$predict_newdata(test_imp)

lrn_ranger$train(task)
preds_ranger_test <- lrn_ranger$predict_newdata(test_imp)

# --- BLENDING: FIND WEIGHTS & PREDICT ----
message("Finding optimal blending weight...")
w_values <- seq(0, 1, by = 0.01)
logloss_scores <- map_dbl(w_values, function(w) {
  blended_preds_oof <- w * preds_xgb_oof + (1 - w) * preds_ranger_oof
  logloss(truth = true_labels, prob = blended_preds_oof)
})
best_w <- w_values[which.min(logloss_scores)]
message(paste("... optimal weight for XGBoost (w) is:", round(best_w, 3)))

blended_preds_test_prob <- best_w * preds_xgb_test$prob + (1 - best_w) * preds_ranger_test$prob
blended_response <- colnames(blended_preds_test_prob)[apply(blended_preds_test_prob, 1, which.max)]

# --- Generate Submission File from Stacking Model ---
message("Generating submission from the stacking model (Multinom super learner)...")
submission_xgb <- data.frame(
  id           = test_all$id,
  status_group = preds_xgb_test$response,
  stringsAsFactors = FALSE
)
write.csv(submission_xgb, "seminar/result/submission_xgb.csv", row.names = FALSE)
submission_ranger <- data.frame(
  id           = test_all$id,
  status_group = preds_ranger_test$response,
  stringsAsFactors = FALSE
)
write.csv(submission_ranger, "seminar/result/submission_ranger.csv", row.names = FALSE)
submission_blend <- data.frame(
  id           = test_all$id,
  status_group = blended_response,
  stringsAsFactors = FALSE
)
write.csv(submission_blend, "seminar/result/submission_blend.csv", row.names = FALSE)
message("All 4 submission files (xgb, ranger, blending, stacking) created successfully in 'result/' folder.")

