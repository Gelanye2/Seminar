.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
# 0. SETUP AND DATA LOADING
# ==================================
# Ensure required packages are loaded
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3pipelines)
library(future)
library(data.table)
library(dplyr)

plan(multisession, workers = 16)
future::plan(future.seed = TRUE)
set.seed(7832)

# fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed_enhanced.rds")
test_all <- readRDS("data/test_all.rds")

# 1. TASK DEFINITION
# Use the imputed dataset for consistency
train_imp <- imputed %>% filter(!is.na(status_group))
test_imp  <- imputed %>% filter(is.na(status_group))

# Ensure target is a factor for classification
train_imp$status_group <- as.factor(train_imp$status_group)

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

# Create the training task
task <- TaskClassif$new(
  id      = "waterpoints_stacking",
  backend = train_imp,
  target  = "status_group"
)

# --- Create Learner Instances ---
# 1. XGBoost: Uses base pipeline
xgb_base <- as_learner(
  po("encode", method = "treatment") %>>%
    lrn("classif.xgboost",
        predict_type = "prob",
        nrounds = 1000L,
        eta = 0.1,
        max_depth = 6,
        subsample = 0.8,
        colsample_bytree = 0.8,
        nthread = 7
    )
)
xgb_base$id <- "xgboost.base"
# 2. Ranger: Tree-models are robust to scaling, but we use a minimal pipeline for consistency
ranger_base <- as_learner(
  lrn("classif.ranger",
      predict_type = "prob",
      num.trees = 500,
      mtry = floor(sqrt(ncol(train_imp) - 1)), # ncol-1 for target
      min.node.size = 1,
      num.threads = 7
  )
)
ranger_base$id <- "ranger.base"

xgb_tuned <- as_learner(
  po("encode", method = "treatment") %>>%
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
        nthread = 7
    )
)
xgb_tuned$id <- "xgboost.tuned"
# 2. Ranger: Tree-models are robust to scaling, but we use a minimal pipeline for consistency
ranger_tuned <- as_learner(
  lrn("classif.ranger",
      predict_type = "prob",
      num.trees = 1311,
      mtry = 4,
      min.node.size = 7,
      max.depth = 48,
      num.threads = 7
  )
)
ranger_tuned$id <- "ranger.tuned"

catboost_base <- as_learner(lrn("classif.catboost",
                                predict_type = "prob",
                                thread_count = 7))
catboost_base$id <- "catboost.base"


lightgbm_base <- as_learner(
  po("encode") %>>% lrn("classif.lightgbm",
                        predict_type = "prob",
                        num_iterations  = 1000L,
                        learning_rate = 0.1,
                        num_leaves = 31,
                        feature_fraction = 0.8,
                        bagging_fraction = 0.8,
                        bagging_freq = 1,
                        num_threads = 1
  )
)
lightgbm_base$id <- "lightgbm.base"

kknn_base <- as_learner(
  po("encode", method = "treatment") %>>%
    po("scale") %>>%
    lrn("classif.kknn",
        predict_type = "prob",
        k = 21,
        kernel = "rectangular"
    )
)
kknn_base$id <- "kknn.base"

glmnet_base <- as_learner(
  po("encode", method = "treatment") %>>%
    po("scale") %>>%
    lrn("classif.glmnet",
        predict_type = "prob",
        alpha = 1,
        s = 0.01
    )
)
glmnet_base$id <- "glmnet.lasso"

catboost_tuned <- lrn("classif.catboost",
                      predict_type = "prob",
                      thread_count = 7,
                      learning_rate = 0.11548602,
                      depth = 10,
                      l2_leaf_reg = 4.021390,
                      rsm = 0.8162192,
                      random_strength = 16.579515,
                      bagging_temperature = 0.3254960,
                      iterations = 784)
catboost_tuned$id <- "catboost.tuned"


base_learners <- list(xgb_tuned, ranger_base, ranger_tuned,
                      lightgbm_base, kknn_base,catboost_tuned)


# --- MODEL : Stacking with Multinom Super Learner ---
message("Defining Model with Multinom super learner...")
# Multinom is a linear model, providing diversity to the tree-based Ranger
super_multinom <- lrn("classif.multinom", predict_type = "prob", MaxNWts = 5000)

# 2. Create the final stacking learner directly
final_learner <- as_learner(
  ppl("stacking",
      base_learners = base_learners,
      super_learner = super_multinom,
      method = "cv",
      folds = 5,
      use_features = FALSE
  ),
  id = "stack_multinom"
)

# 3. Train the final model on the full training data
print(paste("Training final model:", final_learner$id))
final_learner$train(task) # 'task' is your full training TaskClassif
print("Final model training complete.")

# 4. Predict on the test set
print("Predicting on the test set...")
task_test <- TaskClassif$new(
  id      = "waterpoints_test",
  backend = test_imp,
  target  = "status_group"
)
final_predictions <- final_learner$predict(task_test)
print("Prediction complete.")

# 5. Generate the submission file
result_stacking <- data.frame(
  id           = test_all$id,
  status_group = final_predictions$response,
  stringsAsFactors = FALSE
)
write.csv(result_stacking, "result/submission_stacking_multinom9.csv", row.names = FALSE)
print("Submission file 'submission_stacking_multinom.csv' has been generated.")
saveRDS(final_learner, "result/model_stacking_multinom9.rds")
