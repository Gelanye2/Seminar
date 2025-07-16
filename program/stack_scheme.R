# 0. SETUP AND DATA LOADING
# ==================================
# Ensure required packages are loaded
library(mlr3)
library(mlr3pipelines)
library(mlr3learners)
library(dplyr)
library(future)
plan(multisession, workers = 8)
future::plan(future.seed = TRUE)
set.seed(7832)

# fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/imputed_sh.rds") # all:train + test
#sub_imputed <- readRDS("data/sub_imputed.rds") # train + test without longtitude and latitude
# test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")

# Assumes these data objects are loaded in your environment:
# imputed, test_full_imp, test_all

# 1. TASK DEFINITION
# =================================
# Use the imputed dataset for consistency
train_imp <- imputed %>% filter(!is.na(status_group))
test_imp  <- imputed %>% filter(is.na(status_group))

# Ensure target is a factor for classification
train_imp$status_group <- as.factor(train_imp$status_group)

# Create the training task
task <- TaskClassif$new(
  id      = "waterpoints_stacking",
  backend = train_imp,
  target  = "status_group"
)

# 2. DEFINE BASE LEARNERS
# ===================================
# IMPORTANT: All base learners must have predict_type = "prob"

# --- Preprocessing Pipelines ---
# Base pipeline: character to factor -> remove constants -> encode
# --- Create Learner Instances ---
# 1. XGBoost: Uses base pipeline
lrn_xgb <- as_learner(
  po("encode", method = "treatment") %>>%
    lrn("classif.xgboost",
        predict_type = "prob",
        nrounds = 1000L,
        eta = 0.1,
        max_depth = 6,
        subsample = 0.8,
        colsample_bytree = 0.8,
        nthread = 1
    )
)

# 2. Ranger: Tree-models are robust to scaling, but we use a minimal pipeline for consistency
lrn_ranger <- as_learner(
  # po("colapply", # Ranger handles factors well, so only this step is needed
  #    applicator = function(x) if (is.character(x)) as.factor(x) else x,
  #    affect_columns = selector_type("character")
  # ) %>>%
    lrn("classif.ranger",
        predict_type = "prob",
        num.trees = 500,
        mtry = floor(sqrt(ncol(train_imp) - 1)), # ncol-1 for target
        min.node.size = 1,
        num.threads = 1
    )
)


# Consolidate base learners into a list
base_learners <- list(lrn_xgb, lrn_ranger)

# 3. DEFINE AND EVALUATE STACKING MODELS
# ===================================
# Define candidate super learners
super_learner_multinom <- lrn("classif.multinom", id = "multinom", predict_type = "prob")

# 2. Create the final stacking learner directly
final_learner <- as_learner(
  ppl("stacking",
      base_learners = base_learners,      
      super_learner = super_learner_multinom,
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
write.csv(result_stacking, "seminar/data/submission_stacking_multinom.csv", row.names = FALSE)
print("Submission file 'submission_stacking_multinom.csv' has been generated.")
