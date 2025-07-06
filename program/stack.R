# 0. SETUP AND DATA LOADING
# ==================================
# Ensure required packages are loaded
library(mlr3verse)
library(mlr3pipelines)
library(mlr3learners)
library(dplyr)
library(future)
plan(multisession, workers = 16)
set.seed(7832)

fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed.rds") # all:train + test
#sub_imputed <- readRDS("data/sub_imputed.rds") # train + test without longtitude and latitude
test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")

# Assumes these data objects are loaded in your environment:
# imputed, test_full_imp, test_all

# 1. TASK DEFINITION
# =================================
# Use the imputed dataset for consistency
df_imp <- imputed
train_imp <- df_imp %>% filter(!is.na(status_group))
test_imp  <- test_full_imp

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
base_pipeline <- po("colapply",
                    applicator = function(x) if (is.character(x)) as.factor(x) else x,
                    affect_columns = selector_type("character")
) %>>%
  po("removeconstants") %>>%
  po("encode", method = "treatment")

# Pipeline with scaling for distance-based models (kknn, svm)
scale_pipeline <- base_pipeline %>>% po("scale")

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
        nthread = 4
    )
)

# 2. Ranger: Tree-models are robust to scaling, but we use a minimal pipeline for consistency
lrn_ranger <- as_learner(
  po("colapply", # Ranger handles factors well, so only this step is needed
     applicator = function(x) if (is.character(x)) as.factor(x) else x,
     affect_columns = selector_type("character")
  ) %>>%
    lrn("classif.ranger",
        predict_type = "prob",
        num.trees = 500,
        mtry = floor(sqrt(ncol(train_imp) - 1)), # ncol-1 for target
        min.node.size = 1
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
# ===================================
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

# Review aggregated results to select the best model
print(bmr$aggregate(msrs(c("classif.acc", "classif.bacc"))))
# 0.8072171    0.6556998

# 4. FINAL MODEL TRAINING AND PREDICTION
# ==============================
# Select the best performing learner based on benchmark results.
# We will proceed with the first one (log_reg) by default.
final_learner <- stacked_learners[[1]]

print(paste("Final model selected:", final_learner$id))
print("Training final stacking model on full training data...")

final_learner$train(task)

print("Final model training complete.")
print("Predicting on the test set...")

# Create the test task
task_test <- TaskClassif$new(
  id      = "waterpoints_test",
  backend = test_imp,
  target  = "status_group"
)

# Generate predictions
final_predictions <- final_learner$predict(task_test)

print("Prediction complete.")

# 5. GENERATE SUBMISSION FILE
# ======================
result_stacking <- data.frame(
  id           = test_all$id,
  status_group = final_predictions$response,
  stringsAsFactors = FALSE
)

# Save results
saveRDS(result_stacking, "data/predictions_stacking.rds")
write.csv(result_stacking, "data/submission_stacking.csv", row.names = FALSE)

print("Submission file 'submission_stacking.csv' has been generated.")
