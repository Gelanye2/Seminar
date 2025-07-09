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

# fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed.rds") # all:train + test
#sub_imputed <- readRDS("data/sub_imputed.rds") # train + test without longtitude and latitude
test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")

# 1. TASK DEFINITION
# Use the imputed dataset for consistency
df_imp <- imputed
train_imp <- df_imp %>% filter(!is.na(status_group))
test_imp  <- test_full_imp

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
  v[abs(v) < thr] <- NA_real_
  v
}

po_latlon_na <- po("colapply", id = "latlon_to_na",
                   param_vals = list(
                     applicator   = latlon_to_na,
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
                         constant       = -999 # Use a distinct constant for imputation
                       ))

po_char2fac <- po("colapply",
                  applicator = function(x) if (is.character(x)) as.factor(x) else x,
                  affect_columns = selector_type("character"))

base_core <- po_char2fac %>>%
  po("removeconstants") %>>%
  po("encode", method = "treatment")

# Combine all preprocessing steps into a single graph
base_pipeline <- po_latlon_na %>>%
  po_latlon_flag %>>%
  po_latlon_impute %>>%
  base_core


# Pipeline with scaling for distance-based models (kknn, svm)
# scale_pipeline <- base_pipeline %>>% po("scale")

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
        nthread = 8
    )
)

# 2. Ranger: Tree-models are robust to scaling, but we use a minimal pipeline for consistency
lrn_ranger <- as_learner(
  base_pipeline %>>%
    lrn("classif.ranger",
        predict_type = "prob",
        num.trees = 500,
        mtry = floor(sqrt(ncol(train_imp) - 1)), # ncol-1 for target
        min.node.size = 1,
        num.threads = 8
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

print("Benchmark Aggregated Results:")
print(bmr$aggregate(msrs(c("classif.acc", "classif.bacc"))))
# 0.8072171    0.6556998

# 4. FINAL MODEL TRAINING AND PREDICTION
bmr_aggr <- bmr$aggregate(msrs(c("classif.acc", "classif.bacc")))

# Find the ID of the best learner based on accuracy (classif.acc)
best_learner_id <- bmr_aggr[order(-classif.acc)]$learner_id[1]

# Retrieve the actual learner object from the benchmark design
final_learner <- bmr$learners[learner_id == best_learner_id][[1]]

print(paste("Final model selected based on benchmark:", final_learner$id))
print("Training final stacking model on full training data...")

final_learner$train(task)

print("Final model training complete.")
print("Predicting on the test set...")
final_predictions <- final_learner$predict_newdata(newdata = test_imp)
print("Prediction complete.")

# 5. GENERATE SUBMISSION FILE
result_stacking <- data.frame(
  id           = test_all$id,
  status_group = final_predictions$response,
  stringsAsFactors = FALSE
)

# Save results
saveRDS(result_stacking, "data/predictions_stacking.rds")
write.csv(result_stacking, "data/submission_stacking.csv", row.names = FALSE)

print("Submission file 'submission_stacking.csv' has been generated.")
