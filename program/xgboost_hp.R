# source("setup.R")
.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
library(dplyr)
library(mlr3)          # TaskClassif, msr(), rsmp(), mlr3::resample()
library(mlr3pipelines) # po(), %>>%, selector_type(), GraphLearner
library(mlr3learners)  # lrn("classif.xgboost")
library(mlr3tuning)
library(paradox)
library(future)
library(parallel)
library(mlr3mbo)
workers <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1"))
print(workers)
plan(multisession, workers = 32)
set.seed(7832)
fi_clean <- readRDS("data/data_imputed.rds")

# ---- Hyperparameter tuning (region_district and lga)----
# eta 1e-4, 1  Logscale
#
# nrounds 1, 5000
#
# max_depth 1, 20
#
# colsample_bytree 0.1 1
#
# colsample_bylevel 0.1 1
#
# lambda  Logscale 0.001 1000
#
# alpha  Logscale 0.001 1000
#
# subsample 0.1 1
df_ll <- fi_clean %>%
  select_if(~ length(unique(.[!is.na(.)])) > 1)

train_ll <- df_ll %>% filter(!is.na(status_group))
test_ll  <- df_ll %>% filter( is.na(status_group))

# mlr3 task on the training set
task_ll <- TaskClassif$new(
  id      = "ll",
  backend = train_ll,
  target  = "status_group"
)

task_ll$col_roles$stratum <- task_ll$target_names

graph <- po("colapply", param_vals = list(
  applicator     = function(x) if (is.character(x)) as.factor(x) else x,
  affect_columns = selector_type("character")
)) %>>%
  po("removeconstants") %>>%
  po("encode", method = "treatment") %>>%
  lrn("classif.xgboost",
      predict_type = "prob",
      nthread = 8,
      eval_metric = "mlogloss",
      early_stopping_rounds = 20,
      nrounds = to_tune(upper = 1000, internal = TRUE),
      eta = to_tune(0.03, 0.06, logscale = TRUE),
      max_depth = to_tune(4,8),
      colsample_bytree = to_tune(0.7, 0.9),
      subsample = to_tune(0.7, 0.9),
      lambda = to_tune(1, 5, logscale = TRUE),
      alpha  = to_tune(0.01, 2, logscale = TRUE),
      gamma = to_tune(0, 5),
      min_child_weight = to_tune(1, 10)
  )

base_lrn <- GraphLearner$new(graph)

set_validate(base_lrn, validate = "test", ids = "classif.xgboost")

auto <- AutoTuner$new(
  learner = base_lrn,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.acc"),
  tuner = tnr("mbo"),
  terminator = trm("evals", n_evals = 100)
)

auto$train(task_ll)

rr <- mlr3::resample(task_ll, auto, rsmp("cv", folds = 3))
acc  <- rr$aggregate(msr("classif.acc"))
bacc <- rr$aggregate(msr("classif.bacc"))
best_params <- auto$learner$param_set$values

result_list <- list(
  acc                = acc,
  bacc               = bacc,
  best_params        = best_params
)

saveRDS(result_list, file = "result/xgb_tuning_results.rds")
saveRDS(auto$learner, file = "result/xgb_tuning_model.rds")
saveRDS(auto$archive, file = "result/xgb_tuning_archive.rds")

