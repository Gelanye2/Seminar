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
plan(multisession, workers = 16)
set.seed(7832)
df <- readRDS("data/data_imputed.rds")

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
df_ll <- df %>%
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

graph <- po_latlon_na %>>%
  po_latlon_flag %>>%
  po_latlon_impute %>>%
  po_char2fac %>>%
  po("removeconstants") %>>%
  po("encode", method = "treatment") %>>%
  lrn("classif.xgboost",
      predict_type = "prob",
      nthread = 8,
      eval_metric = "mlogloss",
      early_stopping_rounds = 10,
      nrounds = to_tune(upper = 100, internal = TRUE),
      eta = to_tune(0.1,0.2, logscale = TRUE),
      max_depth = to_tune(5,15),
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
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.acc"),
  tuner = tnr("mbo"),
  terminator = trm("evals", n_evals = 50)
)

auto$train(task_ll)

best_params <- auto$learner$param_set$values

saveRDS(best_params, file = "result/xgb_tuning_params.rds")
saveRDS(auto$learner, file = "result/xgb_tuning_model.rds")
saveRDS(auto$archive, file = "result/xgb_tuning_archive.rds")


