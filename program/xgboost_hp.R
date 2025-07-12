source("setup.R")

library(dplyr)
library(mlr3)          # TaskClassif, msr(), rsmp(), mlr3::resample()
library(mlr3pipelines) # po(), %>>%, selector_type(), GraphLearner
library(mlr3learners)  # lrn("classif.xgboost")
library(mlr3tuning)
library(paradox)
library(future)
library(parallel)

library(future.apply)
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

train_ll$status_group <- as.factor(train_ll$status_group)
test_ll$status_group  <- factor(test_ll$status_group, levels = levels(train_ll$status_group))

# mlr3 task on the training set
task_ll <- TaskClassif$new(
  id      = "ll",
  backend = train_ll,
  target  = "status_group"
)

task_ll$col_roles$stratum <- "status_group"

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
      nthread = 7,
      eval_metric = "mlogloss",
      early_stopping_rounds = 10,
<<<<<<< Updated upstream
      nrounds = to_tune(upper = 500, internal = TRUE),
      eta = to_tune(0.01,0.2, logscale = TRUE),
      max_depth = to_tune(6,14),
      colsample_bytree = to_tune(0.7, 1),
      subsample = to_tune(0.7, 1),
=======
      nrounds = to_tune(upper = 100, internal = TRUE),
      eta = to_tune(0.1,0.3, logscale = TRUE),
      max_depth = to_tune(3,12),
      colsample_bytree = to_tune(0.7, 0.9),
      subsample = to_tune(0.5, 1.0),
>>>>>>> Stashed changes
      lambda = to_tune(1, 5, logscale = TRUE),
      alpha  = to_tune(0.01, 0.5, logscale = TRUE),
      gamma = to_tune(0, 5),
      min_child_weight = to_tune(1,5)
  )

base_lrn <- GraphLearner$new(graph)

auto <- AutoTuner$new(
  learner = base_lrn,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.acc"),
<<<<<<< Updated upstream
  tuner = tnr("mbo"),
  terminator = trm("evals", n_evals = 200)
=======
  tuner = tnr("random_search"),
  terminator = trm("evals", n_evals = 50)
>>>>>>> Stashed changes
)

auto$train(task_ll)

best_params <- auto$learner$param_set$values

<<<<<<< Updated upstream
saveRDS(best_params, file = "result/xgb_tuning_params3.rds")
saveRDS(auto$learner, file = "result/xgb_tuning_model3.rds")
saveRDS(auto$archive, file = "result/xgb_tuning_archive3.rds")
=======
<<<<<<< Updated upstream
saveRDS(best_params, file = "result/xgb_tuning_params2.rds")
saveRDS(auto$learner, file = "result/xgb_tuning_model2.rds")
saveRDS(auto$archive, file = "result/xgb_tuning_archive2.rds")

archive <-readRDS("result/xgb_tuning_archive2.rds")
dt <- as.data.table(archive)
top10 <- dt[order(-classif.acc)][1:10]
top10_params <- top10[, archive$search_space$ids(), with = FALSE]

=======
saveRDS(best_params, file = "result/xgb_tuning_params.rds")
saveRDS(auto$learner, file = "result/xgb_tuning_model.rds")
saveRDS(auto$archive, file = "result/xgb_tuning_archive.rds")
>>>>>>> Stashed changes
>>>>>>> Stashed changes
