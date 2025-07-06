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
workers <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1"))
plan(multicore, workers = workers)
fi_clean <- readRDS("data/fi_clean.rds")

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
  select(-region, -district_code, -subvillage, -ward,
         -lga, -region_code, -longitude, -latitude) %>%
  select_if(~ length(unique(.[!is.na(.)])) > 1)

train_ll <- df_ll %>% filter(!is.na(status_group))
test_ll  <- df_ll %>% filter( is.na(status_group))

# mlr3 task on the training set
task_ll <- TaskClassif$new(
  id      = "ll",
  backend = train_ll,
  target  = "status_group"
)

graph <- po("colapply",  # char -> factor
            param_vals = list(
              applicator     = function(x) if (is.character(x)) as.factor(x) else x,
              affect_columns = selector_type("character")
            )) %>>%
  po("removeconstants") %>>%
  po("encode", method = "treatment") %>>%
  lrn("classif.xgboost",
      predict_type          = "response",
      nrounds = 500)

base_lrn <- GraphLearner$new(graph)

ps <- ps(
  "classif.xgboost.eta"             = p_dbl(0.1, 0.2, logscale = TRUE),
  "classif.xgboost.nrounds"         = p_int(100, 500),
  "classif.xgboost.max_depth"       = p_int(6, 12),
  "classif.xgboost.subsample"       = p_dbl(0.9, 1),
  "classif.xgboost.colsample_bytree"= p_dbl(0.9, 1),
  "classif.xgboost.lambda"          = p_dbl(0.1, 5, logscale = TRUE),
  "classif.xgboost.alpha"           = p_dbl(0.1, 5, logscale = TRUE)
)

auto <- AutoTuner$new(
  learner      = base_lrn,
  resampling   = rsmp("cv", folds = 3),
  measure      = msr("classif.acc"),
  search_space = ps,
  tuner        = tnr("random_search"),
  terminator   = trm("evals", n_evals = 30)
)

rr <- mlr3::resample(task_ll, auto, rsmp("cv", folds = 3))
acc  <- rr$aggregate(msr("classif.acc"))
bacc <- rr$aggregate(msr("classif.bacc"))

best_vals <- auto$learner$model$param_set$values
print(best_vals)

result_list <- list(
  accuracy          = acc,
  balanced_accuracy = bacc,
  best_params       = best_vals
)

saveRDS(result_list, file = "../result/xgb_tuning_results.rds")

