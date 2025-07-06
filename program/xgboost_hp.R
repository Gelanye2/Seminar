# source("setup.R")
library(dplyr)
library(mlr3)          # TaskClassif, msr(), rsmp(), mlr3::resample()
library(mlr3pipelines) # po(), %>>%, selector_type(), GraphLearner
library(mlr3learners)  # lrn("classif.xgboost")
library(mlr3tuning) 
library(paradox)
library(future)

plan(multisession)
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
      id               = "xgb", 
      predict_type     = "prob",
      nrounds          = 2000L,
      early_stopping_rounds = 50 
  )

base_lrn <- GraphLearner$new(graph)
set_validate(base_lrn, learner_id = "xgb", validate = "predefined")

ps <- ps(
  "xgb.eta"             = p_dbl(0.1, 0.2, logscale = TRUE),
  "xgb.max_depth"       = p_int(6, 12),
  "xgb.subsample"       = p_dbl(0.5, 1),
  "xgb.colsample_bytree"= p_dbl(0.5, 1),
  "xgb.lambda"          = p_dbl(0.1, 5, logscale = TRUE),
  "xgb.alpha"           = p_dbl(0.1, 5, logscale = TRUE)
)

auto <- AutoTuner$new(
  learner      = base_lrn,
  resampling   = rsmp("cv", folds = 5), 
  measure      = msr("classif.acc"),
  search_space = ps,
  tuner        = tnr("mbo"),            
  terminator   = trm("evals", n_evals = 100)
)

rr <- mlr3::resample(task_ll, auto, rsmp("cv", folds = 5))
acc  <- rr$aggregate(msr("classif.acc"))
bacc <- rr$aggregate(msr("classif.bacc"))

best_vals <- auto$learner$model$param_set$values
print(best_vals)

result_list <- list(
  accuracy          = acc,
  balanced_accuracy = bacc,
  best_params       = best_vals
)

saveRDS(result_list, file = "data/xgb_tuning_results.rds")
