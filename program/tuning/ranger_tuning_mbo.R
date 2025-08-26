###contributed by: Gelan Ye, Haoran Ju

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
library(future.apply)

plan(multisession, workers = 16)
set.seed(7832)

# === Daten laden ===
data_imputed_enhance <- readRDS("data/data_imputed_enhanced.rds")
train <- data_imputed_enhance %>% filter(!is.na(status_group))

# === Task definieren ===
task_imp <- TaskClassif$new(
  id = "Waterpoints",
  backend = train,
  target = "status_group"
)

# === Pipeline bauen ===
graph <- po("colapply",
            applicator = function(x) if (is.character(x)) as.factor(x) else x,
            affect_columns = selector_type("character")) %>>%
  lrn("classif.ranger",
      predict_type = "prob",
      importance = "impurity",
      num.threads = 7
  )

glrn <- GraphLearner$new(graph)

# === Suchraum für Tuning ===
param_set <- ps(
  classif.ranger.num.trees     = p_int(1000, 1400),
  classif.ranger.mtry          = p_int(lower = 3, upper = 4),
  classif.ranger.max.depth     = p_int(60, 90),
  classif.ranger.min.node.size = p_int(6, 12),
  classif.ranger.sample.fraction = p_dbl(0.94, 0.99)
)

# === AutoTuner ===
at_ranger <- AutoTuner$new(
  learner     = glrn,
  resampling  = rsmp("cv", folds = 3),
  measure     = msr("classif.acc"),
  terminator  = trm("evals", n_evals = 500),
  tuner       = tnr("mbo"),
  search_space = param_set
)

at_ranger$train(task_imp)

best_params <- at_ranger$learner$param_set$values
saveRDS(best_params, file = "result/ranger_tuning_params_new.rds")
saveRDS(at_ranger$learner, file = "result/ranger_tuning_model_l.rds")
saveRDS(at_ranger$archive, file = "result/ranger_tuning_archive_l.rds")

archive <- readRDS("result/ranger_tuning_archive_l.rds")
dt <- as.data.table(archive)
top10 <- dt[order(-classif.acc)][1:10]

top10_params <- top10[, archive$search_space$ids(), with = FALSE]

saveRDS(top10_params, "result/top10_ranger_params.rds")
