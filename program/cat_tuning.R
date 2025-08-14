.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
library(dplyr)
library(mlr3)
library(mlr3pipelines)
library(mlr3learners)
library(mlr3tuning)
library(paradox)
library(future)
library(parallel)
library(mlr3mbo)
library(future.apply)
library(mlr3extralearners)

plan(multisession, workers = 16)
set.seed(7832)

# === Load data ===
df <- readRDS("data/data_imputed_enhanced.rds")
train <- df %>% filter(!is.na(status_group))

# === Create task ===
task_cat <- TaskClassif$new(
  id = "Waterpoints_CatBoost",
  backend = train,
  target = "status_group"
)
task_cat$col_roles$stratum <- task_cat$target_names

# === Build preprocessing pipeline ===
graph <- po("colapply",
            applicator = function(x) if (is.character(x)) as.factor(x) else x,
            affect_columns = selector_type("character")) %>>%
  lrn("classif.catboost",
      predict_type = "prob",
      loss_function_multiclass = "MultiClass",
      thread_count = 7
  )

glrn <- GraphLearner$new(graph)

# === Define tuning space ===
param_set <- ps(
  classif.catboost.learning_rate = p_dbl(0.01, 0.2),
  classif.catboost.depth = p_int(4, 10),
  classif.catboost.l2_leaf_reg = p_dbl(3, 10),
  classif.catboost.rsm = p_dbl(0.8, 1),
  classif.catboost.random_strength = p_dbl(1,20),
  classif.catboost.bagging_temperature = p_dbl(0, 1),
  classif.catboost.iterations = p_int(300, 800)
)

# === AutoTuner ===
at_catboost <- AutoTuner$new(
  learner     = glrn,
  resampling  = rsmp("cv", folds = 3),
  measure     = msr("classif.acc"),
  terminator  = trm("evals", n_evals = 100),
  tuner       = tnr("mbo"),
  search_space = param_set
)

at_catboost$train(task_cat)

# === Save results ===
best_params <- at_catboost$learner$param_set$values
saveRDS(best_params, file = "result/catboost_tuning_params.rds")
saveRDS(at_catboost$learner, file = "result/catboost_tuning_model.rds")
saveRDS(at_catboost$archive, file = "result/catboost_tuning_archive.rds")

# === Extract Top10 ===
archive <- readRDS("result/catboost_tuning_archive.rds")
dt <- as.data.table(archive)
top10 <- dt[order(-classif.acc)][1:10]
top10_params <- top10[, archive$search_space$ids(), with = FALSE]
saveRDS(top10_params, "result/top10_catboost_params.rds")
