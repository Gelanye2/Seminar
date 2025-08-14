.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3pipelines)
library(future)
library(data.table)
library(dplyr)
set.seed(7832)
plan(multisession,workers = 16)

### Benchmark datasets
resampling <- rsmp("repeated_cv", repeats = 3, folds = 5)

po_char2fac <- po("colapply", id = "char2factor",
                  param_vals = list(
                    applicator     = as.factor,
                    affect_columns = selector_type("character")
                  ))

non_imputed <- readRDS("data/fi_clean.rds") %>% filter(!is.na(status_group))
non_imputed_enhanced <- readRDS("data/data_enhanced.rds") %>% filter(!is.na(status_group))

task2 <- list(
  non_imputed = TaskClassif$new(id = "non_imputed", backend = non_imputed, target = "status_group"),
  non_imputed_enhanced = TaskClassif$new(id = "non_imputed_enhanced", backend = non_imputed_enhanced, target = "status_group")
)

learner2 <- list(
  po_char2fac %>>%po("encode") %>>% lrn("classif.xgboost", predict_type = "prob",nthread = 7) %>% as_learner(),
  po_char2fac %>>%po("encode") %>>% lrn("classif.lightgbm", predict_type = "prob",  objective = "multiclass", num_threads = 7) %>% as_learner(),
  lrn("classif.catboost",predict_type = "prob",thread_count = 7) %>% as_learner()
)

design2 <- benchmark_grid(
  tasks = task2,
  learners = learner2,
  resamplings = list(resampling)
)

bmr2 <- benchmark(design2)
agg2 <- bmr2$aggregate(msrs(c("classif.acc","classif.bacc")))
saveRDS(agg2, "result/benchmark_non_imputed.rds")
