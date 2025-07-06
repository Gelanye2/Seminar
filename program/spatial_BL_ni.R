.libPaths("/media/external/s25_5/Rlibs")
library(mlr3verse)
library(mlr3extralearners)
library(data.table)
library(dplyr)
library(tidyverse)
library(mlr3pipelines)
library(future)
#test XGboost without imputation
data_all_spatial <- readRDS("data/data_all_spatial.rds") %>%
  select(-lga, -ward, -subvillage,-district_code, -region,-dataset,
         -region_district,-region_code,-population_500)
train_raw <- data_all_spatial %>% filter(!is.na(status_group))

#only decide with location_cluster2
task_raw <- TaskClassif$new(
  id = "pump_raw_cluster2",
  backend = train_raw[, c("status_group", "location_cluster2")],
  target = "status_group"
)
#same step as above
learner_raw_xgb <- po("encode") %>>% lrn("classif.xgboost", predict_type = "prob") %>% as_learner()
resampling <- rsmp("cv", folds = 3)
design_raw <- benchmark_grid(
  tasks = list(task_raw),
  learners = list(learner_raw_xgb),
  resamplings = list(resampling)
)

set.seed(7832)
plan(multisession)
bmr_raw <- benchmark(design_raw)
agg_raw <- bmr_raw$aggregate(msrs(c("classif.acc", "classif.ce", "classif.bacc")))
saveRDS(agg_raw, "result/benchmark_spatial_raw_xgb.rds")
