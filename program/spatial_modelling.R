.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
library(mlr3)
library(mlr3pipelines)
library(mlr3learners)
library(data.table)
library(dplyr)
library(future)
library(parallel)
workers <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1"))
print(workers)
plan(multisession, workers = 16)
data_spatial_ml <- readRDS("data/data_all_spatial_imputed.rds") %>%
  mutate(set = ifelse(is.na(status_group), "test", "train")) %>%
  mutate(location_cluster = as.factor(location_cluster),
         location_cluster2 = as.factor(location_cluster2))

data_spatial_ml_copy <- data_spatial_ml %>% select(-location_cluster, -set)

train_data <- data_spatial_ml_copy %>% filter(!is.na(status_group))
test_data  <- data_spatial_ml_copy %>% filter(is.na(status_group))

task_final <- TaskClassif$new(
  id = "pump_final",
  backend = train_data,
  target = "status_group"
)

graph <- po("encode") %>>%
  lrn("classif.xgboost", predict_type = "prob", nthread = 7)

learner <- GraphLearner$new(graph)

learner$train(task_final)

test_task <- TaskClassif$new(
  id = "pump_test",
  backend = test_data,
  target = "status_group"
)

pred <- learner$predict(test_task)

predicted_labels <- pred$response

test_idx <- which(data_spatial_ml$set == "test")

data_spatial_ml$status_group[test_idx] <- predicted_labels

saveRDS(data_spatial_ml, "data/data_spatial_ml.rds")
