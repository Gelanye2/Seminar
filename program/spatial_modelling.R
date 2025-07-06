.libPaths("/media/external/s25_5/Rlibs")
library(mlr3)
library(mlr3pipelines)
library(mlr3learners)
library(data.table)
library(dplyr)
data_spatial_ml <- readRDS("data/data_all_spatial_imputed.rds") %>%
  mutate(set = ifelse(is.na(status_group), "test", "train"))

data_spatial_ml_copy <- data_spatial_ml %>% select(-location_cluster, -set)

train_data <- data_spatial_ml_copy %>% filter(!is.na(status_group))
test_data  <- data_spatial_ml_copy %>% filter(is.na(status_group))

task_final <- TaskClassif$new(
  id = "pump_final",
  backend = train_data,
  target = "status_group"
)

graph <- po("encode") %>>%
  lrn("classif.xgboost", predict_type = "prob")

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

saveRDS(data_spatial_ml, "/media/external/s25_5/data/data_spatial_ml.rds")
