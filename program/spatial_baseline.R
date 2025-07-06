.libPaths("/media/external/s25_5/Rlibs")
library(mlr3verse)
library(mlr3extralearners)
library(data.table)
library(dplyr)
library(tidyverse)
library(mlr3pipelines)
library(future)
source("customized_function.R")
data_all_spatial <- readRDS("data/data_all_spatial.rds") %>%
  select(-lga, -ward, -subvillage,-district_code, -region,-dataset,
         -region_district,-region_code,-population_500)
#read the result of imputation
sp_learner <- readRDS("result/imp_learner_sp.rds")
sp_median_mode <- readRDS("result/imp_median_mode_sp.rds")
sp_mean_mode <- readRDS("result/imp_mean_mode_sp.rds")
sp_delete <-readRDS("result/imp_delete_sp.rds")

#mean mode is slightly better than median mode
train_data_sp <- data_all_spatial %>% filter(!is.na(status_group))
test_data_sp  <- data_all_spatial %>% filter(is.na(status_group))

impute_values <- list(
  gps_height     = median(train_data_sp$gps_height, na.rm = TRUE),
  years_in_use   = median(train_data_sp$years_in_use, na.rm = TRUE),
  scheme_management = get_mode(train_data_sp$scheme_management),
  public_meeting    = get_mode(train_data_sp$public_meeting),
  permit            = get_mode(train_data_sp$permit),
  installer         = get_mode(train_data_sp$installer),
  funder            = get_mode(train_data_sp$funder)
)

train_data_sp_imp <- imputation(train_data_sp, impute_values)
test_data_sp_imp  <- imputation(test_data_sp,  impute_values)


data_all_spatial_imputed <- bind_rows(train_data_sp_imp, test_data_sp_imp)
saveRDS(data_all_spatial_imputed, "data/data_all_spatial_imputed.rds")

###create baseline
#read dataset
df <- readRDS("data/data_all_spatial_imputed.rds")
train_df <- df %>% filter(!is.na(status_group))

#paralel location cluster
feature_sets <- list(
  cluster1 = c("location_cluster", setdiff(colnames(train_df), c("status_group", "location_cluster2"))),
  cluster2 = c("location_cluster2", setdiff(colnames(train_df), c("status_group", "location_cluster")))
)

#learners
learners <- list(
  po("encode") %>>% lrn("classif.kknn", predict_type = "prob") %>% as_learner(),
  po("encode") %>>% lrn("classif.rpart", predict_type = "prob") %>% as_learner(),
  po("encode") %>>% lrn("classif.ranger", predict_type = "prob") %>% as_learner(),
  po("encode") %>>% lrn("classif.xgboost", predict_type = "prob") %>% as_learner()
)

#create tasks
tasks <- lapply(names(feature_sets), function(name) {
  TaskClassif$new(
    id = paste0("pump_", name),
    backend = train_df[, c("status_group", feature_sets[[name]])],
    target = "status_group"
  )
})
names(tasks) <- names(feature_sets)

#3cv
resampling <- rsmp("cv", folds = 3)

#benchmark design
design <- benchmark_grid(
  tasks = tasks,
  learners = learners,
  resamplings = list(resampling)
)

set.seed(7832)
plan(multisession)

bmr <- benchmark(design)
agg <- bmr$aggregate(msrs(c("classif.acc", "classif.ce","classif.bacc")))
saveRDS(agg, "result/benchmark_spatial.rds")
