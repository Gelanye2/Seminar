.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
library(dplyr)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3filters)
library(mlr3measures)
library(mlr3extralearners)
library(paradox)
library(data.table)
library(xgboost)
library(future)
plan(multisession, workers = 16)
set.seed(7832)

fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed_enhanced.rds") # all:train + test
# sub_imputed <- readRDS ("data/sub_imputed.rds") # train+noll test
test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")

df_imp <<- imputed
train_imp <- df_imp %>% filter(!is.na(status_group))
test_imp  <<- df_imp %>% filter(is.na(status_group))
test_imp$status_group <- factor(NA, levels = levels(train_imp$status_group))

# mlr3 task on the training set
task_fi <- TaskClassif$new(
  id      = "fi",
  backend = train_imp,
  target  = "status_group"
)

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
  lrn("classif.kknn", predict_type = "prob")


base_lrn <- GraphLearner$new(graph)

# 5-fold CV
resampling <- rsmp("cv", folds = 3)
rr_fi <- mlr3::resample(task_fi, base_lrn, resampling, store_models = FALSE)

# Aggregate CV accuracy
rr_fi$aggregate(msr("classif.acc"))
rr_fi$aggregate(msr("classif.bacc"))
# cat("XGBoost 5-fold CV Accuracy:", acc_5cv, "\n")

# test prediction
base_lrn$train(task_fi)
task_test_fi <- TaskClassif$new(
  id      = "task_test_fi",
  backend = test_imp,
  target  = "status_group"
)

pred_test_fi <- base_lrn$predict(task_test_fi)

test_imp$status_group <- pred_test_fi$response

result_imp <- data.frame(
  id           = test_all$id,
  status_group = test_imp$status_group,
  stringsAsFactors = FALSE
)

saveRDS(result_imp, "data/knn_base_result.rds")
write.csv(result_imp, "result/subknn_base_l.csv", row.names = FALSE)
