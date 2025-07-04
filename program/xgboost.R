source("setup.R")
fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed.rds") # all:train + test
# sub_imputed <- readRDS("data/sub_imputed.rds") # train+noll test
test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")

# keep region_district
# ---- No imputation----
df_fi <- fi_clean %>%
  select(-region, -district_code, -subvillage, -ward, 
         -lga, -region_code, -longitude, -latitude)

train_fi <- df_fi %>% filter(!is.na(status_group))
test_fi  <- df_fi %>% filter( is.na(status_group))

# mlr3 task on the training set
task_fi <- TaskClassif$new(
  id      = "fi",
  backend = train_fi,
  target  = "status_group"
)

# Build a pipeline Graph:
graph <- 
  po("colapply",   
     param_vals = list(
       applicator     = function(x) if (is.character(x)) as.factor(x) else x,
       affect_columns = selector_type("character")
     )
  ) %>>%
  po("removeconstants", 
     param_vals = list(
       affect_columns = selector_type(c("numeric","integer","factor"))
     )
  ) %>>%
  po("encode", method = "treatment") %>>%
  lrn("classif.xgboost",
      predict_type            = "response",
      nrounds                 = 1000L,
      eta                     = 0.1,
      max_depth               = 6,
      subsample               = 0.8,
      colsample_bytree        = 0.8,
      nthread          = 4
  )


# Wrap as GraphLearner
glrn_fi <- GraphLearner$new(graph)

# 5-fold CV
resampling <- rsmp("cv", folds = 5)
rr_fi <- mlr3::resample(task_fi, glrn_fi, resampling, store_models = FALSE)

# Aggregate CV accuracy
rr_fi$aggregate(msr("classif.acc")) #0.8017056 
rr_fi$aggregate(msr("classif.bacc")) #0.6513737
# cat("XGBoost 5-fold CV Accuracy:", acc_5cv, "\n") #0.7841955

# test prediction
glrn_fi$train(task_fi)
task_test_fi <- TaskClassif$new(
  id      = "task_test_fi",
  backend = test_fi,
  target  = "status_group"
)

pred_test_fi <- glrn_fi$predict(task_test_fi)

test_fi$status_group <- pred_test_fi$response

result_NA <- data.frame(
  id           = test_all$id,
  status_group = test_fi$status_group,
  stringsAsFactors = FALSE
)

saveRDS(result_NA, "data/predictions_with_NA.rds")

# ---- data_imputed and region_district----
df_imp <<- imputed

train_imp <- df_imp %>% filter(!is.na(status_group))
test_imp  <<- test_full_imp
test_imp$status_group <- factor(NA, levels = levels(train_imp$status_group))

# mlr3 task on the training set
task_imp <- TaskClassif$new(
  id      = "imp",
  backend = train_imp,
  target  = "status_group"
)

# Build a pipeline Graph:
graph <- 
  po("colapply",   
     param_vals = list(
       applicator     = function(x) if (is.character(x)) as.factor(x) else x,
       affect_columns = selector_type("character")
     )
  ) %>>%
  po("removeconstants", 
     param_vals = list(
       affect_columns = selector_type(c("numeric", "integer"))
     )
  ) %>>%
  po("encode", method = "treatment") %>>%
  lrn("classif.xgboost",
      predict_type = "response",
      nrounds                 = 1000L,
      eta                     = 0.1,
      max_depth               = 6,
      subsample               = 0.8,
      colsample_bytree        = 0.8,
      nthread          = 4)


# Wrap as GraphLearner
glrn_imp <- GraphLearner$new(graph)

# 5-fold CV
resampling <- rsmp("cv", folds = 5)
rr_imp <- mlr3::resample(task_imp, glrn_imp, resampling, store_models = FALSE)

# Aggregate CV accuracy
rr_imp$aggregate(msr("classif.acc")) #0.8000876 #0.8031889 # 0.8021607 
rr_imp$aggregate(msr("classif.bacc")) #0.6508549 #0.6553556 #0.6531757 

# test prediction
glrn_imp$train(task_imp)
task_test_imp <- TaskClassif$new(
  id      = "task_test_imp",
  backend = test_imp,
  target  = "status_group"
)

pred_test_imp <- glrn_imp$predict(task_test_imp)

test_imp$status_group <- pred_test_imp$response

result_imp <- data.frame(
  id           = test_all$id,
  status_group = test_imp$status_group,
  stringsAsFactors = FALSE
)

saveRDS(result_imp, "data/predictions_with_imp.rds")

