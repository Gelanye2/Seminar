source("setup.R")
fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed.rds") # all:train + test
sub_imputed <- readRDS("data/sub_imputed.rds") # train + test without longtitude and latitude
test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")


# ---- 1. with rpart, No imputation----
df_fi <- fi_clean %>%
  select(-region, -district_code, -subvillage, -ward, 
         -lga, -region_code, -longitude, -latitude)

train_fi <- df_fi %>% filter(!is.na(status_group))
test_fi  <- df_fi %>% filter( is.na(status_group))

# mlr3 task on the training set
task_fi <- TaskClassif$new(
  id      = "Waterpoints",
  backend = train_fi,
  target  = "status_group"
)

# pipeline Graph:
graph_rpart <- 
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
  lrn("classif.rpart",
      predict_type = "response",
      cp = 0.001,
      minsplit = 10,
      maxdepth = 30,
      maxsurrogate = 0
  )

# Wrap as GraphLearner
glrn_rpart <- GraphLearner$new(graph_rpart)

# 5-fold CV
resampling <- rsmp("cv", folds = 5)
rr_rpart <- mlr3::resample(task_fi, glrn_rpart, resampling, store_models = FALSE)

# Aggregate CV accuracy
rr_rpart$aggregate(msr("classif.acc"))  #0.6970387 #0.697123 #0.6965498 0.7087862 
rr_rpart$aggregate(msr("classif.bacc"))  # 0.472056 #0.4721082 #0.4714663 0.5078952 

# test prediction
glrn_rpart$train(task_fi)
task_test_fi <- TaskClassif$new(
  id      = "task_test_fi",
  backend = test_fi,
  target  = "status_group"
)

pred_test_fi <- glrn_rpart$predict(task_test_fi)

test_fi$status_group <- pred_test_fi$response

result_rpart_NA <- data.frame(
  id           = test_all$id,
  status_group = test_fi$status_group,
  stringsAsFactors = FALSE
)

saveRDS(result_rpart_NA, "data/predictions_rpart_NA.rds")


# --------------- 2. with rpart, with imputation -------------------
df_imp <<- imputed
train_imp <- df_imp %>% filter(!is.na(status_group))
test_imp  <<- test_full_imp
test_imp$status_group <- factor(NA, levels = levels(train_imp$status_group))

# mlr3 task on the training set
task_imp <- TaskClassif$new(
  id      = "Waterpoints",
  backend = train_imp,
  target  = "status_group"
)

# pipeline Graph:
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
  lrn("classif.rpart",
      predict_type = "response",
      cp = 0.001,
      minsplit = 10,
      maxdepth = 30,
      maxsurrogate = 0
  )


# Wrap as GraphLearner
glrn_imp <- GraphLearner$new(graph)

# 5-fold CV
resampling <- rsmp("cv", folds = 5)
rr_imp <- mlr3::resample(task_imp, glrn_imp, resampling, store_models = FALSE)

# Aggregate CV accuracy
rr_imp$aggregate(msr("classif.acc"))  # 0.7358885 #0.7335794 
rr_imp$aggregate(msr("classif.bacc")) #  0.5393222 

# test prediction
glrn_imp$train(task_imp)
task_test_imp <- TaskClassif$new(
  id      = "task_test_imp",
  backend = test_imp,
  target  = "status_group"
)

pred_test_imp <- glrn_imp$predict(task_test_imp)

test_imp$status_group <- pred_test_imp$response

result_imp_rpart_imputed <- data.frame(
  id           = test_all$id,
  status_group = test_imp$status_group,
  stringsAsFactors = FALSE
)

saveRDS(result_imp_rpart_imputed, "data/predictions_rpart_imp.rds")


######## ------------ 3. ranger, imputed ------------------------
df_imp <<- imputed
train_imp <- df_imp %>% filter(!is.na(status_group))
test_imp  <<- test_full_imp
test_imp$status_group <- factor(NA, levels = levels(train_imp$status_group))

# mlr3 task on the training set
task_imp <- TaskClassif$new(
  id      = "Waterpoints",
  backend = train_imp,
  target  = "status_group"
)

# pipeline Graph:
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
  lrn("classif.ranger",
      predict_type = "response",
      num.trees = 500,
      mtry = floor(sqrt(ncol(train_imp))),
      min.node.size = 1,
      importance = "impurity"
  )

# Wrap as GraphLearner
glrn_imp <- GraphLearner$new(graph)

# 5-fold CV
resampling <- rsmp("cv", folds = 5)
rr_imp <- mlr3::resample(task_imp, glrn_imp, resampling, store_models = FALSE)

# Aggregate CV accuracy
rr_imp$aggregate(msr("classif.acc"))  #  0.8044698 # 0.8039136 #0.8040316 #0.8047227
rr_imp$aggregate(msr("classif.bacc")) #   0.6560779 # 0.6555948 #0.654244 #0.6552896

# test prediction
glrn_imp$train(task_imp)
task_test_imp <- TaskClassif$new(
  id      = "task_test_imp",
  backend = test_imp,
  target  = "status_group"
)

pred_test_imp <- glrn_imp$predict(task_test_imp)

test_imp$status_group <- pred_test_imp$response

result_imp_ranger_imputed <- data.frame(
  id           = test_all$id,
  status_group = test_imp$status_group,
  stringsAsFactors = FALSE
)

saveRDS(result_imp_ranger_imputed, "data/predictions_ranger_imp.rds")

######## ---------4. knn, imputed -------------
df_imp <<- imputed
train_imp <- df_imp %>% filter(!is.na(status_group))
test_imp  <<- test_full_imp
test_imp$status_group <- factor(NA, levels = levels(train_imp$status_group))

# mlr3 task on the training set
task_imp <- TaskClassif$new(
  id      = "Waterpoints",
  backend = train_imp,
  target  = "status_group"
)

# pipeline Graph:
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
  lrn("classif.kknn",
      predict_type = "response",
      k = 5,              
      distance = 2,       
      scale = TRUE        
  )

# Wrap as GraphLearner
glrn_imp <- GraphLearner$new(graph)

# 5-fold CV
resampling <- rsmp("cv", folds = 5)
rr_imp <- mlr3::resample(task_imp, glrn_imp, resampling, store_models = FALSE)

# Aggregate CV accuracy
rr_imp$aggregate(msr("classif.acc"))  #0.7640862 # 0.7652998
rr_imp$aggregate(msr("classif.bacc")) #0.6426241 #0.6457677

# test prediction
glrn_imp$train(task_imp)
task_test_imp <- TaskClassif$new(
  id      = "task_test_imp",
  backend = test_imp,
  target  = "status_group"
)

pred_test_imp <- glrn_imp$predict(task_test_imp)

test_imp$status_group <- pred_test_imp$response

result_imp_knn_imputed <- data.frame(
  id           = test_all$id,
  status_group = test_imp$status_group,
  stringsAsFactors = FALSE
)

saveRDS(result_imp_knn_imputed, "data/predictions_knn_imp.rds")


