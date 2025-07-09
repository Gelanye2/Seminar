source("setup.R")
fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed.rds") # all:train + test
#test_sub_imp <- readRDS("data/test_sub_imp.rds")
test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")

# ----------- 1. ranger, with imputation ----------------
df_imp <<- imputed
train_imp <- df_imp %>% filter(!is.na(status_group))
test_imp  <<- test_full_imp
test_imp$status_group <- factor(NA, levels = levels(train_imp$status_group))

train_imp$construction_year <- NULL
test_imp$construction_year <- NULL
train_imp$num_private <- NULL
test_imp$num_private <- NULL

# mlr3 task on the training set
task_imp <- TaskClassif$new(
  id      = "Waterpoints",
  backend = train_imp,
  target  = "status_group"
)

# mlr3 task on the training set
graph_ranger <- 
  po("colapply",   
     param_vals = list(
       applicator     = function(x) if (is.character(x)) as.factor(x) else x,
       affect_columns = selector_type("character")
     )
  ) %>>%
  po("removeconstants", 
     param_vals = list(
       affect_columns = selector_type(c("numeric","integer"))
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
glrn_imp <- GraphLearner$new(graph_ranger)

# 5-fold CV
resampling <- rsmp("cv", folds = 5)
rr_imp <- mlr3::resample(task_imp, glrn_imp, resampling, store_models = FALSE)

# Aggregate CV accuracy
rr_imp$aggregate(msr("classif.acc"))  

# 0.8044698 # 0.8039136 #0.8040316 #0.8047227,score 0.8142 #######with median 0.8055316 #0.8039137 #0.8044024 #0.8062732
rr_imp$aggregate(msr("classif.bacc"))

# 0.6560779 # 0.6555948 #0.654244 #0.6552896 #0.6554827 #0.6538123 0.6539252 #0.6573183

# 0.8055654 ohne Installer # 0.8053631 #0.8048744 
# 0.8030035 ohne Installer, funder #0.8032057 
#  0.8042171 ohne "permit", "year_recorded", "public_meeting","water_quality", "scheme_management", "management", "basin")

#0.8052451 新整理列后, public socre 0.8128
# without scheme_management, score 0.8124
#0.8102004 keep langtitude,latitude without imputation, score 0.8200; 0.8111105，score 0.8211!
# 0.8104364 keep langtitude,latitude without imp,根据网上的人的删除 ；0.8113802，0.8115319
# keep langtitude,latitude with imp 0.8113634
# ohne Installer  0.8118353, but score 0.8196; 0.8114139

# 0.8127792, with response = prob

# 0.8136387 without construction year, score 0.8223!!
#m.         without construction year, scheme name, score 0.8205
#  0.8126612 without construction year, num private 

#library(mlr3filters)
#flt = flt("importance", learner = lrn("classif.ranger", importance = "impurity"))
#flt$calculate(task_imp)
#as.data.table(flt)[order(-score)]

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

saveRDS(result_imp_ranger_imputed, "data/predictions_ranger_imp_sub.rds")
write.csv(result_imp_ranger_imputed, "sub17.csv", row.names = FALSE)

#####
library(mlr3filters)

##feature importance
# Beispiel: Gini-Filter
filter = flt("importance", learner = lrn("classif.ranger", importance = "impurity"))
filter$calculate(task_imp)

# Top Features anzeigen
as.data.table(filter)[order(-score)]


# ------- 2. with rpart, No imputation-------
df_fi <- fi_clean %>%
  select(-region, -district_code, -subvillage, -ward, 
         -lga, -region_code, -longitude, -latitude, -region_district)

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


# --------------- 3. with rpart, with imputation -------------------
df_imp <<- imputed %>% select(-region_district)

train_imp <- df_imp %>% filter(!is.na(status_group))
test_imp  <<- test_full_imp %>% select(-region_district)

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


######## ---------5. knn, imputed -------------
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


