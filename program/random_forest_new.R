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


# mlr3 task on the training set
task_imp <- TaskClassif$new(
  id      = "Waterpoints",
  backend = train_imp,
  target  = "status_group"
)

train_imp$col_roles$stratum <- train_imp$target_names

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

# ---- Pipeline Graph ----
graph_ranger <- po_latlon_na %>>%
  po_latlon_flag %>>%
  po_latlon_impute %>>%
  po_char2fac %>>%
  po("removeconstants") %>>%
  lrn("classif.ranger",
      predict_type = "response",
      num.trees = 1142,
      mtry = 4,
      max.depth = 60,
      min.node.size = 4,
      importance = "impurity")


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
# 0.8104364 keep langtitude,latitude without imp,根据网上的删除 ；0.8113802，0.8115319
# keep langtitude,latitude with imp 0.8113634
# ohne Installer  0.8118353, but score 0.8196; 0.8114139

# 0.8127792, with response = prob

##### 0.8136387 without construction year, score 0.8223!!
#         without construction year, scheme name, score 0.8205
# 0.8126612 without construction year, num private 

# delete cons year
# without region_district 0.8112959,score 
# 0.8127287 tuning,score 0.8230!!!with 530... ; 0.812897 tuning,0.8223;0.8139084,score 0.8234!!!!! with 437...;0.8132511,score 0.8228; 
# tuning  0.8145826, score 0.8224; 0.8137061 ,score 0.822*; 0.8127623, score 0.8218; 0.8132174;0.8133, score 0.8219
# geo culster score 0.8171;

# keep month, delete season 0.8145153, 0.8230 -> keep season
# 0.8129139, 

## new pipeline / latitude geändert -> 0.8221 mit 437...;with 629 0.8142455,score 0.8228; 0.8224 with 895...; 0.8232 with 995

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
write.csv(result_imp_ranger_imputed, "sub50.csv", row.names = FALSE)


# ignore
# ----------- 2. with rpart, No imputation-----------
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


