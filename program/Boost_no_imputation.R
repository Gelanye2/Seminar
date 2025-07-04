source("setup.R")
fi_clean <- readRDS("data/fi_clean.rds")
fi_boost <<- fi_clean

# # keep longitude, latitude == NA then drop these 2 columns
# fi_boost <- fi_boost %>%
#   filter(longitude == 0 | latitude == -2e-08) %>%
#   select(-longitude, -latitude)
# 
# only keep region_district
fi_boost <- fi_boost %>% select(-region, -district_code, -subvillage, -ward,
                                -lga, -region_code, -years_in_use) %>%
                         select_if(~ length(unique(.[!is.na(.)])) > 1)

fi_boost <- fi_boost %>% select_if(~ length(unique(.[!is.na(.)])) > 1)

set.seed(7832)

# no learner, no imputation
train_fi <- fi_boost %>% filter(!is.na(status_group))
test_fi  <- fi_boost %>% filter( is.na(status_group))
imputed <- readRDS("data/data_imputed.rds")
fi_boost <<- imputed

# keep longitude, latitude == NA then drop these 2 columns
fi_boost <- fi_boost %>%
  filter(longitude == 0 | latitude == -2e-08) %>%
  select(-longitude, -latitude)

# only keep region_district
fi_boost <- fi_boost %>% select(-region, -district_code, -subvillage, -ward, 
                                -lga, -region_code, -waterpoint_type_group,
                                -years_in_use) %>%
                         select_if(~ length(unique(.[!is.na(.)])) > 1)

set.seed(7832)

# no learner, no imputation
train_fi <- fi_boost %>% filter(dataset == "train") %>% select(-dataset)
test_fi <- fi_boost %>% filter(dataset == "test") %>% select(-dataset)

# encode
train_fi$y_num <- as.numeric(train_fi$status_group) - 1
test_fi$y_num  <- as.numeric(test_fi$status_group)  - 1

cols_feat <- setdiff(names(train_fi), c("status_group", "y_num"))
fmla <- as.formula(paste("y_num ~", paste(cols_feat, collapse = " + ")))

char_cols <- names(train_fi)[ sapply(train_fi, is.character) ]
for(col in char_cols) {
  train_fi[[col]] <- factor(train_fi[[col]])
  test_fi[[col]]  <- factor(test_fi[[col]], levels = levels(train_fi[[col]]))
}
logi_cols <- names(train_fi)[ sapply(train_fi, is.logical) ]
train_fi[logi_cols] <- lapply(train_fi[logi_cols], as.factor)
test_fi[logi_cols] <- lapply(test_fi[logi_cols], as.factor)
levs <- levels(train_fi$status_group)

# --- Models ----
## --- GBM ----
library(gbm)
# baseline for checking acc
gbm_baseline <- gbm(
  formula           = fmla,
  distribution      = "multinomial",
  data              = train_fi,
  n.trees           = 100,
  interaction.depth = 1,
  shrinkage         = 0.01,
  n.minobsinnode    = 10,
  cv.folds          = 5,
  verbose           = FALSE
)

prob_tr_gbm <- predict(gbm_baseline, train_fi, n.trees = 100, type = "response")[,,1]
pred_tr_gbm <- apply(prob_tr_gbm, 1, function(p) levs[which.max(p)])

acc_tr_gbm <- mean(pred_tr_gbm == train_fi$status_group, na.rm = TRUE)
cat("GBM training accuracy (baseline):", acc_tr_gbm, "\n") #0.7046633 0.7257174 0.7273731 

# 5cv: 0.7268212 0.7273731 0.7378587 
# 5cv impu: 0.7000219 

cm   <- table(true = train_fi$status_group, pred = pred_tr_gbm)
sens <- diag(cm) / rowSums(cm)
# balanced_acc_manual
mean(sens)

## --- lightgbm ----
library(lightgbm)
train_mat <- data.matrix(train_fi[, cols_feat])
test_mat  <- data.matrix(test_fi[,  cols_feat])
num_class <- length(levs)
cat_feats <- intersect(cols_feat, char_cols)

dtrain <- lgb.Dataset(
  data = train_mat,
  label = train_fi$y_num,
  categorical_feature = cat_feats
)

## baseline for checking acc
lgb_baseline <- lgb.train(
  params = list(
    objective     = "multiclass",
    num_class     = num_class,
    learning_rate = 0.1,
    num_leaves    = 31,
    metric        = "multi_error"  
  ),
  data    = dtrain,
  valids    = list(train = dtrain),
  nrounds = 100,
  verbose = 0
)

pred_raw_lgb <- predict(lgb_baseline, train_mat)
pred_mat_lgb <- matrix(pred_raw_lgb, ncol = num_class, byrow = FALSE)

colnames(pred_mat_lgb) <- levs
pred_labels <- factor(
  colnames(pred_mat_lgb)[max.col(pred_mat_lgb, ties.method="first")],
  levels = levs
)
mean(pred_labels == train_fi$status_group)
acc_tr_lgb <- 1 - lgb_baseline$record_evals$train$multi_error$eval[100][[1]]
cat("LightGBM training accuracy (baseline):", acc_tr_lgb, "\n") #0.8007744 #0.8984547 0.8962472 0.9050773 
# imp: 0.7000219 

# 5cv
params <- list(
  objective        = "multiclass",
  num_class        = num_class,
  learning_rate    = 0.05,
  num_leaves       = 31,
  metric           = "multi_error"
)

cv_res <- lgb.cv(
  params              = params,
  data                = dtrain,
  nrounds             = 2000,
  nfold               = 5,
  early_stopping_rounds = 50,
  verbose             = 0,
  stratified          = TRUE      
)

best_iter   <- cv_res$best_iter
best_err    <- cv_res$record_evals$valid$multi_error$eval[best_iter]
lgb_acc_5cv <- 1 - best_err[[1]]
cat("LightGBM 5-fold CV Accuracy:", lgb_acc_5cv, "\n") #0.8001179 #0.806846 0.800768 0.8024261 
# imp: 0.8012843 

## --- XGboost ----
library(xgboost)
for(col in cols_feat){
  if (is.character(train_fi[[col]]))
    train_fi[[col]] <- factor(train_fi[[col]])
  if (is.factor(train_fi[[col]]) || is.logical(train_fi[[col]]))
    train_fi[[col]] <- as.integer(train_fi[[col]]) - 1
}

train_mat <- data.matrix(train_fi[, cols_feat])
c(nrow(train_mat), length(train_fi$y_num))

dtrain <- xgb.DMatrix(
  data    = train_mat,
  label   = train_fi$y_num,
  missing = NA
)

num_class <- length(levels(train_fi$status_group))
params <- list(
  objective   = "multi:softprob",
  num_class   = num_class,
  eval_metric = "merror",       
  eta         = 0.3,              
  max_depth   = 6                 
)

xgb_baseline <- xgb.train(
  params   = params,
  data     = dtrain,
  nrounds  = 100,
  verbose  = 0
)

pred_raw <- predict(xgb_baseline, train_mat)
pred_mat <- matrix(pred_raw,
                   nrow = nrow(train_fi),
                   ncol = num_class,
                   byrow = TRUE)
head(rowSums(pred_mat))

colnames(pred_mat) <- levs
pred_labels <- factor(
  colnames(pred_mat)[max.col(pred_mat, ties.method="first")],
  levels = levs
)

acc_train_xgb <- mean(pred_labels == train_fi$status_group, na.rm = TRUE)
cat("XGBoost training accuracy (baseline):", acc_train_xgb, "\n") #0.83 #0.9050773 0.9001104  0.9050773 
# imp: 0.8279314 

# 5cv
dtrain_xgb <- xgb.DMatrix(train_mat, label = train_fi$y_num, missing = NA)

xgb_params <- list(
  objective   = "multi:softprob",
  num_class   = num_class,
  eval_metric = "merror",
  eta         = 0.05,
  max_depth   = 6,
  subsample   = 0.8,
  colsample_bytree = 0.8
)

cv_xgb <- xgb.cv(
  params      = xgb_params,
  data        = dtrain_xgb,
  nrounds     = 2000,
  nfold       = 5,
  early_stopping_rounds = 50,
  verbose     = 0,
  stratified  = TRUE
)

best_idx      <- cv_xgb$best_iteration
best_err_xgb  <- cv_xgb$evaluation_log[best_idx,]$test_merror_mean
xgb_acc_5cv   <- 1 - best_err_xgb
cat("XGBoost 5-fold CV Accuracy:", xgb_acc_5cv, "\n") 
#0.8040729 (no la, lo) 0.8107301 0.8123702 0.8134758 
#0.7984866 

## --- CatBoost ----
Sys.setenv(R_INSTALL_STAGED = "FALSE")
devtools::load_all("catboost/catboost/R-package")

for(col in cols_feat){
  train_fi[[col]] <- factor(train_fi[[col]])
  test_fi [[col]] <- factor(test_fi [[col]], levels = levels(train_fi[[col]]))
}

cat_fac <- names(train_fi)[sapply(train_fi, is.factor)]

train_pool <- catboost.load_pool(
  data         = train_fi[, cols_feat],
  label        = train_fi$y_num
)

catboost_params <- list(
  iterations     = 100,
  depth          = 6,
  learning_rate  = 0.3,
  loss_function  = "MultiClass",
  logging_level  = "Silent"    
)

cat_model <- catboost.train(
  learn_pool = train_pool,
  params     = catboost_params
)

pred0 <- catboost.predict(
  model            = cat_model,
  pool             = train_pool,
  prediction_type  = "Class"
)

pred_lbls <- factor(levs[pred0 + 1], levels = levs)
acc_train_cat <- mean(pred_lbls == train_fi$status_group)
cat("CatBoost training accuracy (baseline):", acc_train_cat, "\n") #0.7819024 #0.8245033 0.8228477 

# 5cv
cb_params <- list(
  loss_function  = "MultiClass",
  learning_rate  = 0.3,
  depth          = 6,
  iterations     = 100,
  eval_metric    = "Accuracy",
  random_seed    = 7832,
  logging_level  = "Silent"
)

cv_cb <- catboost.cv(
  params      = cb_params,
  pool        = train_pool,
  fold_count  = 5,
  type        = "Classical",  
  partition_random_seed = 7832
)

best_row        <- cv_cb[nrow(cv_cb), ]
cb_acc_5cv      <- best_row$test.Accuracy.mean
cat("CatBoost 5-fold CV Accuracy:", cb_acc_5cv, "\n") #0.7908239 (no la, lo) 0.7897493 

## --- H2O GBM ----
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "4G")

train_fi$status_group <- as.factor(train_fi$status_group)
hf <- as.h2o(train_fi)
response_col <- "status_group"

h2o_gbm <- h2o.gbm(
  x                = cols_feat,
  y                = response_col,
  training_frame   = hf,
  distribution     = "multinomial",
  ntrees           = 100,
  max_depth        = 6,
  learn_rate       = 0.3,
  seed             = 7832,
  verbose          = FALSE
)

pred_hf <- h2o.predict(h2o_gbm, hf)
pred_vec <- as.vector(pred_hf$predict)

true_vec <- as.vector(hf[[response_col]])
acc_h2o   <- mean(pred_vec == true_vec)
cat("H2O GBM training accuracy (baseline):", acc_h2o, "\n") #0.9068687 #0.9116998 0.910596

# 5cv
h2o_gbm_cv <- h2o.gbm(
  x               = cols_feat,
  y               = response_col,
  training_frame  = hf,
  nfolds          = 5,
  fold_assignment = "Stratified",
  keep_cross_validation_predictions = TRUE,
  ntrees          = 100,
  max_depth       = 6,
  learn_rate      = 0.3,
  seed            = 7832
)

perf_cv <- h2o.performance(h2o_gbm_cv, xval = TRUE)
cm <- h2o.confusionMatrix(perf_cv)
cm_counts <- as.matrix(cm[1:3, 1:3])
overall_acc <- sum(diag(cm_counts)) / sum(cm_counts)
cat("Multiclass CV accuracy:", overall_acc, "\n") #0.7904545 #0.8018764 0.803532 

## --- C5.0 ----
library(C50)
fmla   <- as.formula(paste("status_group ~", paste(cols_feat, collapse = " + ")))
c5_mod <- C5.0(fmla, data = train_fi)

pred_train <- predict(c5_mod, train_fi)

acc_train_c5 <- mean(pred_train == train_fi$status_group, na.rm = TRUE)
cat("C5.0 training accuracy (baseline):", acc_train_c5, "\n") #0.7680303 #0.8383002 0.8360927 

# 5cv
set.seed(7832)
folds <- createFolds(train_fi$status_group, k = 5)

cv_acc <- sapply(folds, function(idx_test) {
  tr <- train_fi[-idx_test, ]
  va <- train_fi[ idx_test, ]
  
  m  <- C5.0(fmla, data = tr)
  
  preds <- predict(m, va)
  mean(preds == va$status_group, na.rm = TRUE)
})

cv_acc
mean(cv_acc) #0.7461784 #0.79139 0.7814483

fi_boost %>% group_by(region_district) %>% length(is.na(status_group))
