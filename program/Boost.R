source("setup.R")
df_combined <- readRDS("data/df_combined.rds")
df_boost <<- df_combined
set.seed(7832)

trainset <- df_boost %>% filter(dataset == "train")
testset <- df_boost %>% filter(dataset == "test")

# encode
trainset$y_num <- as.numeric(trainset$status_group) - 1
testset$y_num  <- as.numeric(testset$status_group)  - 1

cols_feat <- setdiff(names(trainset), c("dataset", "status_group", "y_num"))
fmla <- as.formula(paste("y_num ~", paste(cols_feat, collapse = " + ")))

char_cols <- names(trainset)[ sapply(trainset, is.character) ]
for(col in char_cols) {
  trainset[[col]] <- factor(trainset[[col]])
  testset[[col]]  <- factor(testset[[col]], levels = levels(trainset[[col]]))
}
logi_cols <- names(trainset)[ sapply(trainset, is.logical) ]
trainset[logi_cols] <- lapply(trainset[logi_cols], as.factor)
testset[ logi_cols] <- lapply(testset[ logi_cols], as.factor)

# --- Models ----
## --- GBM ----
library(gbm)
gbm_model <- gbm(
  formula          = fmla,
  distribution     = "multinomial",
  data             = trainset,
  n.trees          = 200,
  interaction.depth= 4,
  shrinkage        = 0.1,
  cv.folds         = 5,
  n.cores          = NULL,
  verbose          = FALSE
)

best_iter <- gbm.perf(gbm_model, method="cv")

head(gbm_model$cv.error)
plot(gbm_model$cv.error, type="l", 
     xlab="Number of Trees", ylab="CV Deviance")

pred_prob_array <- predict(
  object  = gbm_model,
  newdata = testset,
  n.trees = best_iter,
  type    = "response"
)

pred_mat <- pred_prob_array[,,1]

levs <- levels(trainset$status_group)
pred_class <- apply(pred_mat, 1, function(probs){
  levs[ which.max(probs) ]
})

testset$status_group <- factor(pred_class, levels = levs)

# baseline for checking acc
gbm_baseline <- gbm(
  formula           = fmla,
  distribution      = "multinomial",
  data              = trainset,
  n.trees           = 100,
  interaction.depth = 1,
  shrinkage         = 0.01,
  n.minobsinnode    = 10,
  cv.folds          = 0,
  verbose           = FALSE
)

prob_tr_gbm <- predict(gbm_baseline, trainset, n.trees = 100, type = "response")[,,1]
pred_tr_gbm <- apply(prob_tr_gbm, 1, function(p) levs[which.max(p)])

acc_tr_gbm <- mean(pred_tr_gbm == trainset$status_group, na.rm = TRUE)
cat("GBM training accuracy (baseline):", round(acc_tr_gbm, 4), "\n") #0.7040572

## --- lightgbm ----
library(lightgbm)
train_mat <- data.matrix(trainset[, cols_feat])
test_mat  <- data.matrix(testset[,  cols_feat])
num_class <- length(levs)
cat_feats <- intersect(cols_feat, char_cols)

dtrain <- lgb.Dataset(
  data = train_mat,
  label = trainset$y_num,
  categorical_feature = cat_feats
)

params <- list(
  objective        = "multiclass",
  num_class        = num_class,
  learning_rate    = 0.01,
  num_leaves       = 31,
  metric           = "multi_logloss"
)

cvres <- lgb.cv(
  params              = params,
  data                = dtrain,
  nrounds             = 500,
  nfold               = 5,
  early_stopping_rounds = 50,
  verbose             = 0
)
best_iter <- cvres$best_iter

model_lgb <- lgb.train(
  params   = params,
  data     = dtrain,
  nrounds  = best_iter
)

pred_raw <- predict(model_lgb, test_mat)

pred_mat <- matrix(
  pred_raw,
  ncol = num_class,
  byrow = TRUE
)

pred_labels <- apply(pred_mat, 1, function(p) levs[which.max(p)])

testset$status_group <- factor(pred_labels, levels = levs)
head(testset[, c(cols_feat[1:3], "status_group")])

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
pred_mat_lgb <- matrix(pred_raw_lgb, ncol = num_class, byrow = TRUE)
pred_tr_lgb  <- apply(pred_mat_lgb, 1, function(p) levs[which.max(p)])

colnames(pred_mat) <- levels(trainset$status_group)
pred_labels <- factor(
  colnames(pred_mat)[max.col(pred_mat, ties.method="first")],
  levels = levels(trainset$status_group)
)
acc_tr_lgb <- mean(pred_labels == trainset$status_group)
cat("LightGBM training accuracy (baseline):", round(acc_tr_lgb, 4), "\n") #0.8059933
1 - lgb_baseline$record_evals$train$multi_error$eval[100][[1]]

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
cat("LightGBM 5-fold CV Accuracy:", lgb_acc_5cv, "\n") #0.801027 

## --- XGboost ----
library(xgboost)
for(col in cols_feat){
  if (is.character(trainset[[col]]))
    trainset[[col]] <- factor(trainset[[col]])
  if (is.factor(trainset[[col]]) || is.logical(trainset[[col]]))
    trainset[[col]] <- as.integer(trainset[[col]]) - 1
}

train_mat <- data.matrix(trainset[, cols_feat])
c(nrow(train_mat), length(trainset$y_num))

dtrain <- xgb.DMatrix(
  data    = train_mat,
  label   = trainset$y_num,
  missing = NA
)

num_class <- length(levels(trainset$status_group))
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
                   nrow = nrow(trainset),
                   ncol = num_class,
                   byrow = TRUE)
head(rowSums(pred_mat))

colnames(pred_mat) <- levs
pred_labels <- factor(
  colnames(pred_mat)[max.col(pred_mat, ties.method="first")],
  levels = levs
)

acc_train_xgb <- mean(pred_labels == trainset$status_group, na.rm = TRUE)
cat("XGBoost training accuracy (baseline):", round(acc_train_xgb, 4), "\n") #0.8284

# 5cv
dtrain_xgb <- xgb.DMatrix(train_mat, label = trainset$y_num, missing = NA)

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
cat("XGBoost 5-fold CV Accuracy:", round(xgb_acc_5cv, 4), "\n")

## --- CatBoost ----
Sys.setenv(R_INSTALL_STAGED = "FALSE")<
devtools::load_all("catboost/catboost/R-package")

for(col in cols_feat){
  trainset[[col]] <- factor(trainset[[col]])
  testset [[col]] <- factor(testset [[col]], levels = levels(trainset[[col]]))
}

cat_fac <- names(trainset)[sapply(trainset, is.factor)]

train_pool <- catboost.load_pool(
  data         = trainset[, cols_feat],
  label        = trainset$y_num
)

catboost_params <- list(
  iterations     = 100,
  depth          = 6,
  learning_rate  = 0.3,
  loss_function  = "MultiClass",
  logging_level  = "Silent"    # <- core CatBoost param
)

cat_model <- catboost.train(
  learn_pool = train_pool,
  params     = catboost_params
  # you can still pass verbose=FALSE here if you want no R‐side messages
)

pred0 <- catboost.predict(
  model            = cat_model,
  pool             = train_pool,
  prediction_type  = "Class"
)

pred_lbls <- factor(levs[pred0 + 1], levels = levs)
acc_train_cat <- mean(pred_lbls == trainset$status_group)
cat("CatBoost training accuracy (baseline):", round(acc_train_cat, 4), "\n") #0.7817 

# 5cv
cb_params <- list(
  loss_function  = "MultiClass",
  learning_rate  = 0.05,
  depth          = 6,
  iterations     = 2000,
  eval_metric    = "Accuracy",
  random_seed    = 42,
  logging_level  = "Silent"
)

cv_cb <- catboost.cv(
  params      = cb_params,
  pool        = train_pool,
  fold_count  = 5,
  type        = "Classical",  
  partition_random_seed = 42
)

best_row        <- cv_cb[nrow(cv_cb), ]
cb_acc_5cv      <- best_row$test.Accuracy.mean
cat("CatBoost 5-fold CV Accuracy:", round(cb_acc_5cv, 4), "\n")

## --- H2O GBM ----
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "4G")

trainset$status_group <- as.factor(trainset$status_group)
hf <- as.h2o(trainset)
response_col <- "status_group"

h2o_gbm <- h2o.gbm(
  x                = cols_feat,
  y                = response_col,
  training_frame   = hf,
  distribution     = "multinomial",
  ntrees           = 100,
  max_depth        = 6,
  learn_rate       = 0.3,
  seed             = 1234,
  verbose          = FALSE
)

pred_hf <- h2o.predict(h2o_gbm, hf)
pred_vec <- as.vector(pred_hf$predict)

true_vec <- as.vector(hf[[response_col]])
acc_h2o   <- mean(pred_vec == true_vec)
cat("H2O GBM training accuracy (baseline):", round(acc_h2o, 4), "\n") #0.9019192

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
  seed            = 1234
)

perf_cv <- h2o.performance(h2o_gbm_cv, xval = TRUE)
cm <- h2o.confusionMatrix(perf_cv)
cm_counts <- as.matrix(cm[1:3, 1:3])
overall_acc <- sum(diag(cm_counts)) / sum(cm_counts)
cat("Multiclass CV accuracy:", round(overall_acc,4), "\n") #0.7907912

## --- C5.0 ----
library(C50)
fmla   <- as.formula(paste("status_group ~", paste(cols_feat, collapse = " + ")))
c5_mod <- C5.0(fmla, data = trainset)

pred_train <- predict(c5_mod, trainset)

acc_train_c5 <- mean(pred_train == trainset$status_group, na.rm = TRUE)
cat("C5.0 training accuracy (baseline):", round(acc_train_c5,4), "\n") #0.7632323
