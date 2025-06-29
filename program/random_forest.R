
# ---- baseline ----
set.seed(7832)
train_mice_imputed <- data_imputed %>% 
  filter(!is.na(status_group)) 

cols_feat <- setdiff(names(train_mice_imputed), "status_group")
ranger_baseline <- ranger(
  formula      = status_group ~ .,
  data         = train_mice_imputed,
  num.trees    = 500,
  mtry         = floor(sqrt(length(cols_feat))),
  importance   = "impurity",
  probability  = FALSE
)

ranger_baseline
# 1. with median+mode imputation bzw. train_imp 80.85%
# 2. with mice_imputed 80.85% 

# ─── with CV ───────────────────────────────────
cols_feat <- setdiff(names(data_imputed), "status_group")

tune_grid <- expand.grid(
  mtry           = c(3, 5, 7, 10),
  splitrule      = "gini",    
  min.node.size  = 5         
)

ctrl <- trainControl(method = "cv", number = 5)

# --------- ranger ----------
ranger_cv <- train(
  x          = data_imputed[, cols_feat],     # nur Predictor-Spalten
  y          = factor(data_imputed$status_group),
  method     = "ranger",
  trControl  = ctrl,                          # 5-fold CV
  tuneGrid   = tune_grid,                     # mtry-Grid
  num.trees  = 500,                           # Waldgröße
  importance = "impurity"
)
ranger_cv
#0.806223 # train_imp
#0.8053969 # mice_imputed


# --------- k-NN trainieren (5 × 3-CV) -----------
dummies <- dummyVars(~ ., data = train_imp[, cols_feat], fullRank = TRUE)
X_train <- predict(dummies, newdata = train_imp[, cols_feat])
y_train <- train_imp$status_group

set.seed(42)

ctrl_knn <- trainControl(method  = "repeatedcv",
                         number  = 5,
                         repeats = 3)

grid_knn <- expand.grid(k = seq(3, 31, by = 2))

knn_cv <- train(
  x          = X_train,
  y          = y_train,
  method     = "knn",
  trControl  = ctrl_knn,
  tuneGrid   = grid_knn,
  preProcess = c("center", "scale")     # z-Standardisierung
)

print(knn_cv)
#      train_imp
#      mice_imputed

# ------------ rpart -----------------

set.seed(42)
ranger_cv <- train(
  x          = x_mat,
  y          = y_fac,
  method     = "ranger",
  trControl  = ctrl,                 # identisches 5-fold-CV
  tuneGrid   = tune_grid_ranger,
  num.trees  = 500,                  # Waldgröße
  importance = "impurity",
  metric     = "Accuracy"            # default bei Mehrklassen
)
ranger_cv

# --- with for loop, region wise ---
library(mlr3)
library(mlr3learners)   
library(mlr3pipelines)
library(data.table)

## -------------------  data filtering stays unchanged  ------------------- ##
df  <- data_imputed %>%
  
  select_if(~ length(unique(.[!is.na(.)])) > 1)

df_train <- data_imputed %>% filter(!is.na(status_group))

results <- list()

## -------------------  loop over regions  -------------------------------- ##
for (this_region in unique(df_train$region)) {
  
  message("Training ranger model for region: ", this_region)
  
  df_train_r <- df_train %>% filter(region == this_region)
  
  ## Task ------------------------------------------------------------------ ##
  task <- TaskClassif$new(
    id      = paste0("reg_", this_region),
    backend = as.data.table(df_train_r),
    target  = "status_group"
  )
  
  ## Pipeline: char → factor  → remove constants → ranger ------------------ ##
  graph <- po("colapply",
              param_vals = list(
                applicator     = function(x) if (is.character(x)) as.factor(x) else x,
                affect_columns = selector_type("character")
              )) %>>%
    po("fixfactors") %>>%                     # <─ NEU
    po("removeconstants",
       param_vals = list(affect_columns = selector_type("numeric"))) %>>%
    lrn("classif.ranger",
        predict_type             = "response",
        num.trees                = 500,
        mtry.ratio               = 0.3,
        min.node.size            = 5,
        sample.fraction          = 0.8,
        importance               = "impurity",
        respect.unordered.factors = "order",  # vermeidet 53-Level-Limit
        seed                     = 42)
  
  glrn <- GraphLearner$new(graph)
  
  
  ## 5-fold stratified CV --------------------------------------------------- ##
  rr  <- mlr3::resample(task, glrn, rsmp("cv", folds = 5))
 library(caret)
  acc <- rr$aggregate(msr("classif.acc"))
  
  results[[this_region]] <- acc
}

## mean regional CV-accuracy
mean_acc <- mean(unlist(results))
print(paste("Mean 5-fold CV accuracy across regions:", round(mean_acc, 4)))

find("resample")
# [1] "package:mlr3" ...

#0,8096 train_imp
#      mice_imputed
