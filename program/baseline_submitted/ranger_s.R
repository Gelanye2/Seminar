###contributed by: Gelan Ye, Haoran Ju

.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
# 0. SETUP AND DATA LOADING
# ==================================
# Ensure required packages are loaded
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3pipelines)
library(future)
library(data.table)
library(dplyr)
library(mlr3filters)

plan(multisession, workers = 16)
future::plan(future.seed = TRUE)
set.seed(7832)

data_imputed<- readRDS("data/data_imputed_enhanced.rds")
train <- data_imputed %>% filter(!is.na(status_group))
test  <- data_imputed %>% filter(is.na(status_group))
test_all <- readRDS("data/test_all.rds")

# === Task definieren ===
task_imp <- TaskClassif$new(
  id = "Waterpoints",
  backend = train,
  target = "status_group"
)
graph2 <- lrn("classif.ranger", predict_type = "prob", num.threads = 7)

glrn <- GraphLearner$new(graph2)

resampling <- rsmp("repeated_cv", folds = 5, repeats = 3)
rr_imp <- mlr3::resample(task_imp, glrn, resampling, store_models = FALSE)

# === Metriken anzeigen ===
acc <- rr_imp$aggregate(msr("classif.acc"))
bacc <- rr_imp$aggregate(msr("classif.bacc"))

cat("🔹 Accuracy:          ", round(acc, 5), "\n")
cat("🔹 Balanced Accuracy: ", round(bacc, 5), "\n")

glrn$train(task_imp)
task_test_imp <- TaskClassif$new(
  id      = "task_test_imp",
  backend = test,
  target  = "status_group"
)

pred_test_imp <- glrn$predict(task_test_imp)

test$status_group <- pred_test_imp$response

result_imp_ranger_imputed <- data.frame(
  id           = test_all$id,
  status_group = test$status_group,
  stringsAsFactors = FALSE
)

write.csv(result_imp_ranger_imputed, "ranger_baseline.csv", row.names = FALSE)
