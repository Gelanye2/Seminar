.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
library(mlr3)
library(mlr3pipelines)
library(mlr3learners)
library(dplyr)
library(future)

plan(multisession, workers = 64)
set.seed(7832)

# Daten laden
data_imputed_enhance <- readRDS("data/data_enhanced.rds")
data_imputed_regular <- readRDS("data/train_all_clean.rds")

# Ergebnis-Tabelle vorbereiten
results <- data.frame(
  dataset = character(),
  imputation = character(),
  learner = character(),
  accuracy = numeric(),
  stringsAsFactors = FALSE
)

# task <- TaskClassif$new("reg_mean_ranger", backend = data_imputed_regular %>%
#                           filter(!is.na(status_group)) %>%
#                           mutate(across(where(is.character), as.factor))
#                         , target = "status_group")
#
# learner <- lrn("classif.ranger", predict_type = "prob")
# graph <- po("imputemean") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
# graph_learner <- GraphLearner$new(graph)
# rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3), store_models = FALSE)
# cc <- rr$aggregate(msr("classif.acc"))
# results <- rbind(results, data.frame(dataset="Regular", imputation="Mean", learner="Ranger",accuracy=round(cc, 5)))
#
# learner <- lrn("classif.xgboost", predict_type = "prob")
# graph <- po("imputemean") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
# graph_learner <- GraphLearner$new(graph)
# rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
# cc <- rr$aggregate(msr("classif.acc"))
# results <- rbind(results, data.frame(dataset="Regular", imputation="Mean", learner="XGBoost", accuracy=round(cc, 5)))
#
# learner <- lrn("classif.rpart", predict_type = "prob")
# graph <- po("imputemean") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
# graph_learner <- GraphLearner$new(graph)
# rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
# cc <- rr$aggregate(msr("classif.acc"))
# results <- rbind(results, data.frame(dataset="Regular", imputation="Mean", learner="Rpart", accuracy=round(cc, 5)))
#
# learner <- lrn("classif.kknn", predict_type = "prob")
# graph <- po("imputemean") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
# graph_learner <- GraphLearner$new(graph)
# rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
# cc <- rr$aggregate(msr("classif.acc"))
# results <- rbind(results, data.frame(dataset="Regular", imputation="Mean", learner="KNN", accuracy=round(cc, 5)))
#
#
# task <- TaskClassif$new("reg_median_ranger", backend = data_imputed_regular %>% filter(!is.na(status_group)) %>% mutate(across(where(is.character), as.factor)), target = "status_group")
#
# learner <- lrn("classif.ranger", predict_type = "prob")
# graph <- po("imputemedian") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
# graph_learner <- GraphLearner$new(graph)
# rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
# cc <- rr$aggregate(msr("classif.acc"))
# results <- rbind(results, data.frame(dataset="Regular", imputation="Median", learner="Ranger", accuracy=round(cc, 5)))
#
# learner <- lrn("classif.xgboost", predict_type = "prob")
# graph <- po("imputemedian") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
# graph_learner <- GraphLearner$new(graph)
# rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
# cc <- rr$aggregate(msr("classif.acc"))
# results <- rbind(results, data.frame(dataset="Regular", imputation="Median", learner="XGBoost", accuracy=round(cc, 5)))
#
# learner <- lrn("classif.rpart", predict_type = "prob")
# graph <- po("imputemedian") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
# graph_learner <- GraphLearner$new(graph)
# rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
# cc <- rr$aggregate(msr("classif.acc"))
# results <- rbind(results, data.frame(dataset="Regular", imputation="Median", learner="Rpart", accuracy=round(cc, 5)))
#
# learner <- lrn("classif.kknn", predict_type = "prob")
# graph <- po("imputemedian") %>>% po("imputemode") %>>% po("removeconstants")  %>>% po("encode", method = "treatment") %>>% learner
# graph_learner <- GraphLearner$new(graph)
# rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
# cc <- rr$aggregate(msr("classif.acc"))
# results <- rbind(results, data.frame(dataset="Regular", imputation="Median", learner="KNN",accuracy=round(cc, 5)))


task <- TaskClassif$new("enh_mean_ranger", backend = data_imputed_enhance %>% filter(!is.na(status_group)) %>% mutate(across(where(is.character), as.factor)), target = "status_group")
learner <- lrn("classif.ranger", predict_type = "prob")
graph <- po("imputemean") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
graph_learner <- GraphLearner$new(graph)
rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
cc <- rr$aggregate(msr("classif.acc"))
results <- rbind(results, data.frame(dataset="Enhanced", imputation="Mean", learner="Ranger", accuracy=round(cc, 5)))


learner <- lrn("classif.xgboost", predict_type = "prob")
graph <- po("imputemean") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
graph_learner <- GraphLearner$new(graph)
rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
cc <- rr$aggregate(msr("classif.acc"))
results <- rbind(results, data.frame(dataset="Enhanced", imputation="Mean", learner="XGBoost", accuracy=round(cc, 5)))

learner <- lrn("classif.rpart", predict_type = "prob")
graph <- po("imputemean") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
graph_learner <- GraphLearner$new(graph)
rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
cc <- rr$aggregate(msr("classif.acc"))
results <- rbind(results, data.frame(dataset="Enhanced", imputation="Mean", learner="Rpart", accuracy=round(cc, 5)))


learner <- lrn("classif.kknn", predict_type = "prob")
graph <- po("imputemean") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
graph_learner <- GraphLearner$new(graph)
rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
cc <- rr$aggregate(msr("classif.acc"))
results <- rbind(results, data.frame(dataset="Enhanced", imputation="Mean", learner="KNN", accuracy=round(rr$aggregate(msr("classif.acc")), 5)))


task <- TaskClassif$new("enh_median_ranger", backend = data_imputed_enhance %>% filter(!is.na(status_group)) %>% mutate(across(where(is.character), as.factor)), target = "status_group")
learner <- lrn("classif.ranger", predict_type = "prob")
graph <- po("imputemedian") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
graph_learner <- GraphLearner$new(graph)
rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
cc <- rr$aggregate(msr("classif.acc"))
results <- rbind(results, data.frame(dataset="Enhanced", imputation="Median", learner="Ranger", accuracy=round(rr$aggregate(msr("classif.acc")), 5)))

learner <- lrn("classif.xgboost", predict_type = "prob")
graph <- po("imputemedian") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
graph_learner <- GraphLearner$new(graph)
rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
cc <- rr$aggregate(msr("classif.acc"))
results <- rbind(results, data.frame(dataset="Enhanced", imputation="Median", learner="XGBoost", accuracy=round(rr$aggregate(msr("classif.acc")), 5)))

learner <- lrn("classif.rpart", predict_type = "prob")
graph <- po("imputemedian") %>>% po("imputemode") %>>% po("removeconstants")  %>>% po("encode", method = "treatment") %>>% learner
graph_learner <- GraphLearner$new(graph)
rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
cc <- rr$aggregate(msr("classif.acc"))
results <- rbind(results, data.frame(dataset="Enhanced", imputation="Median", learner="Rpart", accuracy=round(rr$aggregate(msr("classif.acc")), 5)))


learner <- lrn("classif.kknn", predict_type = "prob")
graph <- po("imputemedian") %>>% po("imputemode") %>>% po("removeconstants") %>>% po("encode", method = "treatment") %>>% learner
graph_learner <- GraphLearner$new(graph)
rr <- mlr3::resample(task, graph_learner, rsmp("cv", folds = 3))
cc <- rr$aggregate(msr("classif.acc"))
results <- rbind(results, data.frame(dataset="Enhanced", imputation="Median", learner="KNN", accuracy=round(rr$aggregate(msr("classif.acc")), 5)))

dir.create("results", showWarnings = FALSE)
saveRDS(results, "results/imputation2.rds")
