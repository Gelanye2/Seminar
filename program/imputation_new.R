print(.libPaths())
.libPaths("/media/external/s25_5/Rlibs")

library(future)
plan(multisession)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(dplyr)

set.seed(7832)

# Daten
data_clean <- readRDS("data/fi_clean.rds")
train_data_clean <- data_clean %>% filter(dataset == "train")
drop_cols <- c("longitude", "latitude", 
               "dataset")         
train  <- train_data_clean %>% 
          select(-all_of(drop_cols)) %>% 
         mutate(across(where(is.character), as.factor))

# Task
task <- TaskClassif$new(
  id = "waterpoints",
  backend = train,
  target = "status_group"
)

# Resampling
outer_rsmp = rsmp("cv", folds = 3)

# Lernermodelle 
learners = list(
  ranger = lrn("classif.ranger", predict_type = "prob"),
  xgboost = lrn("classif.xgboost", predict_type = "prob"),
  rpart = lrn("classif.rpart", predict_type = "prob"),
  kknn = lrn("classif.kknn", predict_type = "prob")
)


results = list()

#-------------- 1. Pipeline (mean + mode + encoding) + verschiedene Learner----------
for (name in names(learners)) {
  learner = learners[[name]]
  
  graph = po("imputemean") %>>%
    po("imputemode") %>>%
    po("encode", method = "treatment") %>>%
    learner
  
  graph_learner = GraphLearner$new(graph)
  
  rr = mlr3::resample(task, graph_learner, outer_rsmp, store_models = TRUE)
  
  acc = rr$aggregate(msr("classif.acc")) 
  results[[name]] = acc
}

print(results) #ranger: 0.796211 
               #xgboost 
               #rpart 0.6977634(重新整理列之前)
               #knn:  0.7538386(重新整理列之前)

#-------------- 2. Pipeline (median + mode + encoding) + verschiedene Learner ----------
for (name in names(learners)) {
  learner = learners[[name]]
  
  graph = po("imputemedian") %>>%
    po("imputemode") %>>%
    po("encode", method = "treatment") %>>%
    learner
  
  graph_learner = GraphLearner$new(graph)
  
  rr = mlr3::resample(task, graph_learner, outer_rsmp, store_models = TRUE)
  
  acc = rr$aggregate(msr("classif.acc"))
  results_median[[name]] = acc
}

print(results_median)

#ranger:  0.7965651  
#xgboost: 
#rpart: 0.697696(重新整理列之前)
#knn: 0.7534341(重新整理列之前)




