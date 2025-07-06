source("setup.R")
library(mlr3verse)
library(mlr3pipelines)
library(mlr3tuning)
library(paradox)
library(paradox)

source("setup.R")
fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed.rds") # all:train + test
sub_imputed <- readRDS("data/sub_imputed.rds") # train + test without longtitude and latitude
test_full_imp <- readRDS("data/test_full_imp.rds")
test_all <- readRDS("data/test_all.rds")

# ---- Daten vorbereiten ----
df_imp <<- imputed
train_imp <- df_imp %>% filter(!is.na(status_group))
test_imp  <<- test_full_imp
test_imp$status_group <- factor(NA, levels = levels(train_imp$status_group))

# ---- Task definieren ----
task_imp <- TaskClassif$new(
  id      = "Waterpoints",
  backend = train_imp,
  target  = "status_group"
)


# ---- Pipeline Graph ----
graph <- 
  po("colapply", param_vals = list(
    applicator = function(x) if (is.character(x)) as.factor(x) else x,
    affect_columns = selector_type("character")
  )) %>>%
  po("encode", method = "treatment") %>>%
  po("removeconstants") %>>%
  lrn("classif.ranger",
      predict_type = "response"
  )

# GraphLearner
glrn <- GraphLearner$new(graph)

param_set <- ps(
  classif.ranger.num.trees     = p_int(300, 600),
  classif.ranger.mtry          = p_int(10, 30),
  classif.ranger.max.depth     = p_int(15, 40),
  classif.ranger.min.node.size = p_int(3, 15)
)

# ---- AutoTuner ----
at_ranger <- AutoTuner$new(
  learner = glrn,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.acc"),
  terminator = trm("evals", n_evals = 100),
  tuner = tnr("grid_search", resolution = 5),
  search_space = param_set
)

# Trainieren auf Trainingsdaten
set.seed(42)
at_ranger$train(task_imp)

# Beste Parameter einsehen
at_ranger$tuning_result

# Cross-Validated Accuracy mit best. Parametern
resampling_final <- mlr3::resample(task_imp, at_ranger, rsmp("cv", folds = 5))
acc <- resampling_final$aggregate(msr("classif.acc"))
bacc <- resampling_final$aggregate(msr("classif.bacc"))

# Beste Parameter aus dem getunten Modell
best_vals <- at_ranger$glrn$model$param_set$values
print(best_vals)