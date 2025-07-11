source("setup.R")
library(mlr3verse)
library(mlr3pipelines)
library(mlr3tuning)
library(paradox)
library(paradox)
library(mlr3mbo)

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

task_imp$col_roles$stratum <- task_imp$target_names

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

graph <- po_latlon_na %>>%
  po_latlon_flag %>>%
  po_latlon_impute %>>%
  po_char2fac %>>%
  po("removeconstants") %>>%
  lrn("classif.ranger", 
      predict_type = "response")
  

# GraphLearner
glrn <- GraphLearner$new(graph)

param_set <- ps(
  classif.ranger.num.trees     = p_int(900, 1600),
  classif.ranger.mtry          = p_int(lower = 3, upper = 7),
  classif.ranger.max.depth     = p_int(40, 80),
  classif.ranger.min.node.size = p_int(1, 10),
  classif.ranger.sample.fraction = p_dbl(0.4, 1.0)
)

#####

# ---- AutoTuner ----
at_ranger <- AutoTuner$new(
  learner = glrn,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.acc"),
  terminator = trm("evals", n_evals = 20),
  tuner = tnr("mbo"),
  search_space = param_set
)

# Trainieren auf Trainingsdaten

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

# Ergebnis als Liste speichern
result_list <- list(
  accuracy          = acc,
  balanced_accuracy = bacc,
  best_params       = best_vals
)

# Optional: Als RDS speichern
saveRDS(result_list, "data/ranger_tuned_result_summary.rds")

