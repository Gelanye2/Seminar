###contributed by: Haoran Ju

.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
library(data.table); library(dplyr)
library(mlr3); library(mlr3pipelines); library(mlr3learners)
library(mlr3tuning); library(mlr3mbo); library(paradox)
library(future); plan(multisession, workers = 16)
set.seed(7832)

path_data <- "data/data_imputed_enhanced.rds"

#data preparation as other tuning files
df_full <- readRDS(path_data)
train_df <- df_full %>%
  filter(!is.na(status_group)) %>%
  select_if(~ length(unique(.[!is.na(.)])) > 1)

latlon_to_na <- function(v,thr=3e-8){v<-as.numeric(v);v[abs(v)<thr]<-NA_real_;v}
ppipe <- po("colapply",id = "latlon_to_na",param_vals=list(applicator=latlon_to_na,
                                        affect_columns=selector_name(c("longitude","latitude")))) %>>%
  po("mutate", param_vals=list(mutation=list(
    long_missing=~is.na(longitude), lat_missing=~is.na(latitude)))) %>>%
  po("imputeconstant", param_vals=list(
    affect_columns=selector_name(c("longitude","latitude")), constant=-999)) %>>%
  po("colapply", id = "char2factor", param_vals=list(applicator=as.factor,
                                 affect_columns=selector_type("character"))) %>>%
  po("removeconstants") %>>%
  po("encode", method = "treatment")

###############################################################################
# 1️⃣  Level-1  Tuning  (functional vs abnormal)
###############################################################################
task_l1 <- TaskClassif$new("lvl1",
                           backend = train_df %>%
                             mutate(lvl1_target = factor(if_else(status_group=="functional",
                                                                 "functional","abnormal"))) %>%
                             select(-status_group),
                           target   = "lvl1_target",
                           positive = "functional")
task_l1$col_roles$stratum <- "lvl1_target"

xgb_l1 <- lrn("classif.xgboost",
                predict_type = "prob",
                nthread = 7,
                eval_metric = "logloss",
                early_stopping_rounds = 10,
                nrounds = to_tune(upper = 500, internal = TRUE),
                eta = to_tune(0.01,0.1, logscale = TRUE),
                max_depth = to_tune(6,16),
                colsample_bytree = to_tune(0.7, 1),
                subsample = to_tune(0.7, 1),
                lambda = to_tune(1, 5, logscale = TRUE),
                alpha  = to_tune(0.01, 0.5, logscale = TRUE),
                gamma = to_tune(0, 5),
                min_child_weight = to_tune(1,5)
)

graph <- ppipe %>>%
  xgb_l1

base_l1 <-GraphLearner$new(graph)

set_validate(base_l1, validate = "test", ids = "classif.xgboost")

at_l1 <- AutoTuner$new(
  learner     = base_l1,
  resampling  = rsmp("cv", folds = 3),
  measure     = msr("classif.acc"),
  tuner       = tnr("mbo"),
  terminator  = trm("evals", n_evals = 80)
)
at_l1$train(task_l1)
saveRDS(at_l1, "result/at_lvl1.rds")
saveRDS(at_l1$archive, "result/at_lvl1_archive.rds")
cat("✓ Level-1 tuning done • best acc:",
    round(at_l1$archive$best()$classif.acc,4), "\n")

###############################################################################
# 2️⃣  Level-2  Tuning  (needs repair vs non-functional)
###############################################################################
abn_df <- train_df %>% filter(status_group!="functional")
task_l2 <- TaskClassif$new("lvl2",
                           backend = abn_df %>%
                             mutate(lvl2_target = droplevels(status_group)) %>%
                             select(-status_group),
                           target   = "lvl2_target",
                           positive = "functional needs repair")
task_l2$col_roles$stratum <- "lvl2_target"

#balancing the two classes
po_opb <- po("classbalancing", id = "opb",
               param_vals = list(
                 adjust    = "minor",
                 reference = "major",
                 ratio     = 0.3,
                 shuffle   = FALSE
               ))

rf_l2 <- lrn("classif.ranger",
             predict_type   = "prob",
             num.trees      = to_tune(p_int(700, 900)),
             mtry           = to_tune(p_int(3,   5)),
             max.depth      = to_tune(p_int(65,  85)),
             min.node.size  = to_tune(p_int(2,   6)),
             sample.fraction= to_tune(0.95, 1.0),
             num.threads       = 7)


at_l2 <- AutoTuner$new(
  learner     = GraphLearner$new(po_opb %>>% ppipe %>>% rf_l2),
  resampling  = rsmp("cv", folds = 3),
  measure     = msr("classif.acc"),
  tuner       = tnr("mbo"),
  terminator  = trm("evals", n_evals = 80)
)
at_l2$train(task_l2)
saveRDS(at_l2, "result/at_lvl2.rds")
saveRDS(at_l2$archive, "result/at_lvl2_archive.rds")
cat("✓ Level-2 tuning done • best acc:",
    round(at_l2$archive$best()$classif.acc,4), "\n")
