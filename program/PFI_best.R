library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3pipelines)
library(future)
library(data.table)
library(dplyr)

final_learner <- readRDS("result/model_stacking_multinom8.rds")

plan(multisession, workers = 64)
future::plan(future.seed = TRUE)
set.seed(7832)

# fi_clean <- readRDS("data/fi_clean.rds") # no imp and all
imputed <- readRDS("data/data_imputed_enhanced.rds")
test_all <- readRDS("data/test_all.rds")

# 1. TASK DEFINITION
# Use the imputed dataset for consistency
train_imp <- imputed %>% filter(!is.na(status_group))
test_imp  <- imputed %>% filter(is.na(status_group))

# Ensure target is a factor for classification
train_imp$status_group <- as.factor(train_imp$status_group)

# Create the training task
task <- TaskClassif$new(
  id      = "waterpoints_stacking",
  backend = train_imp,
  target  = "status_group"
)

##PFI
filter = flt("permutation",
             learner = final_learner,
             nmc = 5,
             measure = msr("classif.acc"),
             standardize = FALSE
)
pfi_result <- as.data.table(filter$calculate(task))
saveRDS(pfi_result, "result/pfi_stacking_multinom8.rds")
