###contributed by: Gelan Ye, Haoran Ju
.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3pipelines)
library(data.table)
library(dplyr)

set.seed(7832)

#read data
enhanced <- readRDS("data/data_enhanced.rds") %>% filter(!is.na(status_group))
regular  <- readRDS("data/fi_clean.rds")       %>% filter(!is.na(status_group)) %>% select(-dataset)

to_factor <- function(df) {
  df %>%
    mutate(
      across(where(is.character), as.factor),
      across(where(is.logical),   ~factor(.x, levels = c(FALSE, TRUE)))
    )
}

enhanced <- to_factor(enhanced)
regular  <- to_factor(regular)

#create tasks
task_enhanced <- TaskClassif$new(id = "enhanced_imputed", backend = enhanced, target = "status_group")
task_regular  <- TaskClassif$new(id = "regular_imputed",  backend = regular,  target = "status_group")


graph <- po("imputemedian") %>>% po("imputemode") %>>%  lrn("classif.ranger", predict_type = "prob", num.threads = 7)

glrn <- GraphLearner$new(graph)

#resampling 3*5cv
resampling <- rsmp("repeated_cv", folds = 5, repeats = 3)

#benchmarking
design <- benchmark_grid(
  tasks       = list(task_regular, task_enhanced),
  learners    = list(glrn),
  resamplings = list(resampling)
)

bmr <- benchmark(design)

#save doc
agg <- bmr$aggregate(msrs(c("classif.acc", "classif.bacc")))
saveRDS(agg, "result/bs_imp_rf.rds")

