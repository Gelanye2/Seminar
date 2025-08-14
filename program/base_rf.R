.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3pipelines)
library(future)
library(data.table)
library(dplyr)

set.seed(7832)
plan(multisession,workers = 16)

enhance <- readRDS("data/data_enhanced.rds")
regular <- readRDS("data/fi_clean.rds")

train <- enhance %>% filter(!is.na(status_group))
test  <- enhance %>% filter(is.na(status_group))

task <- TaskClassif$new(
  id = "Waterpoints",
  backend = train,
  target = "status_group"
)


# 创建 GraphLearner（含插补 + 模型）
graph <- po_char2fac %>>% po("imputemedian") %>>% po("encode", method = "treatment")%>>% lrn("classif.ranger", predict_type = "prob", num.threads = 7)
glrn <- GraphLearner$new(graph)

# 定义交叉验证策略
resampling <- rsmp("repeated_cv", folds = 5, repeats = 3)
resampling$instantiate(task)

# 交叉验证评估
rr <- mlr3::resample(task, glrn, resampling)

# 聚合评估指标
acc  <- rr$aggregate(msr("classif.acc"))     # Accuracy
bacc <- rr$aggregate(msr("classif.bacc"))    # Balanced Accuracy

cat("ACC RF: ", round(acc, 4), "\n")
cat("BACC RF:", round(bacc, 4), "\n")
