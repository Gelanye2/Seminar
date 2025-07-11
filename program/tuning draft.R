# ===== 加载必要包 =====
library(mlr3)
library(mlr3pipelines)
library(mlr3learners)
library(mlr3tuning)
library(paradox)
library(future)
library(mlr3mbo)
plan(multisession, workers = 2)
set.seed(42)

# ===== 用 iris 替代原数据 =====
task_ll <- tsk("iris")
task_ll$col_roles$stratum <- task_ll$target_names

# ===== 构建 GraphLearner（保持结构）=====
graph <- po("colapply", param_vals = list(
  applicator     = function(x) if (is.character(x)) as.factor(x) else x,
  affect_columns = selector_type("character")
)) %>>%
  po("removeconstants") %>>%
  po("encode", method = "treatment") %>>%
  lrn("classif.xgboost",
      predict_type = "prob",
      nthread = 2,
      eval_metric = "mlogloss",
      early_stopping_rounds = 10,
      nrounds = to_tune(upper = 100, internal = TRUE),
      eta = to_tune(0.03, 0.1, logscale = TRUE),
      max_depth = to_tune(2, 4)
  )

base_lrn <- GraphLearner$new(graph)
set_validate(base_lrn, validate = "test", ids = "classif.xgboost")

# ===== AutoTuner结构不变，极简设置调参轮数 =====
auto <- AutoTuner$new(
  learner = base_lrn,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.acc"),
  tuner = tnr("grid_search"),
  terminator = trm("evals", n_evals = 2)
)

# ===== 启动训练，验证是否可跑通 =====
  auto$train(task_ll)

rr <- resample(task_ll, auto, rsmp("cv", folds = 2))
acc  <- rr$aggregate(msr("classif.acc"))
bacc <- rr$aggregate(msr("classif.bacc"))
best_params <- auto$learner$param_set$values

result_list <- list(
  acc         = acc,
  bacc        = bacc,
  best_params = best_params
)

print(result_list)

library(ggplot2)
library(data.table)

arch <- as.data.table(auto$archive$data)

# 保证 eta 是 numeric
arch[, classif.xgboost.eta := as.numeric(classif.xgboost.eta)]

ggplot(arch, aes(x = classif.xgboost.eta, y = factor(classif.xgboost.max_depth), fill = classif.acc)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Grid Search Performance",
       x = "eta",
       y = "max_depth",
       fill = "Accuracy") +
  theme_minimal()

auto$archive


