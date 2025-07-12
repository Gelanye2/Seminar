library(data.table)
auto <- readRDS("result/xgb_tuning_model2.rds")
archive <- readRDS("result/xgb_tuning_archive2.rds")

dt <- as.data.table(archive)

# 查看有哪些列可用
colnames(dt)

library(ggplot2)

dt_best <- dt[, .(min_acc = max(classif.acc)), by = batch_nr]  # 如果目标是 accuracy
ggplot(dt_best, aes(x = batch_nr, y = min_acc)) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy Convergence over Batches",
       x = "Batch Number", y = "Best Accuracy")

ggplot(dt, aes(x = `classif.xgboost.eta`, y = classif.acc)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "eta vs Accuracy", x = "eta", y = "accuracy")
ggplot(dt, aes(x = `classif.xgboost.colsample_bytree`, y = classif.acc)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "colsample vs Accuracy", x = "eta", y = "accuracy")
