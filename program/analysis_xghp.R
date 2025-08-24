library(data.table)

#read the archive during the process
auto <- readRDS("result/xgb_tuning_model_l.rds")
archive <- readRDS("result/xgb_tuning_archive_l.rds")

dt <- as.data.table(archive)

colnames(dt)

library(ggplot2)

dt_best <- dt[, .(min_acc = max(classif.acc)), by = batch_nr]
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
  labs(title = "colsample vs Accuracy", x = "col_sample", y = "accuracy")

ggplot(dt, aes(x = `classif.xgboost.max_depth`, y = classif.acc)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "max_depth vs Accuracy", x = "max_depth", y = "accuracy")

ggplot(dt, aes(x = `internal_tuned_values_classif.xgboost.nrounds`, y = classif.acc)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "nrounds vs Accuracy", x = "nrounds", y = "accuracy")

# library(GGally)
#
# cols_hp1 <- c("classif.xgboost.eta",
#              "classif.xgboost.alpha",
#              "classif.xgboost.gamma",
#              "classif.xgboost.lambda",
#              "classif.xgboost.min_child_weight",
#              "classif.xgboost.subsample",
#              "classif.xgboost.max_depth",
#              "classif.xgboost.colsample_bytree")
#
# ggpairs(dt[, cols_hp, with = FALSE])
#
# pr <- prcomp(dt[, ..cols_hp], center = TRUE, scale. = TRUE)
#
# plot(pr$x[, 1:2],
#      col = scales::col_numeric("Blues", NULL)(dt$classif.acc),
#      pch = 19,
#      main = "PCA of Hyperparameters (colored by Accuracy)")
#
# summary(pr)
# pr$rotation
#

