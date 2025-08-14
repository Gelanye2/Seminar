library(data.table)
archive <- readRDS("result/ranger_tuning_archive_l.rds")

dt <- as.data.table(archive)

colnames(dt)

library(ggplot2)

dt_best <- dt[, .(min_acc = max(classif.acc)), by = batch_nr]
ggplot(dt_best, aes(x = batch_nr, y = min_acc)) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy Convergence over Batches",
       x = "Batch Number", y = "Best Accuracy")
