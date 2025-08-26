#Contributed by: Gelan Ye
source("setup.R")

##-----random forest with mbo----------

# Load data
mbo <- readRDS("result/ranger_tuning_archive_l.rds")

# Prepare data
dt <- as.data.table(mbo)
setorder(dt, batch_nr)

# Calculate best-so-far accuracy
dt[, best_so_far := cummax(classif.acc)]

# Identify improvement points
dt[, improvement := c(FALSE, diff(best_so_far) > 0)]

# Plot
p_rf <- ggplot() +
  # Raw accuracy as points (black)
  geom_point(data = dt, aes(x = batch_nr, y = classif.acc),
             color = "black", alpha = 0.4, size = 0.8) +

  # Raw accuracy as line (orange)
  geom_line(data = dt, aes(x = batch_nr, y = classif.acc),
            color = "orange", alpha = 0.8, linewidth = 0.4) +

  # Best-so-far line (black)
  geom_step(data = dt, aes(x = batch_nr, y = best_so_far),
            color = "black", linewidth = 1) +

  # Red triangles for improvement
  geom_point(data = dt[improvement == TRUE],
             aes(x = batch_nr, y = classif.acc),
             color = "red", shape = 17, size = 2.2) +

  labs(
    x = "Evaluation step",
    y = "Accuracy"
  ) +
  theme_minimal()

ggsave("plot/rf_convergence_plot.png",
       plot = p_rf,
       width = 6.5, height = 4.5, dpi = 300)
#--xgboost with mbo---
xgb <- readRDS("result/xgb_tuning_archive_l.rds")
dt <- as.data.table(xgb)
setorder(dt, batch_nr)

# Cumulative best performance (highest accuracy so far)
dt[, best_so_far := cummax(classif.acc)]

# New column: indicates whether this step is an improvement
dt[, improvement := classif.acc == best_so_far]

# For geom_line: select only the last row for each new best accuracy
dt_best <- dt[, .SD[.N], by = best_so_far]

p_xgb <- ggplot() +
  # Raw accuracy as points (black)
  geom_point(data = dt, aes(x = batch_nr, y = classif.acc),
             color = "black", alpha = 0.4, size = 0.8) +

  # Raw accuracy as line (orange)
  geom_line(data = dt_best, aes(x = batch_nr, y = classif.acc),
            color = "orange", alpha = 0.8, linewidth = 0.4) +

  # Best-so-far accuracy as a step function (black)
  geom_step(data = dt_best, aes(x = batch_nr, y = best_so_far),
            color = "black", linewidth = 1) +

  # Mark improvements (red triangles)
  geom_point(data = dt_best[improvement == TRUE],
             aes(x = batch_nr, y = classif.acc),
             color = "red", shape = 17, size = 2.2) +

  # Axis labels and title
  labs(
    x = "Evaluation step",
    y = "Accuracy",
    title = "Convergence (XGBoost tuning: raw accuracy + best-so-far)"
  ) +
  theme_minimal()

ggsave("plot/xgb_convergence_plot.png",
       plot = p_xgb,
       width = 6.5, height = 4.5, dpi = 300)
