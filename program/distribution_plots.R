library(ggplot2)

###population
# log1p: calculate log(population + 1)
data_all$log_pop <- log1p(data_all$population)

#find the median value
med_log <- median(data_all$log_pop, na.rm = TRUE)
med_raw <- exp(med_log) - 1

#histogram
ggplot(data_all, aes(x = log_pop)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  geom_vline(xintercept = med_log, color = "red", size = 1) +
  annotate("text", x = med_log + 0.2, y = 20000,
           label = paste0("Median: ", round(med_log, 3),
                          " (logscale) or ", round(med_raw)),
           color = "red", hjust = 0) +
  labs(title = "Population (log scale + 1) Feature",
       x = "Population", y = "Count") +
  theme_minimal()

ggsave("plot/population_log_histogram.png", width = 8, height = 6)


###amount_tsh
# log1p: log(amount_tsh + 1)
data_all$log_tsh <- log1p(data_all$amount_tsh)

# median
med_log <- median(data_all$log_tsh, na.rm = TRUE)
med_raw <- exp(med_log) - 1

ggplot(data_all, aes(x = log_tsh)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  geom_vline(xintercept = med_log, color = "red", size = 1) +
  annotate("text",
           x = med_log + 0.3,
           y = 20000,
           label = paste0("Median: ", round(med_log, 3),
                          " (logscale) or ", round(med_raw)),
           color = "red", hjust = 0) +
  labs(title = "Amount TSH (log scale + 1)",
       x = "log(amount_tsh + 1)",
       y = "Count")

