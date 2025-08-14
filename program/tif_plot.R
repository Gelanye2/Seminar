library(terra)

r <- rast("data/tza_ppp_2012.tif")

png("plot/tif_2012.png", width = 800, height = 800, res = 300)
p1 <- plot(r,
     type = "classes",
     legend = FALSE,
     axes = FALSE,
     main = "",
     frame = FALSE,
     mar = c(0, 0, 0, 0))


dev.off()

df <- readRDS("data/data_enhanced.rds")

library(ggplot2)
library(dplyr)

df <- df %>%
  mutate(diff = population_1k - population)

ggplot(df, aes(x = "", y = diff)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
       y = "Difference between Worldpop and Dataset",
       x = "") +
  theme_minimal()
ggsave("plot/population_diff_boxplot.png", width = 8, height = 6, dpi = 300)

ggplot(df, aes(x = population, y = population_1k)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Original Population",
    y = "WorldPop-enhanced Population",
    title = "Original vs. WorldPop-enhanced Population"
  ) +
  theme_minimal()
ggsave("plot/population_diff_scatter.png", width = 8, height = 6, dpi = 300)
