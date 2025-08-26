###contributed by: Haoran Ju

library(purrr)
library(cluster)
set.seed(1)

# Read data
df <- readRDS("data/data_all_spatial0.rds") %>%
  filter(longitude > 25)

# Select variables and cluster labels
X    <- df[, c("population_1k", "longitude", "latitude")]
labs <- df$location_cluster2


# ------- Single sampling -------
n_sample <- 10000                       # Subsample size
idx      <- sample(nrow(X), n_sample)   # Random indices
X_sub    <- scale(X[idx, ])             # Standardize
labs_sub <- as.integer(labs[idx])       # Convert labels to integers if factor/character

sil      <- silhouette(labs_sub, dist(X_sub))
overall  <- mean(sil[, 3])
cat(sprintf("Silhouette (single sampling) = %.3f\n", overall))

# ------- 50 bootstrap replicates -------
B <- 50
overall_vec <- map_dbl(1:B, function(b) {
  idx_b <- sample(nrow(X), n_sample)
  sil_b <- silhouette(as.integer(labs[idx_b]), dist(scale(X[idx_b, ])))
  mean(sil_b[, 3])
})

mean_overall <- mean(overall_vec)
ci95         <- quantile(overall_vec, c(0.025, 0.975))

cat(sprintf("Bootstrap mean silhouette = %.3f\n", mean_overall))
cat(sprintf("95%% CI: [%.3f, %.3f]\n", ci95[1], ci95[2]))
