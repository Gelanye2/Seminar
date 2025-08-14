# ------- 前置 -------
library(purrr)
library(cluster)     # silhouette()
set.seed(1)          # 结果可复现

# 读取数据
df <- readRDS("data/data_all_spatial0.rds") %>%
  filter(longitude > 25)

# 选择变量和聚类标签
X    <- df[, c("population_1k", "longitude", "latitude")]
labs <- df$location_cluster2


# ------- 单次抽样 -------
n_sample <- 10000                       # 子样本大小，可调 5000~20000
idx      <- sample(nrow(X), n_sample)   # 随机下标
X_sub    <- scale(X[idx, ])             # 量纲归一化
labs_sub <- as.integer(labs[idx])       # 如果 labs 是字符/因子要转整数

sil      <- silhouette(labs_sub, dist(X_sub))
overall  <- mean(sil[, 3])
cat(sprintf("一次抽样的 silhouette = %.3f\n", overall))

# ------- 50 次 bootstrap -------
library(purrr)        # map_dbl 辅助
B <- 50
overall_vec <- map_dbl(1:B, function(b) {
  idx_b <- sample(nrow(X), n_sample)
  sil_b <- silhouette(as.integer(labs[idx_b]), dist(scale(X[idx_b, ])))
  mean(sil_b[, 3])
})

mean_overall <- mean(overall_vec)
ci95         <- quantile(overall_vec, c(0.025, 0.975))

cat(sprintf("bootstrap 平均 silhouette = %.3f\n", mean_overall))
cat(sprintf("95%% 置信区间: [%.3f, %.3f]\n", ci95[1], ci95[2]))
