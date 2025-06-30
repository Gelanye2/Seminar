##for working station
options(repos = c(CRAN = "https://cran.rstudio.com/"))
.libPaths("/media/external/s25_5/Rlibs")
packages <- c(
  "data.table", "mlr3", "mlr3pipelines", "mlr3learners",
  "sf", "ggplot2", "ggrepel", "rnaturalearth", "rnaturalearthdata", "pROC",
  "patchwork", "stringdist", "stringr", "spdep", "xgboost", "mlr3verse",
  "readr", "ranger", "cluster", "clusterCrit", "dplyr", "caret",
  "lightgbm", "paradox", "terra", "purrr","gbm","kknn","remotes"
)

installed <- rownames(installed.packages())
for (p in packages) {
  if (!(p %in% installed)) {
    install.packages(p, dependencies = TRUE)
  }
}

remotes::install_github("mlr-org/mlr3extralearners")
