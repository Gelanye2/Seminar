###contributed by: Yuxin Liu

source("setup.R")

data_all_clean <- readRDS("data/data_all_clean.rds")
data_clean <- data_all_clean %>%
  mutate(row_id = row_number())

# --- Clean individual entries ----
clean_func <- function(x) {

  # Trim whitespace, convert to lowercase, and coerce to character
  x <- trimws(tolower(as.character(x)))

  # Identify missing-like values and set them to NA
  is_missing   <- x %in% c("", " ", "na", "null", "none", "n/a") | is.na(x)
  x[is_missing] <- NA

  # Identify unknown-like values and standardize to "unknown"
  is_unknown   <- x %in% c("unknown", "not known", "_unknown", "0")
  x[is_unknown] <- "unknown"

  return(x)
}

data_clean$funder <- clean_func(data_clean$funder)
data_clean$installer <- clean_func(data_clean$installer)
data_clean$scheme_name <- clean_func(data_clean$scheme_name)

# --- Evaluation ----
evaluate_string_clustering <- function(string_vec, thresholds = seq(0.05, 0.5, by = 0.01)) {
  # Remove NAs and empty strings
  string_vec <- string_vec[!is.na(string_vec) & string_vec != ""]

  # Clean strings
  col_vec <- tolower(trimws(as.character(string_vec)))
  col_vec <- gsub("[^a-z0-9 ]", "", col_vec)
  col_vec <- str_squish(col_vec)
  col_vec <- col_vec[col_vec != ""]  # Filter out empty strings again
  unique_vals <- unique(col_vec)

  # Compute string distance matrix
  # Jaro–Winkler distance
  dist_mat <- stringdistmatrix(unique_vals, unique_vals, method = "jw")
  dist_obj <- as.dist(dist_mat)

  # Perform hierarchical clustering
  hc <- hclust(dist_obj, method = "average")

  # Initialize result storage
  result_list <- list()

  for (h in thresholds) {
    labels <- cutree(hc, h = h)

    # Silhouette score
    sil_score <- NA
    if (length(unique(labels)) > 1) {
      sil_score <- mean(silhouette(labels, dist_obj)[, "sil_width"])
    }

    # Number of clusters
    n_clusters <- length(unique(labels))

    result_list[[as.character(h)]] <- data.frame(
      threshold = h,
      silhouette = sil_score,
      n_clusters = n_clusters
    )
  }

  # Combine results into a data frame
  result_df <- do.call(rbind, result_list)
  rownames(result_df) <- NULL
  # Return the result data frame
  return(result_df)
}

data_funder <- evaluate_string_clustering(data_clean$funder)
data_installer <- evaluate_string_clustering(data_clean$installer)
best_thres_f   <- data_funder$threshold[which.max(data_funder$silhouette)]
best_thres_i   <- data_installer$threshold[which.max(data_installer$silhouette)]

data_scheme <- evaluate_string_clustering(data_clean$scheme_name)
best_thres_s   <- data_scheme$threshold[which.max(data_scheme$silhouette)]

plot_string_clustering <- function(result_df) {
  # Compute best‐Silhouette threshold
  best_idx_sil   <- which.max(result_df$silhouette)
  best_sil       <- result_df$threshold[best_idx_sil]

  # Compute elbow threshold via max negative slope of n_clusters
  d_counts       <- diff(result_df$n_clusters) / diff(result_df$threshold)
  best_idx_elbow <- which.min(d_counts) + 1
  best_elbow     <- result_df$threshold[best_idx_elbow]

  # Silhouette vs Threshold
  p1 <- ggplot(result_df, aes(x = threshold, y = silhouette)) +
    geom_point(color = "darkgreen") +
    geom_point(
      data = result_df[best_idx_sil, ],
      aes(threshold, silhouette),
      color = "yellow", size = 3, shape = 19
    ) +
    geom_text_repel(
      data = result_df[best_idx_sil, ],
      aes(threshold, silhouette, label = sprintf("%.2f", best_sil)),
      color   = "black",
      size    = 4
    ) +
    labs(
      title = "Silhouette Score vs Threshold",
      x     = "Threshold",
      y     = "Silhouette"
    ) +
    theme_minimal()

  # Cluster Count vs Threshold
  p2 <- ggplot(result_df, aes(x = threshold, y = n_clusters)) +
    geom_point(color = "#34495e") +
    geom_point(
      data = result_df[best_idx_elbow, ],
      aes(threshold, n_clusters),
      color = "yellow", size = 3, shape = 19
    ) +
    geom_text_repel(
      data = result_df[best_idx_elbow, ],
      aes(threshold, n_clusters, label = sprintf("%.2f", best_elbow)),
      color   = "black",
      size    = 4
    ) +
    labs(
      title = "Cluster Count vs Threshold",
      x     = "Threshold",
      y     = "Number of Clusters"
    ) +
    theme_minimal()

  # Combine side by side
  p1 + p2 + plot_layout(ncol = 2)
}

plot_string_clustering(data_funder)
plot_string_clustering(data_installer)
plot_string_clustering(data_scheme)

# --- group ----
group_similar_categories <- function(data, column, sim_threshold = 0.2, new_col = NULL) {
  # Check that the specified column exists
  stopifnot(column %in% names(data))

  # Preprocess the text column: lowercase, trim whitespace, remove non-alphanumeric
  col_vec <- tolower(trimws(as.character(data[[column]])))
  col_vec <- gsub("[^a-z0-9 ]", "", col_vec)
  col_vec <- str_squish(col_vec)

  # Get unique values and their frequency counts
  col_freq <- sort(table(col_vec), decreasing = TRUE)
  unique_vals <- names(col_freq)

  # Compute string distance matrix and perform hierarchical clustering
  dist_mat <- stringdistmatrix(unique_vals, unique_vals, method = "jw")
  hc <- hclust(as.dist(dist_mat), method = "average")
  groups <- cutree(hc, h = sim_threshold)

  # Build a lookup table of cluster representatives (most frequent per group)
  cluster_dt <- data.table(original = unique_vals, group = groups)
  cluster_dt[, freq := col_freq[original]]
  cluster_dt <- cluster_dt[order(group, -freq)]
  cluster_dt[, representative := first(original), by = group]

  # Merge representative values back into the original data
  # .__tmp_cleaned holds the cleaned original values,
  # while representative stores the matched cluster representatives;
  # they are linked via a merge to form a mapping from original to representative strings.
  data$.__tmp_cleaned <- col_vec
  replace_map <- cluster_dt[, .(original, representative)]
  data <- merge(data, replace_map, by.x = ".__tmp_cleaned", by.y = "original", all.x = TRUE)

  # Replace NA representatives with the original cleaned value
  data$representative[is.na(data$representative)] <- data$.__tmp_cleaned[is.na(data$representative)]

  # Create the output column name and assign the representative values
  data[[ifelse(is.null(new_col), paste0(column, "_grouped"), new_col)]] <- data$representative

  # # Report any values that did not match a representative
  # missing_map <- unique(data$.__tmp_cleaned[is.na(data$representative)])
  # cat("The following values did not match any cluster representative:\n")
  # print(missing_map)

  # Clean up temporary columns
  data$.__tmp_cleaned <- NULL
  data$representative <- NULL

  return(data)
}

data_clean <- group_similar_categories(
  data = data_clean,
  column = "funder",
  sim_threshold = best_thres_f
)
n_distinct(data_clean$funder_grouped)
data_clean$funder <- data_clean$funder_grouped
data_clean$funder_grouped <- NULL

data_clean <- group_similar_categories(
  data = data_clean,
  column = "installer",
  sim_threshold = best_thres_i
)
n_distinct(data_clean$installer_grouped)
data_clean$installer <- data_clean$installer_grouped
data_clean$installer_grouped <- NULL

data_clean <- group_similar_categories(
  data = data_clean,
  column = "scheme_name",
  sim_threshold = best_thres_s
)
n_distinct(data_clean$scheme_name_grouped)
data_clean$scheme_name <- data_clean$scheme_name_grouped
data_clean$scheme_name_grouped <- NULL

# --- Assign categories based on keyword patterns ----
keyword_map <- list(
  un_agency    = c("unicef", "undp", "unhcr", "^un[ _]", "un-habitat", "^uni", "un"),
  ngo          = c("oxfam", "care", "world vision", "red cross", "engineers without border",
                   "engin", "international aid services", "redeso", "total land care",
                   "africa amini alama", "concern", "ngos", "women for partnership"),
  foreign_gov  = c("germany", "china", "finland", "holland",
                   "netherlands", "usa", "\\bjapan\\b", "korea", "\\bbritain\\b",
                   "\\begypt\\b", "us", "african development foundation",
                   "african realief committe of ku", "frankfurt", "england$", "foreigne",
                   "italy$", "swedish", "swiss if"),
  church       = c("^kkkt", "^rc ", "lutheran", "church", "cct", "diocese", "^roman", "rc$"),
  government   = c("government", "\\bgovt?\\b", "district", "halmashauri", "cg",
                   "cental government", "central government", "centr",
                   "central basin", "concern government", "tanzania",
                   "lake tanganyika", "national rural", "tanza"),
  pet = c("^pet"),
  school = c("\\bschool\\b", "scott", "sda", "secondary", "college",
             "institute$", "instute$", "institution", "school$"),
  sema = c("^sema"),
  tlc_group    = c("^tlc"),
  local_village = c("^village", "villagers", "village council", "village community",
                    "village government", "village water committee", "village$"),
  world_bank = c("world bank", "wb", "bank"),
  water_dep = c("^water", "^wd", "rudep", "rural water department",
                "rural water supply and sanitat", "water$", "lawatefuka water supply",
                "ministry of water"),
  rwssp = c("^rwssp"),
  twesa = c("^twesa"),
  private_company = c("ltd$", "co$", "company$", "contractor", "enterprises?", "companies$",
                      "consultant$", "cons", "compa", "club$", "com$"),
  africa = c("af", "africa"),
  private_person   = c("^(mr|mrs|mzee|dr|prof)", "private", "private individuals", "private technician", "pr",
                       "people p", "individuals", "bongkug ohhchoonlza lee"),
  mission = c("mission$", "missions$"),
  mama = c("^mama"),
  local_dep = c("^local"),
  dwe = c("^dwe"),
  dh = c("^dh"),
  action = c("^action", "^active"),
  aic = c("^aic"),
  ir = c("ir", "iran govern", "irevea sister"),
  is = c("^is"),
  md = c("^md"),
  wfp = c("^wfp"),
  do = c("^do")
)

assign_by_keywords <- function(vec, keyword_map) {

  # Clean input vector to lowercase trimmed strings
  vec_clean <- tolower(trimws(as.character(vec)))

  result <- rep(NA_character_, length(vec_clean))

  # Loop through each category and its patterns
  for (cat in names(keyword_map)) {
    patterns <- keyword_map[[cat]]
    match_index <- Reduce(`|`, lapply(patterns, function(p) grepl(p, vec_clean, perl = TRUE)))
    result[is.na(result) & match_index] <- cat
  }

  # keep the original cleaned value
  result[is.na(result)] <- vec_clean[is.na(result)]
  return(result)
}

data_clean$funder <- assign_by_keywords(data_clean$funder, keyword_map)
data_clean$installer <- assign_by_keywords(data_clean$installer, keyword_map)
n_distinct(data_clean$funder)
n_distinct(data_clean$installer)

# --- Recode rare categories as 'other' ----
group_rare <- function(x, target_prop = 0.1, other_label = "other") {
  # Convert to character vector
  x_char <- as.character(x)
  total_count <- length(x_char)
  target_count <- total_count * target_prop

  # Frequency table (ascending by default)
  freq_tab <- sort(table(x_char))

  # Cumulative sum of frequencies
  cum_freq <- cumsum(freq_tab)

  # Identify levels to lump into 'Other'
  rare_levels <- names(freq_tab)[cum_freq <= target_count]

  # Recode rare levels
  x_recoded <- ifelse(x_char %in% rare_levels, other_label, x_char)

  # Return as factor, with 'Other' as the first level
  levels_final <- c(other_label, sort(unique(setdiff(x_recoded, other_label))))
  factor(x_recoded, levels = levels_final)
}

data_clean$funder <- group_rare(data_clean$funder)
data_clean$installer <- group_rare(data_clean$installer)

n_distinct(data_clean$funder)
n_distinct(data_clean$installer)

data_clean <- data_clean %>%
  arrange(row_id) %>%
  select(-row_id)

saveRDS(data_clean, "data/fi_clean_year.rds")
saveRDS(data_clean, "data/fi_clean.rds")

