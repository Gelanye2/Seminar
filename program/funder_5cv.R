source("setup.R")

set.seed(7832)
data_cls <<- data_all_clean
data_cls <- data_cls %>% select(-region_code, -district_code, -subvillage,
                                -longitude, -latitude, -ward, -lga, -region_district)

# --- 1. Cleaning function ----
# Trim, lowercase, coerce to character; set missing‐like values to NA; standardize unknowns
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
data_cls$funder <- clean_func(data_cls$funder)
data_cls$installer <- clean_func(data_cls$installer)
df_train <- data_cls %>% filter(dataset == "train")
df_test <- data_cls %>% filter(dataset == "test")

# --- 2. Clustering utilities ---
# 2.1 Evaluate silhouette across thresholds and pick the best non‐NA threshold
evaluate_string_clustering <- function(string_vec, thresholds = seq(0.05, 1.0, by = 0.01)) {
  # Remove NAs and empty strings
  string_vec <- string_vec[!is.na(string_vec) & string_vec != ""]
  
  # Clean strings
  col_vec <- tolower(trimws(as.character(string_vec)))
  col_vec <- gsub("[^a-z0-9 ]", "", col_vec)
  col_vec <- str_squish(col_vec)
  col_vec <- col_vec[col_vec != ""]  # Filter out empty strings again
  unique_vals <- unique(col_vec)
  
  # Compute string distance matrix
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
  
  best_idx_sil <- which.max(result_df$silhouette)
  best_sil_h   <- result_df$threshold[best_idx_sil]
  
  # Compute elbow threshold via max negative slope of n_clusters
  d_counts       <- diff(result_df$n_clusters) / diff(result_df$threshold)
  best_idx_elbow <- which.min(d_counts) + 1
  best_elbow     <- result_df$threshold[elbow_idx]
  
  # Return the result data frame
  list(
    best_sil_h  = best_sil_h,
    best_elbow  = best_elbow
  )
}

# funder, installer
thresh_cutoff_f <- evaluate_string_clustering(df_train$funder)    
thresh_cutoff_i <- evaluate_string_clustering(df_train$installer)

# 2.2 Build a lookup table mapping each original string to its cluster representative
build_group_map <- function(vec_train, sim_threshold = 0.2) {
  # Text cleaning
  col_vec <- tolower(trimws(as.character(vec_train)))
  col_vec <- gsub("[^a-z0-9 ]", "", col_vec)
  col_vec <- str_squish(col_vec)
  col_vec <- col_vec[col_vec != ""] 
  
  # Get unique values and sort by frequency (descending)
  freq_tab <- sort(table(col_vec), decreasing = TRUE)
  unique_vals <- names(freq_tab)
  
  # Compute string distance matrix (Jaro–Winkler distance)
  dmat <- stringdistmatrix(unique_vals, unique_vals, method = "jw")
  dist_obj <- as.dist(dmat)
  
  # Perform hierarchical clustering (average linkage)
  hc <- hclust(dist_obj, method = "average")
  
  # Cut the dendrogram at the specified distance threshold
  groups <- cutree(hc, h = sim_threshold)
  
  # Build lookup table: within each cluster, pick the most frequent string
  dt <- data.table(original = unique_vals, group = groups)
  dt[, freq := as.integer(freq_tab[original])]
  setorder(dt, group, -freq)
  dt[, representative := first(original), by = group]
  
  # Return a table with 'original' and 'representative' columns
  return(dt[, .(original, representative)])
}

# 2.3 Map new strings to their cluster representatives (or keep original)
apply_group_map <- function(vec_new, clustermap, sim_threshold = 0.2) {
  # Text cleaning (must match build_cluster_map exactly)
  new_clean <- tolower(trimws(as.character(vec_new)))
  new_clean <- gsub("[^a-z0-9 ]", "", new_clean)
  new_clean <- str_squish(new_clean)
  
  # Prepare output vector
  rep_out <- rep(NA_character_, length(new_clean))
  
  # Extract training originals and their representatives
  train_originals <- clustermap$original
  train_reps      <- clustermap$representative
  
  # Direct matching: if new_clean[i] exists in train_originals, assign its representative
  idx_direct <- match(new_clean, train_originals)
  rep_out[!is.na(idx_direct)] <- train_reps[idx_direct[!is.na(idx_direct)]]
  
  # For the remaining unmatched values, perform nearest-distance matching
  idx_missing <- which(is.na(rep_out) & new_clean != "")
  if (length(idx_missing) > 0) {
    dsub <- stringdistmatrix(new_clean[idx_missing], train_originals, method = "jw")
    for (j in seq_along(idx_missing)) {
      row_j <- dsub[j, ]
      min_dist <- min(row_j, na.rm = TRUE)
      if (min_dist <= sim_threshold) {
        nearest_idx <- which(row_j == min_dist)[1]
        rep_out[idx_missing[j]] <- train_reps[nearest_idx]
      } else {
        # If the minimum distance exceeds the threshold, keep the cleaned string as is
        rep_out[idx_missing[j]] <- new_clean[idx_missing[j]]
      }
    }
  }
  
  # For values that were originally NA or empty, keep them as NA
  is_na_orig <- is.na(new_clean) | new_clean == ""
  rep_out[is_na_orig] <- NA_character_
  
  return(rep_out)
}

folds <- createFolds(df_train$status_group, k = 5, returnTrain = TRUE)
cand_f <- seq(min(thresh_cutoff_f$best_sil_h, thresh_cutoff_f$best_elbow) - 0.1, 
              min(thresh_cutoff_f$best_sil_h, thresh_cutoff_f$best_elbow) + 0.1, by = 0.01)
cand_i <- seq(min(thresh_cutoff_i$best_sil_h, thresh_cutoff_i$best_elbow) - 0.1, 
              min(thresh_cutoff_i$best_sil_h, thresh_cutoff_i$best_elbow) + 0.1, by = 0.01)

tune_threshold <- function(df, col, candidates, folds) {
  accs <- sapply(candidates, function(h) {
    fold_acc <- sapply(folds, function(train_idx) {
      val_idx <- setdiff(seq_len(nrow(df)), train_idx)
      dtr <- df[train_idx, ]
      dval <- df[val_idx, ]
      for (colname in names(dtr)) {
        if (is.logical(dtr[[colname]])) {
          dtr[[colname]] <- factor(dtr[[colname]], levels = c(FALSE, TRUE))
        }
      }
      for (colname in names(dval)) {
        if (is.logical(dval[[colname]])) {
          dval[[colname]] <- factor(dval[[colname]], 
                                      levels = levels(dtr[[colname]]))
        }
      }
      map  <- build_group_map(dtr[[col]], sim_threshold = h)
      dtr[[col]]  <- apply_group_map(dtr[[col]],  map, h)
      dval[[col]]<- apply_group_map(dval[[col]],map, h)
      for (colname in names(dtr)) {
        if (is.character(dtr[[colname]])) {
          dtr[[colname]] <- factor(dtr[[colname]])
        }
      }
      for (colname in names(dval)) {
        if (is.character(dval[[colname]])) {
          dval[[colname]] <- factor(
            dval[[colname]],
            levels = levels(dtr[[colname]])
          )
        }
      }
      task_tr  <- TaskClassif$new("tr", backend = dtr,   target = "status_group")
      task_val <- TaskClassif$new("va", backend = dval,  target = "status_group")
      lrn <- lrn("classif.lightgbm", predict_type = "response")
      lrn$train(task_tr)
      pred <- lrn$predict(task_val)$response
      mean(pred == dval$status_group)
    })
    acc <- mean(fold_acc)
    cat(sprintf("Threshold = %.2f | Mean CV ACC = %.4f\n", 
                h, acc))
    acc
  })
  candidates[which.max(accs)]
}

best_h_f <- tune_threshold(df_train, "funder",    cand_f, folds)
best_h_i <- tune_threshold(df_train, "installer", cand_i, folds)

map_f <- build_group_map(df_train$funder,    sim_threshold = best_h_f)
map_i <- build_group_map(df_train$installer, sim_threshold = best_h_i)

df_train$funder  <- apply_group_map(df_train$funder,
                                    clustermap = map_f, sim_threshold = best_h_f)
df_test$funder <- apply_group_map(df_test$funder,
                                  clustermap = map_f, sim_threshold = best_h_f)
df_train$funder  <- factor(df_train$funder)
df_test$funder <- factor(df_test$funder, 
                         levels = levels(df_train$funder))

df_train$installer  <- apply_group_map(df_train$installer,
                                    clustermap = map_i, sim_threshold = best_h_i)
df_test$installer <- apply_group_map(df_test$installer,
                                  clustermap = map_i, sim_threshold = best_h_i)
df_train$installer  <- factor(df_train$installer)
df_test$installer <- factor(df_test$installer, 
                         levels = levels(df_train$installer))

df_test$status_group <- NA
df_combined <- rbind(df_train, df_test)

# --- 3. Keyword mapping and rare grouping ---
# Full keyword map as provided
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
                   "lake tanganyika", "national rural", "tanza", "go"),
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

df_combined$funder <- assign_by_keywords(df_combined$funder, keyword_map)
df_combined$installer <- assign_by_keywords(df_combined$installer, keyword_map)

# Lump rare categories below a frequency threshold into "other"
group_rare <- function(x, min_freq = 20, other_label = "other") {
  # Coerce to character for counting
  x_char <- as.character(x)
  
  # Compute frequencies
  freq_tab <- table(x_char)
  
  # Identify levels to lump
  rare_levels <- names(freq_tab)[freq_tab < min_freq]
  
  # Recode: if in rare_levels, set to other_label
  x_recoded <- ifelse(x_char %in% rare_levels, other_label, x_char)
  
  # Return as factor, with Other level first
  levels_final <- c(other_label, sort(unique(setdiff(x_recoded, other_label))))
  factor(x_recoded, levels = levels_final)
}

df_combined$funder <- group_rare(df_combined$funder, 
                                 min_freq = 20, other_label = "other")
df_combined$installer <- group_rare(df_combined$installer, 
                                 min_freq = 20, other_label = "other")
saveRDS(df_combined, "data/df_combined.rds")
