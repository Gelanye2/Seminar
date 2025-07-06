###chisq_test contributed by Gelan Ye
chisq_missingness <- function(data, missing_col_name, exclude_vars = NULL) {
  results <- list()

  for (var in names(data)) {
    if (!var %in% c(missing_col_name, exclude_vars)) {
      if (is.factor(data[[var]]) || is.character(data[[var]])) {
        tbl <- table(data[[missing_col_name]], data[[var]])

        if (all(dim(tbl) > 1)) {
          test <- tryCatch(
            chisq.test(tbl),
            error = function(e) NULL
          )
          if (!is.null(test)) {
            results[[var]] <- test$p.value
          }
        }
      }
    }
  }
  return(sort(unlist(results)))
}


### t_test_missingness contributed by Gelan Ye
t_test_missingness <- function(data, missing_col_name, numeric_vars) {
  results <- list()

  for (var in numeric_vars) {
    if (!var %in% c(missing_col_name) && is.numeric(data[[var]])) {
      test <- tryCatch(
        t.test(data[[var]] ~ data[[missing_col_name]]),
        error = function(e) NULL
      )
      if (!is.null(test)) {
        results[[var]] <- test$p.value
      }
    }
  }

  return(sort(unlist(results)))
}


###missingness_map contributed by Gelan Ye
missingness_map <- function(data, missing_col_name, title = "") {
  data <- data %>%
  filter(longitude != 0 & latitude != 0)
  ggplot(data, aes(x = longitude, y = latitude, color = .data[[missing_col_name]])) +
    geom_point(alpha = 0.5) +
    labs(title = title)
}


## --- Clean individual entries ----   contributed by: Yuxin Liu
clean_func <- function(x) {

  # Trim whitespace, convert to lowercase, and coerce to character
  x <- trimws(tolower(as.character(x)))

  # Identify missing-like values and set them to NA
  is_missing   <- x %in% c("", " ", "na", "null", "none") | is.na(x)
  x[is_missing] <- NA

  # Identify unknown-like values and standardize to "unknown"
  is_unknown   <- x %in% c("unknown", "not known", "n/a", "_unknown", "0")
  x[is_unknown] <- "unknown"

  return(x)
}

##get mode
get_mode <- function(x) {
ux <- unique(x)
ux[which.max(tabulate(match(x, ux)))]
}

##imputation:median+mode
imputation <- function(df, values) {
  df %>%
    mutate(
      gps_height     = replace_na(gps_height, values$gps_height),
      years_in_use   = replace_na(years_in_use, values$years_in_use),
      scheme_management = replace_na(scheme_management, values$scheme_management),
      public_meeting    = as.factor(replace_na(public_meeting, values$public_meeting)),
      permit            = as.factor(replace_na(permit, values$permit)),
      installer         = replace_na(installer, values$installer),
      funder            = replace_na(funder, values$funder),
      across(where(is.character), as.factor)
    )
}
