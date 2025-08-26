# Contributed by: Gelan Ye

# --- Load cleaned data ---
data_clean <- readRDS("data/fi_clean.rds")

# Separate training and test sets
train_data_clean <- data_clean %>% filter(dataset == "train")
test_data_clean  <- data_clean %>% filter(dataset == "test")
test_full        <- readRDS("data/test_all.rds")

# --- Drop unnecessary columns ---
drop_cols <- c("dataset")
train     <- train_data_clean %>% select(-all_of(drop_cols))
test_full <- test_data_clean  %>% select(-all_of(drop_cols))

# Subset of test set with only invalid coordinates
test <- test_data_clean %>%
  filter(latitude %in% c(0, -2e-08) | longitude %in% c(0, -2e-08)) %>%
  select(-all_of(drop_cols))

# --- Imputation using median/mode for missing values ---

# Function to compute the mode
get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Compute median/mode values from training set
yiu_median <- median(train$years_in_use, na.rm = TRUE)
sch_mode   <- get_mode(train$scheme_name)
pub_mode   <- get_mode(train$public_meeting)
perm_mode  <- get_mode(train$permit)
inst_mode  <- get_mode(train$installer)
fund_mode  <- get_mode(train$funder)

# Function for imputing missing values (excluding coordinates)
impute_fun <- function(df) {
  df %>% mutate(
    years_in_use    = replace_na(years_in_use, yiu_median),
    public_meeting  = factor(replace_na(public_meeting, pub_mode)),
    permit          = factor(replace_na(permit, perm_mode)),
    installer       = replace_na(installer, inst_mode),
    funder          = replace_na(funder, fund_mode),
    scheme_name     = factor(replace_na(as.character(scheme_name), sch_mode)),
    across(where(is.character), as.factor)
  )
}

# Apply imputation
train_imp      <- impute_fun(train)
test_imp       <- impute_fun(test)
test_full_imp  <- impute_fun(test_full)

# --- Align factor levels between train and test sets ---
align_factors <- function(train, test) {
  common_names <- intersect(names(train), names(test))
  
  for (col in common_names) {
    if (is.factor(train[[col]]) && is.factor(test[[col]])) {
      test[[col]] <- factor(test[[col]], levels = levels(train[[col]]))
    }
  }
  return(test)
}

test_full_imp <- align_factors(train_imp, test_full_imp)
sub_imputed   <- bind_rows(train_imp, test_imp)

# --- Separate handling of scheme_name to ensure factor alignment ---
impute_scheme_name <- function(df) {
  df$scheme_name[is.na(df$scheme_name)] <- as.character(sch_mode)
  df$scheme_name <- factor(df$scheme_name, levels = levels(train_imp$scheme_name))
  return(df)
}

train_imp     <- impute_scheme_name(train_imp)
test_imp      <- impute_scheme_name(test_imp)
test_full_imp <- impute_scheme_name(test_full_imp)

# --- Final combined datasets ---
sub_imputed  <- bind_rows(train_imp, test_imp)          # Only test subset with invalid coords
data_imputed <- bind_rows(train_imp, test_full_imp)     # Full imputed data

# --- Save results ---
saveRDS(test_imp,        "data/test_imp_invalid_coords.rds")  # Only test set with invalid coords
saveRDS(data_imputed,    "data/data_imputed.rds")             # All data
saveRDS(train_imp,       "data/train_imp.rds")                # Cleaned training set
saveRDS(sub_imputed,     "data/sub_imputed.rds")              # Combined train + subset test
saveRDS(test_full_imp,   "data/test_full_imp.rds")            # Cleaned full test set

