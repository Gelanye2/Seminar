data_clean <- readRDS("data/fi_clean.rds")
train_data_clean <- data_clean %>% filter(dataset == "train")
test_data_clean <- data_clean %>% filter(dataset == "test")


drop_cols <- c("longitude", "latitude", "lga", "ward", "subvillage",
               "district_code", "dataset", "region_code")         

train  <- train_data_clean %>% select(-all_of(drop_cols))
test <- test_data_clean %>% 
  filter(
    latitude  ==  0      | latitude  == -2e-08 |
      longitude ==  0      | longitude == -2e-08
  )   %>% 
  select(-all_of(drop_cols)) 
test_full <- test_data_clean %>% select(-all_of(drop_cols))
##-------- imputation with median and mode --------------

# 2. Compute global medians / modes from the train set only
gph_med   <- median(train$gps_height,   na.rm = TRUE)
yiu_med   <- median(train$years_in_use, na.rm = TRUE)
get_mode  <- function(x) { ux <- unique(x); ux[which.max(tabulate(match(x, ux)))] }
sch_mode  <- get_mode(train$scheme_management)
pub_mode  <- get_mode(train$public_meeting)
perm_mode <- get_mode(train$permit)
inst_mode <- get_mode(train$installer)
fund_mode <- get_mode(train$funder)

# 3. Impute both data frames with those same values
impute_fun <- function(df) {
  df %>% mutate(
    gps_height        = replace_na(gps_height,        gph_med),
    years_in_use      = replace_na(years_in_use,      yiu_med),
    scheme_management = replace_na(scheme_management, sch_mode),
    public_meeting    = factor(replace_na(public_meeting, pub_mode)),
    permit            = factor(replace_na(permit, perm_mode)),
    installer         = replace_na(installer, inst_mode),
    funder            = replace_na(funder, fund_mode),
    across(where(is.character), as.factor)
  )
}

train_imp <- impute_fun(train)
test_imp  <- impute_fun(test)
test_full_imp <- impute_fun(test_full)

#Faktor-Levels in beiden Datensätzen angleichen

# alle Spalten im test-set so anpassen, dass die Levels wie im train sind
align_factors <- function(train, test) {
  common_names <- intersect(names(train), names(test))
  
  for (col in common_names) {
    if (is.factor(train[[col]]) && is.factor(test[[col]])) {
      # setze Levels im Test-Datensatz gleich wie im Train-Datensatz
      test[[col]] <- factor(test[[col]], levels = levels(train[[col]]))
    }
  }
  return(test)
}

# Anwendung
test_full_imp <- align_factors(train_imp, test_full_imp)
sub_imputed <- bind_rows(train_imp, test_imp) # for submodel
data_imputed <- bind_rows(train_imp, test_full_imp)

# ------------ imputation with mice ---------
data_all <- bind_rows(
  train_imp %>% mutate(source == "train"),   # still has status_group
  test_imp  %>% mutate(source == "test")     # status_group is NA
)

##########
library(mice)

# 2a. A dry-run to grab default settings
ini  <- mice(data_all, maxit = 0)

# 2b. Copy the defaults
meth <- ini$method
pred <- ini$predictorMatrix

# 2c. Tell mice not to impute the target
meth["status_group"] <- ""     # ""  = leave as is (don’t touch)
pred[,  "status_group"] <- 0    # don’t use it to predict others either

#########
imp <- mice(
  data_all,
  m      = 5,          # 5 completed data sets (common default)
  maxit  = 5,          # iterations per chain; raise if convergence is slow
  method = meth,
  predictorMatrix = pred,
  seed   = 2024
)

mice_imputed <- complete(imp, action = 1)   # pick the first of the m imputations

train_clean <- mice_imputed %>% filter(source == "train" & !is.na(status_group))
test_clean  <- mice_imputed %>% filter(source == "test")
mice_imputed <- mice_imputed %>% 
  select(-`source == "train"`, -`source == "test"`)

saveRDS(data_imputed, "data/data_imputed.rds")
saveRDS(sub_imputed, "data/sub_imputed.rds")
saveRDS(test_full_imp, "data/test_full_imp.rds")
saveRDS(mice_imputed, "data/mice_imputed.rds")


#####
## check if test_full_imp identical to test_data_clean (order)
identical(
  as.data.frame(test_all) %>%
    select(extraction_type, management, region, basin,population) %>%
    mutate(across(everything(), as.character)),
  
  as.data.frame(test_full_imp) %>%
    select(extraction_type, management, region, basin,population) %>%
    mutate(across(everything(), as.character))
)

library(waldo)

old <- test_all %>% 
  select(extraction_type, management, region, basin,population,public_meeting) %>%
  mutate(across(everything(), as.character))

new <- test_full_imp %>%
  select(extraction_type, management, region, basin,population,public_meeting) %>%
  mutate(across(everything(), as.character))

# Compare the two cleaned-up data frames
compare(old, new, max_diffs = Inf)



