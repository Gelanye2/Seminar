data_clean <- readRDS("data/fi_clean.rds")
train_data_clean <- data_clean %>% filter(dataset == "train")
test_data_clean <- data_clean %>% filter(dataset == "test")

drop_cols <- c("longitude","latitude","dataset")        

train  <- train_data_clean %>% select(-all_of(drop_cols))
test <- test_data_clean %>% 
  filter(
    latitude  ==  0      | latitude  == -2e-08 |
      longitude ==  0      | longitude == -2e-08
  )   %>% 
  select(-all_of(drop_cols)) 
test_full <- test_data_clean %>% select(-all_of(drop_cols))
##-------- imputation with median and mode --------------

# 2. Compute global mean / modes from the train set only
get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

gph_median         <- median(train$gps_height, na.rm = TRUE)
yiu_median         <- median(train$years_in_use, na.rm = TRUE)
construction_median <- median(train$construction_year, na.rm = TRUE)

sch_mode  <- get_mode(train$scheme_name)
pub_mode  <- get_mode(train$public_meeting)
perm_mode <- get_mode(train$permit)
inst_mode <- get_mode(train$installer)
fund_mode <- get_mode(train$funder)

# 3. Impute both data frames with those same values
impute_fun <- function(df) {
  df %>% mutate(
    gps_height        = replace_na(gps_height,        gph_median),
    years_in_use      = replace_na(years_in_use,      yiu_median),
    construction_year = replace_na(construction_year, construction_median),
    public_meeting    = factor(replace_na(public_meeting, pub_mode)),
    permit            = factor(replace_na(permit, perm_mode)),
    installer         = replace_na(installer, inst_mode),
    funder            = replace_na(funder, fund_mode),
    scheme_name = factor(replace_na(as.character(scheme_name), sch_mode)),
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

# 7. Impute scheme_name danach separat (nach Level-Angleichung)
impute_scheme_name <- function(df) {
  df$scheme_name <- as.character(df$scheme_name)
  df$scheme_name[is.na(df$scheme_name)] <- as.character(sch_mode)
  df$scheme_name <- factor(df$scheme_name, levels = levels(train_imp$scheme_name))
  return(df)
}

train_imp       <- impute_scheme_name(train_imp)
test_imp        <- impute_scheme_name(test_imp)
test_full_imp   <- impute_scheme_name(test_full_imp)

# 8. Kombinierte Daten erzeugen
sub_imputed   <- bind_rows(train_imp, test_imp)        # für Teildatenmodell
data_imputed  <- bind_rows(train_imp, test_full_imp)   # vollständiger Testsatz

saveRDS(data_imputed, "data/data_imputed.rds")
saveRDS(train_imp, "data/train_imp.rds")
saveRDS(sub_imputed, "data/sub_imputed.rds")
saveRDS(test_full_imp, "data/test_full_imp.rds")
