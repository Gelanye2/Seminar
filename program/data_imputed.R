
#####1. with imputed Latitude/Longitude

data_clean <- readRDS("data/fi_clean.rds")
train_data_clean <- data_clean %>% filter(dataset == "train")
test_data_clean <- data_clean %>% filter(dataset == "test")
test_full <- readRDS("data/test_all.rds")
drop_cols <- c("dataset")

# 1. Latitude/Longitude korrigieren (NA setzen bei 0/-2e-08)
train_data_clean <- train_data_clean %>%
  mutate(
    latitude  = ifelse(latitude %in% c(0, -2e-08), NA, latitude),
    longitude = ifelse(longitude %in% c(0, -2e-08), NA, longitude)
  )

test_data_clean <- test_data_clean %>%
  mutate(
    latitude  = ifelse(latitude %in% c(0, -2e-08), NA, latitude),
    longitude = ifelse(longitude %in% c(0, -2e-08), NA, longitude)
  )


# 2. Jetzt Spalten entfernen
drop_cols <- c("dataset")  # latitude und longitude bleiben jetzt erhalten!
train      <- train_data_clean %>% select(-all_of(drop_cols))
test_full  <- test_data_clean %>% select(-all_of(drop_cols))

# test mit nur invaliden Koordinaten extrahieren
test <- test_data_clean %>% 
  filter(
    is.na(latitude) | is.na(longitude)
  ) %>% 
  select(-all_of(drop_cols))

##-------- imputation with median and mode --------------

# 2. Compute global mean / modes from the train set only
get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

gph_median         <- median(train$gps_height, na.rm = TRUE)
yiu_median         <- median(train$years_in_use, na.rm = TRUE)

latitude_median <- median(train$latitude, na.rm = TRUE)
longitude_median <- median(train$longitude, na.rm = TRUE)

sch_mode  <- get_mode(train$scheme_management)
pub_mode  <- get_mode(train$public_meeting)
perm_mode <- get_mode(train$permit)
inst_mode <- get_mode(train$installer)
fund_mode <- get_mode(train$funder)

# 3. Impute both data frames with those same values
impute_fun <- function(df) {
  df %>% mutate(
    gps_height        = replace_na(gps_height,        gph_median),
    years_in_use      = replace_na(years_in_use,      yiu_median),

    latitude          = replace_na(latitude,          latitude_median),
    longitude         = replace_na(longitude,         longitude_median),
    public_meeting    = factor(replace_na(public_meeting, pub_mode)),
    permit            = factor(replace_na(permit, perm_mode)),
    installer         = replace_na(installer, inst_mode),
    funder            = replace_na(funder, fund_mode),
    scheme_management = factor(replace_na(as.character(scheme_management), sch_mode)),
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

# 8. Kombinierte Daten erzeugen
sub_imputed   <- bind_rows(train_imp, test_imp)        # für Teildatenmodell
data_imputed  <- bind_rows(train_imp, test_full_imp)   # vollständiger Testsatz

saveRDS(data_imputed, "data/data_imputed_with_la.rds")
saveRDS(train_imp, "data/train_imp_with_la.rds")
saveRDS(sub_imputed, "data/sub_imputed_with_la.rds")
saveRDS(test_full_imp, "data/test_full_imp_with_la.rds")

########

data_clean <- readRDS("data/fi_clean.rds")
train_data_clean <- data_clean %>% filter(dataset == "train")
test_data_clean <- data_clean %>% filter(dataset == "test")

drop_cols <- c("dataset")

------------- # 1. Latitude/Longitude NICHT korrigieren (0/-2e-08 behalten) ---------
# 2. Spalten entfernen
drop_cols <- c("dataset")  # latitude und longitude bleiben erhalten
train      <- train_data_clean %>% select(-all_of(drop_cols))
test_full  <- test_data_clean %>% select(-all_of(drop_cols))

# test mit nur invaliden Koordinaten extrahieren
test <- test_data_clean %>% 
  filter(latitude %in% c(0, -2e-08) | longitude %in% c(0, -2e-08)) %>% 
  select(-all_of(drop_cols))

##-------- imputation with median and mode (OHNE LAT/LONG!) --------------
data_clean <- readRDS("data/fi_clean.rds")
train_data_clean <- data_clean %>% filter(dataset == "train")
test_data_clean <- data_clean %>% filter(dataset == "test")

drop_cols <- c("dataset")
# Compute global mean / modes from the train set only

train      <- train_data_clean %>% select(-all_of(drop_cols))
test_full  <- test_data_clean %>% select(-all_of(drop_cols))
test <- test_data_clean %>% 
  filter(
    is.na(latitude) | is.na(longitude)
  ) %>% 
  select(-all_of(drop_cols))

get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

gph_median          <- median(train$gps_height, na.rm = TRUE)
yiu_median          <- median(train$years_in_use, na.rm = TRUE)


sch_mode  <- get_mode(train$scheme_name)
pub_mode  <- get_mode(train$public_meeting)
perm_mode <- get_mode(train$permit)
inst_mode <- get_mode(train$installer)
fund_mode <- get_mode(train$funder)

# Impute both data frames (OHNE latitude und longitude)
impute_fun <- function(df) {
  df %>% mutate(
    gps_height        = replace_na(gps_height,        gph_median),
    years_in_use      = replace_na(years_in_use,      yiu_median),
    
    public_meeting    = factor(replace_na(public_meeting, pub_mode)),
    permit            = factor(replace_na(permit, perm_mode)),
    installer         = replace_na(installer, inst_mode),
    funder            = replace_na(funder, fund_mode),
    scheme_name       = factor(replace_na(as.character(scheme_name), sch_mode)),
    across(where(is.character), as.factor)
  )
}

train_imp <- impute_fun(train)
test_imp  <- impute_fun(test)
test_full_imp <- impute_fun(test_full)

# Faktor-Levels angleichen
align_factors <- function(train, test) {
  common_names <- intersect(names(train), names(test))
  
  for (col in common_names) {
    if (is.factor(train[[col]]) && is.factor(test[[col]])) {
      test[[col]] <- factor(test[[col]], levels = levels(train[[col]]))
    }
  }
  return(test)
}

# Anwendung
test_full_imp <- align_factors(train_imp, test_full_imp)
sub_imputed <- bind_rows(train_imp, test_imp) # für Teilmodell

# scheme_name separat (nach Level-Angleichung)
impute_scheme_name <- function(df) {
  df$scheme_name <- as.character(df$scheme_name)
  df$scheme_name[is.na(df$scheme_name)] <- as.character(sch_mode)
  df$scheme_name <- factor(df$scheme_name, levels = levels(train_imp$scheme_name))
  return(df)
}

train_imp       <- impute_scheme_name(train_imp)
test_imp        <- impute_scheme_name(test_imp)
test_full_imp   <- impute_scheme_name(test_full_imp)

# Kombinierte Daten erzeugen
sub_imputed   <- bind_rows(train_imp, test_imp)
data_imputed  <- bind_rows(train_imp, test_full_imp)

saveRDS(data_imputed, "data/data_imputed.rds")
saveRDS(train_imp, "data/train_imp.rds")
saveRDS(sub_imputed, "data/sub_imputed.rds")
saveRDS(test_full_imp, "data/test_full_imp.rds")

