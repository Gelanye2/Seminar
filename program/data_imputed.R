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

data_imputed <- bind_rows(train_imp, test_imp)

