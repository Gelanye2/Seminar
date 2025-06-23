train_all_clean <- readRDS("data/fi_clean.rds")

#select cols for training, use region_code
train_model_copy  <- train_all_clean %>%
  select(-longitude, -latitude, -lga, -ward, -subvillage, -district_code, -region,-dataset, -region_code)

#write a function for getting mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#here we should edit: get mode for scheme_management, public_meeting, permit,installer and funder,
#and mutate,also factorize
train_model_copy <- train_model_copy %>%
  mutate(
    population = replace_na(population, median(population, na.rm = TRUE)),
    years_in_use = replace_na(years_in_use, median(years_in_use, na.rm = TRUE)),
    gps_height = replace_na(gps_height, median(gps_height, na.rm = TRUE)),
    scheme_management = replace_na(scheme_management, get_mode(scheme_management)),
    public_meeting = as.factor(public_meeting),
    public_meeting = replace_na(public_meeting, get_mode(public_meeting)),
    permit = as.factor(permit),
    permit = replace_na(permit, get_mode(permit)),
    installer = replace_na(installer, get_mode(installer)),
    funder = replace_na(funder, get_mode(funder)),
    gps_height = replace_na(gps_height, median(gps_height, na.rm = TRUE)),
    across(where(is.character), as.factor))

#save the imputed dataset
factor_cols <- names(train_model_copy)[vapply(train_model_copy, is.factor, logical(1))]
test_all_clean <- test_all_clean %>% 
  mutate(across(all_of(factor_cols), as.factor)) %>%
  select(-longitude, -latitude, -lga, -ward, -subvillage,-district_code, -region,-dataset, -region_code)

data_imputed <- bind_rows(train_model_copy, test_all_clean)

saveRDS(data_imputed, "data/data_imputed.rds")

