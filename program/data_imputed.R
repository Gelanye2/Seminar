data_clean <- readRDS("data/fi_clean.rds")
train_data_clean <- data_clean %>% filter(dataset == "train")
test_data_clean <- data_clean %>% filter(dataset == "test")

#select cols for training, use region_code
train_data_clean  <- train_data_clean %>%
  select(-longitude, -latitude, -lga, -ward, -subvillage, -district_code, -region,-dataset, -region_code)

#write a function for getting mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#here we should edit: get mode for scheme_management, public_meeting, permit,installer and funder,
#and mutate,also factorize
train_data_imputed <- train_data_clean %>%
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

# Select rows with missing latitude or longitude (test data)
test_data_clean <- test_data_clean %>% 
  filter(
    latitude  ==  0      | latitude  == -2e-08 |
      longitude ==  0      | longitude == -2e-08
  )  %>% 
  select(-longitude, -latitude, -lga, -ward, -subvillage,-district_code, -region,-dataset, -region_code)

bool_cols <- c(
  names(train_data_imputed)[vapply(train_data_imputed, is.logical, logical(1))],
  names(test_data_clean   )[vapply(test_data_clean,    is.logical, logical(1))]
)

train_data_imputed <- train_data_imputed %>%
  mutate(across(all_of(bool_cols), as.logical))

test_data_clean  <- test_data_clean %>%
  mutate(across(any_of(bool_cols), as.logical)) 

data_imputed <- bind_rows(train_data_imputed, test_data_clean)

saveRDS(data_imputed, "data/data_imputed.rds")
