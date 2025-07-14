data_enhanced <- readRDS("data/data_all_spatial0.RDS") %>%
  select(-population_500, -location_cluster, -year_recorded, -dataset) %>%
  mutate(location_cluster2 = as.factor(location_cluster2)) %>%
  mutate(population_1k = ifelse(is.na(population_1k), -999, population_1k),
                                                                  location_cluster2 = ifelse(is.na(location_cluster2), -999, location_cluster2),
                                                                  neighbor_count_1km = ifelse(is.na(neighbor_count_1km), -999, neighbor_count_1km))

train_data_sp <- data_enhanced %>% filter(!is.na(status_group))
test_data_sp  <- data_enhanced %>% filter(is.na(status_group))

impute_values <- list(
  gps_height     = median(train_data_sp$gps_height, na.rm = TRUE),
  years_in_use   = median(train_data_sp$years_in_use, na.rm = TRUE),
  public_meeting    = get_mode(train_data_sp$public_meeting),
  permit            = get_mode(train_data_sp$permit),
  installer         = get_mode(train_data_sp$installer),
  funder            = get_mode(train_data_sp$funder),
  scheme_name       = get_mode(train_data_sp$scheme_name)
)

train_data_sp_imp <- imputation(train_data_sp, impute_values)
test_data_sp_imp  <- imputation(test_data_sp,  impute_values)

data_imputed_enhanced <- bind_rows(train_data_sp_imp, test_data_sp_imp)
saveRDS(data_imputed_enhanced, "data/data_imputed_enhanced.rds")
saveRDS(data_enhanced, "data/data_enhanced.rds")
