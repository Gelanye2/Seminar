# ####contributed by: Haoran Ju
# ###Do Moran's test to test autocorrelation of residuals
# train_all_clean <- readRDS("data/fi_clean.rds") %>%
#   filter(dataset == "train")
# train_model <- train_all_clean %>%
#   select(-lga, -ward, -subvillage,-district_code, -region,-dataset, -month_recorded
#          ,-region_district,-region_code) %>%
#   mutate(population = ifelse(population == 0, NA, population))
#
# train <- train_model %>%
#   filter(longitude != 0, latitude != 0)
#
# train$status_numeric <- as.numeric(factor(train$status_group))
#
# model <- ranger(status_numeric ~ ., data = train)
# train$residual <- model$predictions - train$status_numeric
#
# coords <- cbind(train$longitude, train$latitude)
# nb <- knn2nb(knearneigh(coords, k = 8))
# lw <- nb2listw(nb, style = "W")
# moran_test <- moran.test(train$residual, listw = lw) ####no need for spatial modeling
# print(moran_test) #no need for spatial modeling
# # Moran's I for XGBoost residuals
# # Moran's I for LM residuals
#population_buffer
data_all_clean <- readRDS("data/fi_clean_year.rds")
data_all_clean <- data_all_clean %>%
  mutate(population_1k = NA_integer_,
         population_500 = NA_integer_)
valid_years <- c(2001, 2002, 2004, 2011, 2012, 2013)

pop_rasters <- map(setNames(valid_years, valid_years), function(y) {
  rast(paste0("data/tza_ppp_", y, ".tif"))
})

for (yr in valid_years) {
  data_year <- data_all_clean %>%
    filter(year_recorded == yr & !is.na(longitude) & !is.na(latitude)) %>%
    mutate(row_id = row_number())

  points_sf <- st_as_sf(data_year, coords = c("longitude", "latitude"), crs = 4326)
  points_sf <- st_transform(points_sf, crs = crs(pop_rasters[[as.character(yr)]]))

  buffer_1km <- st_buffer(points_sf, dist = 1000)
  buffer_500 <- st_buffer(points_sf, dist = 500)

  pop_1k <- extract(pop_rasters[[as.character(yr)]], vect(buffer_1km), fun = sum, na.rm = TRUE)
  pop_500 <- extract(pop_rasters[[as.character(yr)]], vect(buffer_500), fun = sum, na.rm = TRUE)
  target_rows <- which(data_all_clean$year_recorded == yr & !is.na(data_all_clean$longitude))
  data_all_clean$population_1k[target_rows] <- as.integer(pop_1k[,2])
  data_all_clean$population_500[target_rows] <- as.integer(pop_500[,2])
}

plot(data_all_clean$population, data_all_clean$population_500,
     xlab = "Original Population",
     ylab = "Buffer 500m Population",
     main = "Original vs Buffer-based Population",
     pch = 16, col = rgb(0,0,1,0.3))
abline(0, 1, col = "red", lwd = 2)

saveRDS(data_all_clean, "data/data_all_buffer.rds")

###location_cluster
set.seed(123)
data_all_buffer <- readRDS("data/data_all_buffer.rds")
data_geo <- data_all_buffer %>%
  filter(longitude != 0, latitude != 0) %>%
  select(longitude, latitude)
k <- length(unique(data_all_buffer$region_district))
km_res <- kmeans(data_geo, centers = k)
data_all_buffer$location_cluster <- NA_integer_
data_all_buffer$location_cluster[which(data_all_buffer$longitude != 0 & data_all_buffer$latitude != 0)] <- km_res$cluster

#location cluster with population
data_geo <- data_all_buffer %>%
  filter(longitude != 0, latitude != 0) %>%
  select(longitude, latitude, population_1k)
data_scaled <- scale(data_geo)
km_res <- kmeans(data_scaled, centers = k,iter.max = 100)
data_all_buffer$location_cluster2 <- NA_integer_
data_all_buffer$location_cluster2[which(data_all_buffer$longitude != 0 & data_all_buffer$latitude != 0)] <- km_res$cluster

###count neighborhoods for each pump
#transfer into sf object
points_sf <- data_all_buffer %>%
  filter(longitude != 0, latitude != 0) %>%
  mutate(row_id = row_number()) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32736)

# create 1km buffer
buffers_geom <- st_buffer(points_sf, dist = 1000)
buffers <- st_sf(
  row_id_center = points_sf$row_id,
  geometry = st_geometry(buffers_geom)
)

# spatial join via center and find neighborhoods
neighbors_all <- st_join(buffers, points_sf, join = st_intersects)
neighbors_filtered <- neighbors_all %>%
  filter(row_id_center != row_id)
#any(neighbors_filtered$row_id== neighbors_filtered$row_id_center)
neighbor_counts <- neighbors_filtered %>%
  group_by(row_id_center) %>%
  summarise(neighbor_count_1km = n(), .groups = "drop")

#add pumps with 0 neighborhoods
idx <- which(data_all_buffer$longitude != 0 & data_all_buffer$latitude != 0)
full_ids <- tibble(row_id_center = 1:length(idx))
neighbor_counts_full <- full_ids %>%
  left_join(neighbor_counts, by = "row_id_center") %>%
  mutate(neighbor_count_1km = replace_na(neighbor_count_1km, 0))

#combine back to the origin dataset
data_all_buffer$neighbor_count_1km <- NA_integer_
data_all_buffer$neighbor_count_1km[idx] <- neighbor_counts_full$neighbor_count_1km

#save rds
saveRDS(data_all_buffer, file = "data/data_all_spatial0.rds")
#a dataset with additional information: population around coords, coords cluster, neighbor counts
data_all_spatial <- readRDS("data/data_all_spatial0.rds")
data_all_spatial<- data_all_spatial %>%
  filter(longitude != 0, latitude != 0)
#save the final dataset
saveRDS(data_all_spatial, "data/data_all_spatial.rds")

###imputation
data_all_spatial <- readRDS("data/data_all_spatial.rds") %>%
  select(-dataset, -year_recorded, -population_500)

#mean mode is slightly better than median mode
train_data_sp <- data_all_spatial %>% filter(!is.na(status_group))
test_data_sp  <- data_all_spatial %>% filter(is.na(status_group))

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

data_all_spatial_imputed <- bind_rows(train_data_sp_imp, test_data_sp_imp)
saveRDS(data_all_spatial_imputed, "data/data_all_spatial_imputed.rds")
x<- readRDS("data/data_all_spatial_imputed.rds")
