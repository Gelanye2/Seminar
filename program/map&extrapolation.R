###contributed by:Haoran Ju

data_all_clean <- readRDS("data/data_all_clean.rds")
train_all_clean <- readRDS("data/train_all_clean.rds")
test_all_clean <- readRDS("data/test_all_clean.rds")

########only those with useful coordinates
#get the map of tanzania
world <- ne_countries(scale = "medium", returnclass = "sf")
tanzania <- world %>% filter(admin == "United Republic of Tanzania")

data_all <- readRDS("data/data_all.rds")

data_clean <- data_all %>% filter(longitude != 0, latitude != 0)

####map preprocessing
#transfer into sf form
data_sf <- st_as_sf(data_clean, coords = c("longitude", "latitude"), crs = 4326)

ggplot() +
  geom_sf(data = tanzania, fill = "gray95", color = "black") +
  geom_sf(data = data_sf, aes(color = dataset), size = 0.6, alpha = 0.7) +
  scale_color_manual(values = c("train" = "steelblue", "test" = "tomato")) +
  facet_wrap(~dataset) +
  theme_minimal() +
  labs(title = "Train and Test Pump Locations", color = "Dataset")


#get train_clean
train_clean <- data_clean %>% filter(dataset == "train")

#analyze distribution of status_group
train_clean %>%
  count(status_group) %>%
  mutate(prop = n / sum(n))

###create ggplot for training set.
train_clean_sf <- st_as_sf(train_clean, coords = c("longitude", "latitude"), crs = 4326)
ggplot() +
  geom_sf(data = tanzania, fill = "gray95", color = "black") +
  geom_sf(data = train_clean_sf, aes(color = status_group), alpha = 0.7, size = 0.8) +
  theme_minimal() +
  labs(title = "Spatial Distribution of Pump Status",
       color = "Status Group")

ggplot() +
  geom_sf(data = tanzania, fill = "gray95", color = "black") +
  geom_sf(data = train_clean_sf,
          aes(color = as.factor(district_code)),
          alpha = 0.7, size = 0.8) +
  facet_wrap(~ region_code) +
  theme_minimal() +
  labs(
    title = "Spatial Distribution by Region Code (Colored by District Code)",
    color = "District Code"
  )

####bar plot for analyzing status via region
train_clean %>%
  group_by(region, status_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(region) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = reorder(region, -prop), y = prop, fill = status_group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Pump Status Proportions by Region",
       y = "Proportion", x = "Region")


###show train_test split on district level
train_test_split <- data_all_clean %>%
  count(region_district, dataset) %>%
  pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>%
  mutate(
    total = train + test,
    test_ratio = round(test / total, 3)
  ) %>%
  arrange(desc(test_ratio)) ###highest 0.275 reasonable split.

#####AUC test for data with non-missing coordinates
data_clean <- data_all_clean %>%
  filter(
    longitude != 0 & latitude != 0
  ) %>%
  mutate(is_test = ifelse(dataset == "test", 1, 0))

#simple glm model
model <- glm(is_test ~ latitude + longitude, data = data_clean, family = binomial())

#predict the probability
data_clean$pred_prob <- predict(model, type = "response")

#calculate AUC
auc <- roc(data_clean$is_test, data_clean$pred_prob)$auc #0.4978


