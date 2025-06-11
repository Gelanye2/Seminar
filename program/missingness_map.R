
custom_map_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key.size = unit(0.7, "cm")
  )

#Clean Coordinates
map_data <- train_all_clean %>%
  filter(longitude != 0 & latitude != 0)

#gps_height
map_data_gps <- map_data %>%
  mutate(missing_gps = is.na(gps_height))

ggplot() +
  geom_point(data = map_data_gps %>% filter(missing_gps), 
             aes(x = longitude, y = latitude), 
             color = "grey80", size = 0.8) +
  geom_point(data = map_data_gps %>% filter(!missing_gps), 
             aes(x = longitude, y = latitude, color = gps_height), size = 0.8) +
  scale_color_gradient(low = "#00008b", high = "#add8e6", name = "GPS Height") +
  labs(title = "GPS Height Distribution", x = "Longitude", y = "Latitude") +
  custom_map_theme

#population
map_data_pop <- map_data %>%
  mutate(missing_pop = is.na(population) | population == 0)

ggplot() +
  geom_point(data = map_data_pop %>% filter(missing_pop), 
             aes(x = longitude, y = latitude), 
             color = "grey80", size = 0.8) +
  geom_point(data = map_data_pop %>% filter(!missing_pop), 
             aes(x = longitude, y = latitude, color = population), size = 0.8) +
  scale_color_gradient(low = "#00008b", high = "#add8e6", name = "Population") +
  labs(title = "Population by Location", x = "Longitude", y = "Latitude") +
  custom_map_theme

#installer
map_data_installer <- map_data %>%
  mutate(installer_status = ifelse(is.na(installer) | installer == "", "Missing", "Non-Missing"))

ggplot(map_data_installer, aes(x = longitude, y = latitude, color = installer_status)) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("Missing" = "darkred", "Non-Missing" = "steelblue")) +
  labs(title = "Installer Missingness", x = "Longitude", y = "Latitude", color = "Status") +
  custom_map_theme

#funder
map_data_funder <- map_data %>%
  mutate(funder_status = ifelse(is.na(funder) | funder == "", "Missing", "Non-Missing"))

ggplot(map_data_funder, aes(x = longitude, y = latitude, color = funder_status)) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("Missing" = "darkred", "Non-Missing" = "steelblue")) +
  labs(title = "Funder Missingness", x = "Longitude", y = "Latitude", color = "Status") +
  custom_map_theme

#scheme_management
map_data_scheme <- map_data %>%
mutate(scheme_status = ifelse(is.na(scheme_management) | scheme_management == "", "Missing", "Non-Missing"))

ggplot(map_data_scheme, aes(x = longitude, y = latitude, color = scheme_status)) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("Missing" = "darkred", "Non-Missing" = "steelblue")) +
  labs(title = "Scheme Management Missingness", x = "Longitude", y = "Latitude", color = "Status") +
  custom_map_theme

#public_meeting
map_data_pm <- map_data %>%
mutate(public_meeting = ifelse(is.na(public_meeting), "Missing",
                               ifelse(public_meeting == TRUE, "Yes", "No")))

ggplot(map_data_pm, aes(x = longitude, y = latitude, color = public_meeting)) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("Yes" = "steelblue", "No" = "darkred", "Missing" = "grey70")) +
  labs(title = "Public Meeting Availability", x = "Longitude", y = "Latitude", color = "Public Meeting") +
  custom_map_theme

#permit
map_data_permit <- map_data %>%
  mutate(permit = ifelse(is.na(permit), "Missing",
                         ifelse(permit == TRUE, "Yes", "No")))

ggplot(map_data_permit, aes(x = longitude, y = latitude, color = permit)) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("Yes" = "steelblue", "No" = "darkred", "Missing" = "grey70")) +
  labs(title = "Permit Availability", x = "Longitude", y = "Latitude", color = "Permit") +
  custom_map_theme
