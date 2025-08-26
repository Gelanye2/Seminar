# Contributed by: Gelan Ye

# Load setup and cleaned training data
source("setup.R")
train_all_clean <- readRDS("data/train_all_clean.rds")

# Define a custom theme for all maps
custom_map_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key.size = unit(0.7, "cm")
  )

# Keep only entries with valid geographic coordinates
map_data <- train_all_clean %>%
  filter(longitude != 0 & latitude != 0)

# Visualize missingness in the 'installer' variable
map_data_installer <- map_data %>%
  mutate(installer_status = ifelse(is.na(installer) | installer == "", "Missing", "Non-Missing"))

ggplot(map_data_installer, aes(x = longitude, y = latitude, color = installer_status)) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("Missing" = "darkred", "Non-Missing" = "steelblue")) +
  labs(
    title = "Installer Field Missingness",
    x = "Longitude",
    y = "Latitude",
    color = "Pump Installer"
  ) +
  custom_map_theme

# Visualize missingness in the 'funder' variable
map_data_funder <- map_data %>%
  mutate(funder_status = ifelse(is.na(funder) | funder == "", "Missing", "Non-Missing"))

ggplot(map_data_funder, aes(x = longitude, y = latitude, color = funder_status)) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("Missing" = "darkred", "Non-Missing" = "steelblue")) +
  labs(
    title = "Funder Field Missingness",
    x = "Longitude",
    y = "Latitude",
    color = "Status"
  ) +
  custom_map_theme

# Visualize missingness in the 'scheme_management' variable
map_data_scheme <- map_data %>%
  mutate(scheme_status = ifelse(is.na(scheme_management) | scheme_management == "", "Missing", "Non-Missing"))

ggplot(map_data_scheme, aes(x = longitude, y = latitude, color = scheme_status)) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("Missing" = "darkred", "Non-Missing" = "steelblue")) +
  labs(
    title = "Scheme Management Missingness",
    x = "Longitude",
    y = "Latitude",
    color = "Status"
  ) +
  custom_map_theme

# Visualize availability of 'public_meeting'
map_data_pm <- map_data %>%
  mutate(public_meeting = ifelse(is.na(public_meeting), "Missing",
                                 ifelse(public_meeting == TRUE, "Yes", "No")))

ggplot(map_data_pm, aes(x = longitude, y = latitude, color = public_meeting)) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("Yes" = "steelblue", "No" = "darkred", "Missing" = "grey70")) +
  labs(
    title = "Public Meeting Availability",
    x = "Longitude",
    y = "Latitude",
    color = "Public Meeting"
  ) +
  custom_map_theme

# Visualize availability of 'permit'
map_data_permit <- map_data %>%
  mutate(permit = ifelse(is.na(permit), "Missing",
                         ifelse(permit == TRUE, "Yes", "No")))

ggplot(map_data_permit, aes(x = longitude, y = latitude, color = permit)) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("Yes" = "steelblue", "No" = "darkred", "Missing" = "grey70")) +
  labs(
    title = "Permit Availability",
    x = "Longitude",
    y = "Latitude",
    color = "Permit"
  ) +
  custom_map_theme

