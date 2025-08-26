#Contributed by: Gelan Ye

# --- Setup ---
library(dplyr)
library(ggplot2)

# --- Load data (keep only rows with non-missing status_group) ---
train_all <- readRDS("data/data_all.rds") %>%
  filter(!is.na(status_group))

# --- Check redundancy between categorical columns ---

# water_quality vs. quality_group
train_all %>% 
  select(water_quality, quality_group) %>% 
  distinct()

# payment vs. payment_type
train_all %>% 
  select(payment, payment_type) %>% 
  distinct()

# quantity vs. quantity_group
train_all %>% 
  select(quantity, quantity_group) %>% 
  distinct()

# extraction_type, extraction_type_group, extraction_type_class
train_all %>% 
  select(extraction_type, extraction_type_group, extraction_type_class) %>% 
  distinct()

# management vs. management_group
train_all %>% 
  select(management, management_group) %>% 
  distinct()

# source, source_type, source_class
train_all %>% 
  select(source, source_type, source_class) %>% 
  distinct()

# waterpoint_type vs. waterpoint_type_group
train_all %>% 
  select(waterpoint_type, waterpoint_type_group) %>% 
  distinct()

# Frequency counts for extraction categories

train_all %>% count(extraction_type, sort = TRUE)
train_all %>% count(extraction_type_group, sort = TRUE)
train_all %>% count(extraction_type_class, sort = TRUE)

data_clean <- readRDS("data/fi_clean.rds")

# Visualize missingness and distribution of years_in_use
  ggplot(data_clean, aes(x = years_in_use)) +
    geom_histogram(aes(y = ..density..), bins = 30,
                   fill = "steelblue", color = "white", alpha = 0.6) +
    geom_density(color = "red", size = 1) +
    labs(x = "Pump age (years)", y = "Proportion") +
    scale_x_continuous(breaks = seq(0, 50, by = 5), limits = c(0, 50)) +
    scale_y_continuous(breaks = seq(0, 0.07, by = 0.01)) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank()
    )

