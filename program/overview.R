# Contributed by: Gelan Ye

# Setup
source("setup.R")

library(leaflet)   
library(dplyr)    
library(sf)       
library(ggplot2)   

# Load data
data_all  <- readRDS("data/data_all.rds")
train_all <- readRDS("data/train_all.rds")

# Pump distribution
# Basic pump distribution
leaflet(data_all) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 2,
    stroke = FALSE,
    fillOpacity = 0.6
  )

# Pump distribution with styled basemap
leaflet(data_all) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 2,            
    stroke = FALSE,
    fillOpacity = 0.6,
    color = "blue"
  )

# Distribution of target variable
# Keep consistent color mapping across plots
status_colors <- c(
  "functional"              = "#377eb8",            
  "functional needs repair" = "#ff7f00",
  "non functional"          = "#984ea3"          
)

train_all %>%
  count(status_group) %>%
  mutate(
    percent      = n / sum(n) * 100,
    status_group = factor(
      status_group, 
      levels = c("functional", "functional needs repair", "non functional")
    )
  ) %>%
  ggplot(aes(x = status_group, y = n, fill = status_group)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    vjust = -0.4, size = 4.5
  ) +  
  scale_fill_manual(values = status_colors) +
  labs(
    x    = NULL,
    y    = "Count",
    fill = "Status"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title   = element_text(hjust = 0.5),
    axis.text.x  = element_text(angle = 20, vjust = 0.8, hjust = 0.8),
    legend.position = "right"
  )


