###contributed by: Haoran Ju

library(terra)
library(sf)
library(ggplot2)
library(dplyr)

# Create a 5x5 raster (UTM Zone 36S), extent 0–2500m
r <- rast(ncols = 5, nrows = 5, xmin = 0, xmax = 2500, ymin = 0, ymax = 2500, crs = "EPSG:32736")

# Assign population values (toy example)
values(r) <- matrix(c(
  10, 20, 30, 40, 50,
  15, 25, 35, 45, 55,
  20, 30, 40, 50, 60,
  25, 35, 45, 55, 65,
  30, 40, 50, 60, 70
), ncol = 5, byrow = TRUE)

# Point at the raster center
point_df <- data.frame(id = 1, x = 1250, y = 1250)
point_sf <- st_as_sf(point_df, coords = c("x", "y"), crs = 32736)

# 1 km buffer around the point
buffer <- st_buffer(point_sf, dist = 1000)
buffer_vect <- vect(buffer)

# Extract population sum within buffer
pop_extract <- extract(r, buffer_vect, fun = sum, na.rm = TRUE)
point_df$pop_1k <- pop_extract[,2]

# Convert raster for ggplot
r_df <- as.data.frame(r, xy = TRUE)

# Plot raster + buffer + point
pop <- ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = lyr.1)) +
  geom_sf(data = buffer, fill = NA, color = "red", lwd = 1) +
  geom_sf(data = point_sf, color = "black", size = 3) +
  scale_fill_viridis_c(name = "Population") +
  coord_sf(crs = st_crs(32736), datum = NA) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


ggsave("plot/pop_buffer.png", plot = pop, width = 4, height = 4, dpi = 300)



