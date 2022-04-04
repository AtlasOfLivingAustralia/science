# script to plot turtles project data

library(galah)
library(ggplot2)
library(ozmaps)
library(terra)
library(viridis)

# search_fields("uid") # check which uid to use

# set up filters
collection_filter <- select_filters(dataResourceUid = "dr17780")
# ala_counts(filters = collection_filter) # check = OK

galah_config(email = "martinjwestgate@gmail.com")
df <- ala_occurrences(filters = collection_filter)
# saveRDS(df, "./data/turtles.rds")
# df <- readRDS("./data/turtles.rds")
# str(df)
# xtabs(~scientificName, data = df)

df$scientificName[df$scientificName == "CHELIDAE"] <- "Unknown"

bbox_df <- data.frame(
  x = c(min(df$decimalLongitude), max(df$decimalLongitude))[c(1, 2, 2, 1, 1)],
  y = c(min(df$decimalLatitude), max(df$decimalLatitude))[c(1, 1, 2, 2, 1)])
  
# get global elevation data
elev_world <- terra::rast("./downloads/wc2.1_30s_elev.tif")
bbox_terra <- ext(125, 140, -20, -10)
elev_cropped <- crop(elev_world, bbox_terra)

# resample for plotting
elev_resampled <- spatSample(
  elev_cropped, 
  method = "regular", 
  size = 10^5,
  as.raster = TRUE)
elev_df <- as.data.frame(elev_resampled, xy = TRUE)
colnames(elev_df)[3] <- "elevation"

# high-res for a smaller area
bbox_small <- ext(134, 136, -15.5, -13)
elev_small <- crop(elev_cropped, bbox_small)
test_flow <- terrain(elev_small, v = "slope")
elev_resampled <- spatSample(
  test_flow, 
  method = "regular", 
  size = 10^6,
  as.raster = TRUE)
elev_df_small <- as.data.frame(elev_resampled, xy = TRUE)
colnames(elev_df_small)[3] <- "slope"

#boundary detection 
# test_boundaries <- boundaries(test_flow, classes = FALSE)
# boundary_df <- as.data.frame(test_boundaries, xy = TRUE)
# colnames(boundary_df)[3] <- "is_boundary"
# boundary_df <- boundary_df[boundary_df$is_boundary == 1, ]
# boundary_df <- boundary_df[c(
#   boundary_df$x != min(boundary_df$x) &
#   boundary_df$y != min(boundary_df$y) &
#   boundary_df$y != max(boundary_df$y)), ]


# panel b: zoomed map

## attempt on a plain base map
# ggplot(ozmaps::ozmap_states) + 
#   geom_sf() +
#   geom_polygon(
#     data = bbox_df,
#     mapping = aes(x = x, y = y),
#     color = "red",
#     fill = NA
#   ) +
#   lims(x = c(125, 140), y = c(-20, -10)) +
#   theme_bw()
  
# zoomed map with raster underlay
panel_b <- ggplot() +
  geom_raster(
    data = elev_df,
    mapping = aes(x = x, y = y, fill = elevation)) +
  # geom_sf(
  #   data = ozmaps::ozmap_states,
  #   fill = NA,
  #   color = "grey30"
  # ) +
  geom_polygon(
    data = bbox_df,
    mapping = aes(x = x, y = y),
    color = "red",
    fill = NA) +
  scale_fill_gradient(low = "#1a6133", high = "#ababab") +
  # coord_sf() +
  lims(x = c(125, 140), y = c(-20, -10)) +
  coord_fixed(expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "#b8d3e3", color = "#1a6133"),
    legend.position = "none")
# not used

    
# panel a: australia map
panel_a <- ggplot(ozmaps::ozmap_states) + 
  geom_sf(
    color = NA, #"#ffffff",
    fill = "#FFC557",
    size = 0.1
  ) +
  geom_polygon(
    data = bbox_df,
    mapping = aes(x = x, y = y),
    fill = "#E06E53") +
  lims(x = c(110, 155), y = c(-45, -5)) +
  theme_void()

  
# panel c: location map
panel_c <- ggplot() +
  geom_raster(
    data = elev_df_small, #[elev_df_small$slope > 0.5, ],
    mapping = aes(x = x, y = y, fill = slope)) +
  # geom_point(
  #   data = boundary_df,
  #   aes(x = x, y = y),
  #   size = 1,
  #   color = "#79b1d1") +
  geom_point(
    data = df,
    mapping = aes(
      x = decimalLongitude, 
      y = decimalLatitude),
     #  color = scientificName)
    shape = 16,
    color = "#E06E53",
    # fill = "black",
    alpha = 0.5,
    size = 3) + 
  geom_polygon(
    data = data.frame(
      x = c(134.15, 136)[c(1, 2, 2, 1, 1)],
      y = c(-15.2, -13.2)[c(1, 1, 2, 2, 1)]),
    mapping = aes(x = x, y = y),
    color = "#9E9E9F",
    fill = NA,
    size = 1
  ) + 
  scale_fill_gradient(low = "#ffffff", high = "#B7CD96") +
  # scale_fill_viridis(begin = 0, end = 0.9, direction = -1) +
  coord_fixed(expand = FALSE) +
  lims(x = c(134.15, 136), y = c(-15.2, -13.2)) +
  labs(x = "Longitude", y = "Latitude", color = "Species") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#c0ebe8", color = NA),
    legend.position = "none")
    
library(patchwork)
# small <- (panel_a / panel_b) 
# small + panel_c # fails

panels <- panel_c + 
  inset_element(panel_a, left = 0.05, right = 0.35, bottom = 0.7, top = 1)

ggsave("turtles_plot.png", panels, width = 15, height = 15, units = "cm")
