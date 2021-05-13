## ---------------------------
## Title: Visualisation of mapgpie occurrence records in the ALA 
## Author: Matilda Stevenson
## Date Created: 2021-04-14
## ---------------------------

library(ggplot2)
library(galah)
library(tidyr)
library(ozmaps)
library(sf)
library(hexbin)

# Download magpie occurrence records using `galah`
# Note: need to set an email address using `ala_config()`
magpie_occ <- ala_occurrences(
  taxa = select_taxa(term = "Cracticus tibicen"),
  filters = select_filters(profile = "ALA")
)

# Filter records not on the mainland/tasmania
filtered_occ <- magpie_occ %>% dplyr::filter(decimalLongitude < 155,
                                      decimalLongitude > 110,
                                      decimalLatitude > -45,
                                      decimalLatitude < -10)

# Hexagon map using geom_hex
ggplot(filtered_occ) +
  geom_hex(mapping = aes(x = decimalLongitude, y = decimalLatitude), bins = 55) +
  ylim(-45, -10) +
  xlim(110, 155) +
  scale_fill_gradientn(colours = c("#EEECEA", "#E06E53")) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none")


# Hexagon map with manually created hexagons

# Convert map of aus to same coordinate system as ALA points
aus <- st_transform(ozmaps::ozmap_country, 4326)

# transform points
points <- lapply(1:nrow(filtered_occ), function(x) {
  st_point(c(filtered_occ[x,'decimalLongitude'],
             filtered_occ[x,'decimalLatitude']))
})

# Build hexagonal grid
grid_all <- st_make_grid(aus, cellsize = 1, what = "polygons", square = FALSE)[aus]
gridSF <- st_as_sf(grid_all)

# Find which polygon each point is in
intersect <- st_intersects(st_as_sfc(points) %>% st_set_crs(4326), gridSF)

# count the number of points in each hexagon
freqs <- as.data.frame(table(unlist(intersect)), stringsAsFactors = FALSE)

names(freqs) <- c("hex_id", "count")

# Add counts to hexagons
freqs$hex_id <- as.integer(freqs$hex_id)

# fill in the values for hexagons with no records
filled_freqs <- freqs %>%
  complete(hex_id = full_seq(c(1,925), 1), fill = list(count = 0))
gridSF$n <- filled_freqs$count

# Plot the hexagons in ALA colours, with log-transformed counts
ggplot() +
  geom_sf(data = gridSF, aes(fill = n), size = .01) +
  scale_fill_gradientn(colours = c("#EEECEA", "#E06E53"), na.value = "white", trans = "log2") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none") +
  ylim(-45, -10) +
  xlim(110, 155)
