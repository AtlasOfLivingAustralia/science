#---
# title: Function to make a hex map of occurrence records from galah 
# author: Olivia Torresan, Dax Kellie
# date: 14 August, 2024
#---

make_hex_map <- function(taxon, 
                         hex_colour_1,
                         hex_colour_2,
                         legend_text_colour) {
  
#filter records to within australian bounds
filtered_occ <- taxon |> filter(decimalLongitude < 155,
                                    decimalLongitude > 110,
                                    decimalLatitude > -45,
                                    decimalLatitude < -10)  
#hex map

aus <- st_transform(ozmaps::ozmap_country, 4326)

grid_all <- st_make_grid(aus, 
                         cellsize = 1, 
                         what = "polygons", 
                         square = FALSE,
                         flat_topped = TRUE)

# extract rows that are within AUS land
keep_hexes <- st_intersects(grid_all, aus) %>%
  as.data.frame(.) %>%
  pull(row.id)

# filter full grid to only hexagon IDs in AUS
oz_grid <- grid_all[keep_hexes]

taxon_points_sf <- filtered_occ %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
           crs = st_crs(4326))


intersect <- st_intersects(taxon_points_sf, oz_grid)

intersect[5:10]

counts <- as_tibble(table(unlist(intersect)), 
                    .name_repair = "unique") %>%
  rename("hex_id" = 1,
         "count" = 2) %>%
  mutate(hex_id = as.integer(hex_id)) %>%
  replace_na(list(count = 0))

oz_grid <- oz_grid %>%
  as_tibble() %>%
  mutate(id = row_number()) %>%
  full_join(counts,
            by = join_by(id == hex_id)) %>%
  st_as_sf()

#plot 
p <- ggplot() +
  geom_sf(data = oz_grid, aes(fill = count), size = .01) +
  scale_fill_gradientn(colours = c(hex_colour_1, hex_colour_2), 
                       na.value = "white", 
                       trans = "log10",
                       labels = scales::comma_format(),
                       n.breaks = 7,
                       guide = guide_colourbar(title = "Observations", title.position = "top")) +
  coord_sf(ylim = c(-45, -10), 
           xlim = c(110, 155)) +
  theme_void() +
  theme(legend.title = element_text(hjust = 0.5,
                                    family = "Roboto",
                                    size = 21),
        legend.position = "bottom",
        legend.text = element_text(size = 16,
                                   family = "Roboto",
                                   colour = legend_text_colour),
        legend.key.height = unit(1.5, "lines"),   # Adjust the height of the legend key
        legend.key.width = unit(3, "lines"),      # Adjust the width of the legend key
        legend.text.align = 0.5)

return(p) 

}


