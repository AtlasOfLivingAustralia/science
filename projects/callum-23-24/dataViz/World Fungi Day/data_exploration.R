##### Exploring FUNGI data in the ALA #####

##### Libraries #####
{
  library(alphahull)
  library(concaveman)
  library(galah)
  library(hexbin)
  library(monochromeR)
  library(ozmaps)
  library(sf)
  library(sfheaders)
  library(tidyverse)
}

galah_config(email = "callumwaite2000@gmail.com", verbose = FALSE)

##### Coastal Waters shpfile #####
CWA <- st_read("C://Users/WAI045/OneDrive - CSIRO/ALA/dataViz/World Fungi Day/shapefiles/CWA1980_zones/Coastal_Waters_AMB2020_Areas.shp") |>
  st_transform(4326)

coastal_waters_shp <- CWA |>
  rbind(
    CWA |>
      filter(COMMENT == "New South Wales") |>
      mutate(geometry = st_difference(sf_remove_holes(geometry),
                                      geometry),
             COMMENT = "Australian Capital Territory",
             NAME = "Coastal Waters (State Powers) Act 1980 - AMB2020 - Area - Australian Capital Territory",
             OBJNAM = NA,
             MRN = NA,
             Shape_Leng = NA, #doesn't work for now
             Shape_Area = NA) #different units to rest of the column
  ) |>
  dplyr::select(COMMENT, geometry) |>
  rename(state_long = COMMENT)

##### Map of Australia #####
aus <- st_transform(ozmap_data(data = "states"), 4326)

##### Data Download + Setup #####
fungi_species <- data.frame(
  species = c("Boletellus emodensis", # Shaggy Cap - magenta
              "Microporus affinis", # Micropore - purple/brown
              "Ramaria capitata", # - yellow
              "Mycena interrupta", # Pixie's Parasol - blue
              "Artomyces austropiperatus", # - white
              "Colus pusillus", # Craypot Stinkhorn - red
              "Cladonia fimbriata"), # Trumpet Lichen - green
  colour = c("#F7618F",
             "#842192",
             "#F7C328",
             "#33C8E1",
             "#E4C9C9",
             "#D7271C",
             "#7CC545"),
  species_id = c(2, 5, 4, 1, 7, 6, 3)
)

#   6 5
#  1 7 4
#   2 3

# c("Boletellus emodensis" = "palevioletred2", # Shaggy Cap - magenta
#   "Microporus affinis" = "mediumorchid", # Micropore - purple/brown
#   "Ramaria capitata" = "gold1", # - yellow
#   "Mycena interrupta" = "cyan2", # Pixie's Parasol - blue
#   "Artomyces austropiperatus" = "rosybrown1", # - white
#   "Colus pusillus" = "orangered2", # Craypot Stinkhorn - red
#   "Cladonia fimbriata" = "olivedrab2" # Trumpet Lichen - green
# )

search_taxa(fungi_species$species)

fungi <- galah_call() |>
  galah_identify(fungi_species$species) |>
  galah_select(group = c("basic"), species, vernacularName, coordinateUncertaintyInMeters,
               cl22, country) |>
  atlas_occurrences()

fungi_data <- fungi |>
  # Clean out incomplete data
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude) & !is.na(eventDate)) |>
  #filter(country == "Australia") |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs(4326),
           remove = FALSE) |>
  mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
           as.integer(),
         cw_state = if_else(is.na(intersection),
                            NA,
                            coastal_waters_shp$state_long[intersection])) |>
  filter(!is.na(cw_state)) |>
  select(-intersection, -cw_state)

write_csv(fungi_data, file = "data/fungi_data.csv")
  
##### Hull Map #####
fungi_hulls <- st_read("shapefiles/fungi_alphahulls/fungi_alphahulls.shp") |>
  arrange(desc(species))

hull_map <- ggplot() +
  geom_sf(data = aus, col = "grey50", fill = "gray5", alpha = 0.3) +
  geom_sf(data = fungi_hulls, aes(col = species), fill = NA, alpha = 0.5, linewidth = 1.5) +
  scale_colour_manual(values = alpha(fungi_species$colour |> 
                                      set_names(fungi_species$species),
                                     0.85)
                      ) +
  xlim(c(110,160)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray5"),
        legend.position = "none")

ggsave("plots/hull_map.pdf", plot = hull_map, width = 10, height = 7, units = "in")

##### Hex map #####
all_hex_grid <- st_make_grid(aus, cellsize = 2.5,
                             what = "polygons", square = FALSE, flat_topped = TRUE) |>
  st_as_sf() |>
  rename(geometry = x) |>
  mutate(hex_id = 1:n())

aus_hex_grid <- all_hex_grid |>
  filter(hex_id %in% (st_intersects(all_hex_grid, aus) |> 
                        as.data.frame() |>
                        pull(row.id) |>
                        unique())) |>
  mutate(hex_centre = st_centroid(geometry))

aus_hex_vertices <- map(
  .x = 1:dim(aus_hex_grid)[1], 
  .f = \(r) {aus_hex_grid[[1]][[r]][[1]] |>
      as_tibble() |>
      st_as_sf(coords = c("x", "y"),
               crs = st_crs(4326),
               remove = FALSE) |>
      mutate(hex_id = aus_hex_grid[[2]][[r]],
             hex_vertex = row_number()) |>
      select(-x, -y) |>
      rename(vertex = geometry)}) |>
  list_rbind()

fungi_data_hex <- fungi_data |>
  mutate(intersection = st_intersects(geometry, aus_hex_grid) |>
           as.integer(),
         hex_id = if_else(is.na(intersection),
                          NA,
                          aus_hex_grid$hex_id[intersection])) |>
  filter(!is.na(hex_id)) |>
  st_drop_geometry() |>
  select(species, hex_id) |>
  distinct() |>
  right_join(aus_hex_grid, by = "hex_id") |>
  left_join(fungi_species |> select(species, species_id), by = "species") |>
  filter(!is.na(species)) |>
  rename(geometry = geometry) |>
  st_as_sf()
  
fungi_data_hex_points <- fungi_data_hex |>
  left_join(aus_hex_vertices, by = c("hex_id", "species_id" = "hex_vertex")) |>
  st_as_sf() |>
  mutate(point_vertex = if_else(species_id == 7, hex_centre, vertex)) |>
  st_drop_geometry() |>
  select(-vertex) |>
  pivot_longer(cols = c(hex_centre, point_vertex), 
               names_to = "point", values_to = "geometry") |>
  st_as_sf() |>
  group_by(species, hex_id, species_id) |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING") |>
  left_join(fungi_data_hex |> 
              st_drop_geometry() |> 
              select(hex_id, species_id, hex_centre),
            by = c("hex_id", "species_id")) |>
  mutate(geometry = if_else(species_id == 7, hex_centre, st_centroid(geometry))) |>
  select(-hex_centre) |>
  st_as_sf()

hex_map <- ggplot() +
  geom_sf(data = fungi_data_hex |> select(geometry) |> distinct(), aes(geometry = geometry),
          fill = NA, col = alpha("grey30", 1), linewidth = 0.5) +
  geom_sf(data = aus, 
          col = alpha("grey40", 1), fill = NA, linewidth = 0.75) +
  geom_sf(data = fungi_data_hex_points,
          aes(geometry = geometry, col = species),
          size = 2) +
  scale_colour_manual(values = alpha(fungi_species$colour |> 
                                       set_names(fungi_species$species), 
                                     1)) +
  xlim(c(110,160)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray5"),
        legend.position = "none")

ggsave("plots/hex_map.pdf", plot = hex_map, width = 10, height = 7, units = "in")
ggsave("plots/hex_map.svg", plot = hex_map, width = 10, height = 7, units = "in")

##### Yearly Barplots #####

fungi_sp <- fungi_species$species[3]

plot_maker <- function(fungi_sp) {
  
  text_colour <- "grey40"
  bg_colour <- "grey5"
  fungi_colour <- fungi_species$colour[which(fungi_species$species == fungi_sp)]
  
  this_fungi_data <- fungi_data |>
    st_drop_geometry() |>
    filter(species == fungi_sp) |>
    mutate(month = factor(month.abb[month(eventDate)],
                          levels = month.abb)) |>
    group_by(month) |>
    summarise(count = n())
  
  this_fungi_label <- tibble(
    month = month.abb,
    month_label = toupper(month.abb),
    bar_num = 1:12
  ) |>
    mutate(angle = 105 - bar_num * 30 + 90,
           angle = ifelse(angle < 0, angle + 360, angle),
           angle = ifelse(angle > 90 & angle < 270, angle + 180, angle))
           
  this_fungi_plot <- ggplot(this_fungi_data) +
    geom_bar(stat = "identity",
             aes(x = month, y = count, fill = count)) +
    annotate(geom = "segment", colour = text_colour,
             x = 0.5, xend = 12.5, y = 0, yend = 0) +
    scale_x_discrete(drop = FALSE) +
    ylim(-2 * max(this_fungi_data$count), 
         max(this_fungi_data$count)) +
    coord_polar(start = 0, theta = "x") +
    geom_text(data = this_fungi_label, 
              aes(x = month, y = -35 / 440 * max(this_fungi_data$count), 
                  label = month_label), 
              color = text_colour, fontface = "bold", alpha = 1, size = 3.2, 
              angle = this_fungi_label$angle, inherit.aes = FALSE) +
    scale_fill_gradientn(colours = rev(generate_palette(fungi_colour, "go_lighter", 5))) +
    theme_classic() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(c(-1.2, -1.2, -1.2, -1.2), "cm"),
      #legend.position = c(0.5,0.15),
      legend.position = "none",
      panel.background = element_rect(fill = bg_colour, color = bg_colour)
    )
  this_fungi_plot
    
}

plot_maker_vert <- function(fungi_sp) {
  
  text_colour <- "grey60"
  bg_colour <- "grey5"
  fungi_colour <- fungi_species$colour[which(fungi_species$species == fungi_sp)]
  
  this_fungi_data <- fungi_data |>
    st_drop_geometry() |>
    filter(species == fungi_sp) |>
    mutate(month = factor(month.abb[month(eventDate)],
                          levels = month.abb)) |>
    group_by(month) |>
    summarise(count = n())
  
  this_fungi_label <- tibble(
    month = month.abb,
    month_label = toupper(month.abb),
    month_letter = substr(month.abb, 1, 1),
    bar_num = 1:12
  )
  
  this_fungi_plot <- ggplot(this_fungi_data) +
    geom_bar(stat = "identity",
             aes(x = month, y = count, fill = count), width = 0.75) +
    geom_text(data = this_fungi_label, 
              aes(x = month, y = 0, label = month_letter), 
              color = text_colour, fontface = "bold", alpha = 1, size = 4.5, vjust = 1.3) +
    scale_x_discrete(drop = FALSE) +
    coord_cartesian(ylim = c(0, max(this_fungi_data$count)), clip = 'off') +
    scale_fill_gradientn(colours = rev(generate_palette(fungi_colour, "go_lighter", 5))) +
    theme_classic() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      #plot.margin = unit(c(0,0,0,0), "cm"),
      #legend.position = c(0.5,0.15),
      legend.position = "none",
      panel.background = element_blank(), # element_rect(fill = bg_colour, color = bg_colour),
      plot.background = element_blank() # element_rect(fill = bg_colour, color = bg_colour)
    )
  
  return(this_fungi_plot)
  
}

for (i in 1:7) {
  fungi_sp <- fungi_species$species[i]
  barplot <- plot_maker_vert(fungi_sp)
  ggsave(paste0("plots/bar_", fungi_sp, ".svg"), plot = barplot, width = 6, height = 1.5, units = "in")
}
