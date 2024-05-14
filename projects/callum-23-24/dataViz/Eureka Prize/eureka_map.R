# R script to produce map for Eureka Prize application (galah)

{
  library(galah)
  library(tidyverse)
  library(sf)
  library(RColorBrewer)
  library(ggtext)
  library(ggrepel)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(rgbif)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(paletteer)
  library(ggnewscale)
  library(extrafont)
  library(scales)
}

setwd("C:/Users/WAI045/OneDrive - CSIRO/ALA/dataViz/Eureka Prize")

galah_config(email = "callumwaite2000@gmail.com",
             username = "cwaite",
             password = "GbiF.23",
             verbose = FALSE)

extrafont::loadfonts()

##### Set up parallel clusters #####
cl <- parallel::makeCluster(6)
doParallel::registerDoParallel(cl)

##### Available Atlases #####
atlases <- show_all_atlases()
atlases$region[6] <- "Denmark"

##### World shapefile #####
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  mutate(sup_by_galah = (name_en %in% atlases$region),
         plot_label = ifelse(name_en == "Denmark", "Denmark (GBIF)", name_en),
         centroid = st_centroid(geometry, of_largest_polygon = TRUE),
         # nudge = (centroid |> st_coordinates() |> data.frame() |> mutate(nudge = ifelse(Y > 0, 90 - Y, - (92 + Y))) |> pull(nudge)))
         nudge = (centroid |> st_coordinates() |> data.frame() |> mutate(nudge = 90 - Y) |> pull(nudge))
  )

# World hex grid
hex_grid <- st_make_grid(world,
                         cellsize = 8,
                         what = "polygons",
                         square = FALSE,
                         flat_topped = TRUE) |> 
  st_as_sf() |> 
  st_set_geometry("hex_geometry") |> 
  st_transform(4326) |> 
  rowid_to_column(var = "hex_id") |>
  rowwise() |>
  mutate(wkt_string = st_coordinates(hex_geometry) |>
                        as.data.frame() |>
                        select(-c(L1, L2)) |>
                        mutate(X = case_when(X < -180 ~ -180, X > 180 ~ 180, .default = X),
                               Y = case_when(Y < -90 ~ -90, Y > 90 ~ 90, .default = Y),
                               coords = paste(X, Y, sep = " ")) |>
                        pull(coords) |>
                        paste(collapse = ", "),
         wkt_string = paste0("POLYGON((", wkt_string, "))")) |>
  ungroup() |>
  st_drop_geometry() |>
  st_as_sf(wkt = "wkt_string", crs = 4326, remove = FALSE) |>
  st_set_geometry("hex_geometry") |>
  st_transform(4326)
 
##### Look to download occurrence counts from GBIF #####
galah_config(atlas = "gbif")

### Takes ~20 minutes to run ###
gbif_hex_counts <- function(hex_grid) {
  counts <- foreach(i = hex_grid$hex_id, .combine = 'c',
                         .packages = c('rgbif', 'tibble')) %dopar% {
                           tibble(hex_id = i,
                                  counts = occ_count(geometry = hex_grid$wkt_string[i]))
                         }
  return(counts)
}
hex_counts_8 <- gbif_hex_counts(hex_grid)

hex_grid_counts <- hex_grid |>
  left_join(
    tibble(
      id = rep(hex_grid$hex_id, each = 2),
      names = hex_counts |> unlist() |> names() |> as.vector(),
      values = hex_counts |> unlist() |> as.vector()
    ) |>
      pivot_wider(id_cols = id, names_from = names, values_from = values) |>
      select(-id),
    by = "hex_id"
  ) |>
  st_set_geometry("hex_geometry") |>
  st_transform(4326) |>
  mutate(sup_by_galah = st_intersects(hex_geometry, world |> filter(sup_by_galah))) |>
  rowwise() |>
  mutate(sup_by_galah = (length(sup_by_galah) > 0)) |>
  ungroup() |>
  mutate(sup_by_galah = unlist(sup_by_galah))

save(hex_grid_counts_8, file = "hex_grid_counts_8.rds")
load("hex_grid_counts.rds")

palettes <- list(
  paletteer_c("grDevices::BuGn", 30) |> rev(),
  paletteer_c("grDevices::BuPu", 30) |> rev(),
  paletteer_c("grDevices::PuRd", 30) |> rev(),
  paletteer_c("grDevices::Lajolla", 30)[5:30] |> rev(),
  paletteer_c("viridis::inferno", 30),
  paletteer_c("grDevices::PuBuGn", 30) |> rev(),
  paletteer_c("grDevices::Greens", 30) |> rev(),
  paletteer_c("grDevices::Blues", 30) |> rev(),
  paletteer_dynamic("cartography::pink.pal", 20),
  paletteer_dynamic("cartography::green.pal", 20)
)

custom_label_function <- function(x) {
  parse(text = paste0(10, "^", round(log10(x))))
    
}

##### Plot small hexes (One colour scheme) #####

ggplot() +
  geom_sf(data = hex_grid_counts_2, 
          aes(geometry = hex_geometry, fill = counts), 
          col = NA) +
  scale_fill_gradientn(
    colours = palettes[[1]],
    #direction = 1,
    transform = "log10",
    na.value = "#FFFFFF",
    n.breaks = 9,
    labels = custom_label_function,
    guide = guide_coloursteps(
      title = "# GBIF Occurrences",
      direction = "horizontal",
      show.limits = TRUE
    )
  ) +
  geom_sf(data = world, 
          colour = "grey10", 
          fill = NA, 
          linewidth = 0.2) +
  geom_sf(data = world |> filter(sup_by_galah), 
          colour = "#FFFFFF", 
          fill = NA, 
          linewidth = 0.5) +
  geom_text_repel(
    data = world |> filter(sup_by_galah),
    aes(geometry = centroid, label = plot_label),
    family = "Roboto",
    stat = "sf_coordinates",
    size = 4,
    colour = "grey10",
    segment.linetype = "dashed",
    segment.alpha = 0.5,
    box.padding = 0.25,
    max.overlaps = Inf,
    direction = "x",
    force = 3,
    #nudge_x = c(-10, 40, -40, -40, -30, -10, 40, 5, -50, 60, 5),
    nudge_y =  world |> filter(sup_by_galah) |> pull(nudge),
    hjust = c(-0.5),
    vjust = c(0.5)
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto"),
    legend.position = "bottom",
    plot.margin = margin(t = -5, r = -5, b = -5, l = -5),
    legend.title = element_text(hjust = 0.5),
    legend.title.position = "top",
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(3, "lines")
  )



##### Plot small hexes (Two colour scheme) #####

ggplot() +
  geom_sf(data = hex_grid_counts |> filter(!sup_by_galah), 
          aes(geometry = hex_geometry, fill = counts), 
          col = NA) +
  scale_fill_gradientn(
    colours = brewer.pal("PiYG", n = 11)[6:11],
    limits = c(1, (max(hex_grid_counts$counts))),
    transform = "log10",
    na.value = "#FFFFFF",
    guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = hex_grid_counts |> filter(sup_by_galah), 
          aes(geometry = hex_geometry, fill = counts), 
          col = NA) +
  scale_fill_gradientn(
    colours = brewer.pal("PiYG", n = 11)[6:1],
    limits = c(1, (max(hex_grid_counts$counts))),
    transform = "log10",
    na.value = "#FFFFFF",
    guide = "none"
  ) +
  geom_sf(data = world, 
          colour = "grey10",
          alpha = 0.5,
          fill = NA, 
          linewidth = 0.3) +
  geom_text_repel(
    data = world |> filter(sup_by_galah),
    aes(geometry = centroid, label = plot_label),
    stat = "sf_coordinates",
    size = 4,
    colour = "grey10",
    segment.linetype = "dashed",
    segment.alpha = 0.5,
    segment.curvature = 0,
    segment.square = FALSE,
    box.padding = 0.7,
    max.overlaps = Inf,
    direction = "x",
    force = 3,
    nudge_x = c(-10, 40, -40, -40, -30, -10, 40, 5, -50, 60, 5),
    nudge_y =  world |> filter(sup_by_galah) |> pull(nudge),
    hjust = c(0.5),
    vjust = c(1)
  ) +
  theme_void()

##### Plot big hexes (One colour) #####