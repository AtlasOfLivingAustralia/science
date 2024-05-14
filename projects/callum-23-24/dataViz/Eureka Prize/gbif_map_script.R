# R script to produce map for Eureka Prize application (galah) of GBIF worldwide counts

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
  # remotes::install_github("wilkelab/gridtext")
  library(gridtext)
  library(pdftools)
  library(monochromeR)
}

setwd()

extrafont::loadfonts()

##### Set up parallel clusters #####
cl <- parallel::makeCluster(6)
doParallel::registerDoParallel(cl)

##### World shapefiles #####
world <- ne_countries(scale = "medium", returnclass = "sf")

world_continents <- ne_coastline(scale = "medium", returnclass = "sf")

##### World hex grid #####
hex_grid <- st_make_grid(tibble(geom = "POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))") |> 
                           st_as_sf(wkt = "geom", crs = 4326),
                         offset = c(-180,-96),
                         cellsize = 7.9,
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

##### Download occurrence counts from GBIF #####

### Takes ~20 minutes to run for hex size = 2 ###
### If there are issues with the edge geometries, you may need to play around
###   with the `offset` field of `st_make_grid()` above
gbif_hex_counts <- function(hex_grid) {
  counts <- foreach(i = hex_grid$hex_id, .combine = 'c',
                    .packages = c('rgbif', 'tibble')) %dopar% {
                      tibble(hex_id = i,
                             counts = occ_count(geometry = hex_grid$wkt_string[i]))
                    }
  return(counts)
}

### Run these lines to download the gbif counts
# hex_counts <- gbif_hex_counts(hex_grid)
# hex_counts_7.9 <- hex_counts

##### Join counts to hexes ######
# hex_grid_counts <- hex_grid |>
#   left_join(
#     tibble(
#       id = rep(hex_grid$hex_id, each = 2),
#       names = hex_counts |> unlist() |> names() |> as.vector(),
#       values = hex_counts |> unlist() |> as.vector()
#     ) |>
#       pivot_wider(id_cols = id, names_from = names, values_from = values) |>
#       select(-id),
#     by = "hex_id"
#   ) |>
#   st_set_geometry("hex_geometry") |>
#   st_transform(4326) |>
#   st_make_valid()
# 
# hex_grid_counts_7.9 <- hex_grid_counts
# save(hex_grid_counts_7.9, file = "hex_grid_counts_7.9.rds")

# The number refers to the size of the hex argument
load("hex_grid_counts_2.rds")
load("hex_grid_counts_7.9.rds")

##### Palettes #####
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
  paletteer_dynamic("cartography::green.pal", 20),
  paletteer_c("grDevices::PiYG", 30)[1:15] |> rev(),
  c("#FAEEDF", "#F7E2C7", "#F4CFA9", "#F1BA8B", "#EFA26E", "#EC8752", "#EA6936", "#E7491B")
)

##### PLOTS #####
###### MAPPING Function ######
blank_map <- function(hex_grid_counts, col_palette = palettes[[12]], font = "Roboto", filename = "blank_maps/hex_plot") {
  hex_plot <- ggplot() +
    geom_sf(data = hex_grid_counts, 
            aes(geometry = hex_geometry, fill = counts), 
            col = NA) +
    scale_fill_gradientn(
      colours = col_palette,
      transform = "log10",
      na.value = "#FFFFFF",
      n.breaks = 9,
      limits = c(1, 10^ceiling(log10(max(hex_grid_counts$counts)))),
      labels = label_number(accuracy = 1, scale_cut = cut_short_scale()),
      guide = guide_coloursteps(
        title = "# GBIF Occurrences",
        direction = "horizontal",
        show.limits = TRUE
      )
    ) +
    geom_sf(data = world_continents, 
            colour = "grey10", 
            fill = NA, 
            linewidth = 0.2) +
    theme_void() +
    theme(
      text = element_text(family = font),
      legend.position = "bottom",
      plot.margin = margin(t = -5, r = -5, b = -5, l = -5),
      legend.title = element_text(size = 7, hjust = 0.5, margin = margin(b = 3)),
      legend.title.position = "top",
      legend.key.height = unit(0.6, "lines"),
      legend.key.width = unit(3, "lines"),
      legend.text = element_text(size  = 5, vjust = 0.5, margin = margin(t = 1)),
      legend.margin = margin(t = -15)
    )
  
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = hex_plot,
         device = cairo_pdf, width = 8, height = 5, units = "in")
  
  pdf_convert(paste(filename, ".pdf", sep = ""),
              format = "png", dpi = 600,
              filenames = paste(filename, ".png", sep = ""))
}

blank_map(hex_grid_counts_7.9)

###### Plot small hexes (One colour scheme) ######

hex_plot_small1 <- ggplot() +
  geom_sf(data = hex_grid_counts_2, 
          aes(geometry = hex_geometry, fill = counts), 
          col = NA) +
  scale_fill_gradientn(
    colours = palettes[[1]],
    transform = "log10",
    na.value = "#FFFFFF",
    n.breaks = 9,
    limits = c(1, 10^8),
    labels = label_number(accuracy = 1, scale_cut = cut_short_scale()),
    guide = guide_coloursteps(
      title = "# GBIF Occurrences",
      direction = "horizontal",
      show.limits = TRUE
    )
  ) +
  geom_sf(data = world, 
          colour = "grey10", 
          fill = NA, 
          linewidth = 0.1) +
  theme_void() +
  theme(
    text = element_text(family = font),
    legend.position = "bottom",
    plot.margin = margin(t = -5, r = -5, b = -5, l = -5),
    legend.title = element_text(size = 7, hjust = 0.5, margin = margin(b = 3)),
    legend.title.position = "top",
    legend.key.height = unit(0.6, "lines"),
    legend.key.width = unit(3, "lines"),
    legend.text = element_text(size  = 5, vjust = 0.5, margin = margin(t = 1)),
    legend.margin = margin(t = -15)
  )
hex_plot_small1

# ggsave(filename = "blank_maps/hex_plot_small1.pdf", plot = hex_plot_small1,
#        device = cairo_pdf, width = 8, height = 5, units = "in")

###### Plot big hexes (One colour) gradientn ######
ggplot() +
  geom_sf(data = hex_grid_counts_7.9, 
          aes(geometry = hex_geometry, fill = counts), 
          col = NA) +
  scale_fill_gradientn(
    colours = palettes[[12]],
    transform = "log10",
    na.value = "#FFFFFF",
    n.breaks = 9,
    limits = c(1, 10^9),
    labels = label_number(accuracy = 1, scale_cut = cut_short_scale()),
    guide = guide_coloursteps(
      title = "# GBIF Occurrences",
      direction = "horizontal",
      show.limits = TRUE
    )
  ) +
  geom_sf(data = world_continents, 
          colour = "grey10", 
          fill = NA, 
          linewidth = 0.2) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto"),
    legend.position = "bottom",
    plot.margin = margin(t = -5, r = -5, b = -5, l = -5),
    legend.title = element_text(size = 7, hjust = 0.5, margin = margin(b = 3)),
    legend.title.position = "top",
    legend.key.height = unit(0.6, "lines"),
    legend.key.width = unit(3, "lines"),
    legend.text = element_text(size  = 5, vjust = 0.5, margin = margin(t = 1)),
    legend.margin = margin(t = -15)
  )
hex_plot_large1

# ggsave(filename = "blank_maps/hex_plot_large1.pdf", plot = hex_plot_large1,
#        device = cairo_pdf, width = 8, height = 5, units = "in")
# 
# pdf_convert("blank_maps/hex_plot_large1.pdf",
#             format = "png", dpi = 500,
#             filenames = "hex_plot_large1.png")

