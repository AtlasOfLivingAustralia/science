
# ibra and imcra

library(ozmaps)
library(sf)
library(galah)
library(ggnewscale)
library(viridis)
library(scales)
library(showtext)
library(here)
library(tidyverse)
library(janitor)
library(marquee)

galah_config(email = "dax.kellie@csiro.au")



## create grid

# get a map and project to WGS84
oz_wgs84 <- ozmap_data(data = "country") |>
  st_transform(crs = st_crs("WGS84"))

## check map
ggplot(oz_wgs84) + geom_sf()

#### IBRA ####------------------------------------------------------------------

# create grid
oz_grid <- st_make_grid(oz_wgs84,
                        what = "polygons",
                        cellsize = .7,
                        square = FALSE,
                        flat_topped = TRUE)

# subset to grid cells that are within land
keep_hexes <- st_intersects(oz_grid, oz_wgs84)
keep_hexes <- as.data.frame(keep_hexes)$row.id
oz_grid <- oz_grid[keep_hexes]

## check
ggplot() +
  geom_sf(data = oz_wgs84) +
  geom_sf(data = oz_grid, fill = NA, color = "red")


## Load ibra regions

ibra <- st_read(here::here("data", "IBRA7_regions", "ibra7_regions.shp")) |>
  st_transform(crs = st_crs(4326)) |>
  st_make_valid() # important for calculating area later


## Get complete species list for every ibra region

ibra_region_names <- search_all(fields, "cl1048") |>
  show_values() |>
  pull(cl1048)


# -- set taxa ---------------------------------------------------------------- #
taxa_name <- "Chlorophyta"
# search_result <- search_taxa(tibble(class = "Amphibia", order = "Anura"))
# search_taxa("cetacea")

# NOTE: Currently this creates a list only to species level
get_species_list <- function(ibra_region_name) {
  
  result <- galah_call() |>
    identify("Chlorophyta") |>
    filter(cl1048 == {ibra_region_name}) |>
    atlas_species()
  
  
  # add name to results
  result |>
    mutate(
      ibra_region = ibra_region_name
    )
  
}

big_list <- purrr::map(ibra_region_names, get_species_list) |>
  bind_rows()

ibra_species_total <- big_list |>
  group_by(ibra_region) |>
  count()


## Calculate which ibra region each hexagon overlaps most

# Figure out total area of each hexagon

oz_grid_sf <- oz_grid |>
  st_as_sf() |>
  mutate(total_area = sf::st_area(x),
         hex_id = row_number())

hex_ibra_overlap <- st_intersection(oz_grid_sf, ibra) %>%
  mutate(
    area_intersect = sf::st_area(.),
    proportion = area_intersect / total_area
  )

# filter to only ibra region with top proportion overlap
hex_ibra_filtered <- hex_ibra_overlap |>
  group_by(hex_id) |>
  slice_max(proportion, n = 1) |>
  select(total_area, area_intersect, proportion, everything())

# Join hexagon ibra region + ibra total species count
hex_ibra_total <- hex_ibra_filtered |>
  select(hex_id, REG_NAME_7) |>
  st_drop_geometry() |>
  right_join(ibra_species_total, 
             join_by(REG_NAME_7 == ibra_region))


hex_name <- oz_grid_sf |>
  left_join(hex_ibra_total |> select(REG_NAME_7),
            join_by(hex_id)) |>
  mutate(
    taxa = taxa_name
  )

# download number of species in each hexagon
get_species_counts <- function(hexagon, ibra_name, taxa) {
  
  # get counts
  result <- galah_call() |>
    geolocate(hexagon) |>
    identify(taxa) |>
    filter(cl1048 == ibra_name) |>
    apply_profile(ALA) |>
    atlas_counts(type = "species", # get species counts
                 limit = NULL)
  
  # light formatting to catch errors
  # If no result, create tibble with NA value
  if(length(result) < 1){
    result <- tibble(
      total_area = hexagon$total_area,
      hex_id = hexagon$hex_id,
      x = hexagon$x,
      count = NA
    )
  }
  return(result)
  
}

# get count of species in each hexagon
grid_and_species <- hex_name |>
  # slice(133) |>
  group_split(hex_id) |>
  map(\(df)
      df |>
        mutate(
          get_species_counts(x, df$REG_NAME_7, df$taxa)
        )
  ) |>
  bind_rows(); beepr::beep(sound = 5)

# proportion of total species in area found in hexagon
hex_prop_species <- grid_and_species |>
  left_join(hex_ibra_total |> select(-REG_NAME_7), 
            join_by(hex_id == hex_id)) |> 
  replace_na(list(count = 0)) |>             # replace any NAs with zeroes
  mutate(
    prop_total_species = count/n
  )


#### IMCRA ####-----------------------------------------------------------------

# create grid
oz_grid2 <- st_make_grid(oz_wgs84,
                         what = "polygons",
                         cellsize = .7,
                         square = FALSE,
                         flat_topped = TRUE)

# subset to grid cells that are within land
keep_hexes <- st_intersects(oz_grid2, oz_wgs84)
keep_hexes <- as.data.frame(!keep_hexes)$row.id
oz_grid2 <- oz_grid2[keep_hexes]

## check
ggplot() +
  geom_sf(data = oz_wgs84) +
  geom_sf(data = oz_grid2, fill = NA, color = "red")

## Load imcra regions
imcra <- st_read(here::here("data", "imcra_provincial_bioregions", "Integrated_Marine_and_Coastal_Regionalisation_of_Australia_(IMCRA)_v4.0_-_Provincial_Bioregions.shp")) |>
  st_transform(crs = st_crs(4326)) |>
  st_make_valid() # important for calculating area later

## Get complete species list for every imcra region

imcra_region_names <- search_all(fields, "cl21") |>
  show_values() |>
  pull(cl21)


get_species_list_imcra <- function(imcra_region_name) {
  
  result <- galah_call() |>
    identify("Chlorophyta") |>
    filter(cl21 == {imcra_region_name}) |>
    atlas_species()
  
  
  # add name to results
  result |>
    mutate(
      imcra_region = imcra_region_name
    )
  
}

big_list_imcra <- purrr::map(imcra_region_names, get_species_list_imcra) |>
  bind_rows()

imcra_species_total <- big_list_imcra |>
  group_by(imcra_region) |>
  count()

## Calculate which ibra region each hexagon overlaps most

# Figure out total area of each hexagon

oz_grid_sf2 <- oz_grid2 |>
  st_as_sf() |>
  mutate(total_area = sf::st_area(x),
         hex_id = row_number())

hex_imcra_overlap <- st_intersection(oz_grid_sf2, imcra) %>%
  mutate(
    area_intersect = sf::st_area(.),
    proportion = area_intersect / total_area
  )

# filter to only ibra region with top proportion overlap
hex_imcra_filtered <- hex_imcra_overlap |>
  group_by(hex_id) |>
  slice_max(proportion, n = 1) |>
  select(total_area, area_intersect, proportion, everything())

# Join hexagon ibra region + ibra total species count
hex_imcra_total <- hex_imcra_filtered |>
  select(hex_id, PB_NAME) |>
  st_drop_geometry() |>
  right_join(imcra_species_total, 
             join_by(PB_NAME == imcra_region))


hex_name_imcra <- oz_grid_sf2 |>
  left_join(hex_imcra_total |> select(PB_NAME),
            join_by(hex_id)) |>
  mutate(
    taxa = taxa_name
  )


# download number of species in each hexagon
get_species_counts_imcra <- function(hexagon, imcra_name, taxa) {
  
  # get counts
  result <- galah_call() |>
    geolocate(hexagon) |>
    identify(taxa) |>
    filter(cl21 == imcra_name) |>
    apply_profile(ALA) |>
    atlas_counts(type = "species", # get species counts
                 limit = NULL)
  
  # light formatting to catch errors
  # If no result, create tibble with NA value
  if(length(result) < 1){
    result <- tibble(
      total_area = hexagon$total_area,
      hex_id = hexagon$hex_id,
      x = hexagon$x,
      count = NA
    )
  }
  return(result)
  
}

# get count of species in each hexagon
grid_and_species_imcra <- hex_name_imcra |>
  filter(!is.na(PB_NAME)) |>
  group_split(hex_id) |>
  map(\(df) {
      df |>
        mutate(
          get_species_counts_imcra(x, df$PB_NAME, df$taxa)
        )
  },
      .progress = TRUE
  ) |>
  bind_rows(); beepr::beep(sound = 5)

# proportion of total species in area found in hexagon
hex_prop_species_imcra <- grid_and_species_imcra |>
  left_join(hex_imcra_total |> select(-PB_NAME), 
            join_by(hex_id == hex_id)) |> 
  replace_na(list(count = 0)) |>             # replace any NAs with zeroes
  mutate(
    prop_total_species = count/n
  )



#### PLOT ####------------------------------------------------------------------

# investigator plot
# ggplot() +
#   geom_sf(
#     data = hex_prop_species,
#     mapping = aes(fill = ifelse(prop_total_species > 1, "blue", "grey60")),
#     alpha = 1,
#     color = "grey60") +
#   scale_fill_identity()




## PLOT
library(showtext)
font_add_google("Roboto", "roboto")
showtext_auto()

title <- glue::glue("*{taxa_name}*")

my_palette <- colorRampPalette(c("#ffffff", "#8BCBA5", "#08689c"))(5) 

ggplot() +
  geom_sf(
    data = hex_prop_species,
    mapping = aes(fill = prop_total_species),
    alpha = 1,
    color = "grey60") + 
  geom_sf(
    data = hex_prop_species_imcra,
    mapping = aes(fill = prop_total_species),
    alpha = 1,
    color = "grey60") + 
  geom_sf(
    data = oz_wgs84,
    fill = "transparent",
    colour = "grey20"
  ) +
  scale_fill_gradientn(name = "Proportion of \nIBRA/IMCRA region's\n\"complete\" species list",
                       colors = my_palette,
                       na.value = "grey70",
                       limits = c(0, 1),
                       breaks = c(0, .2, .4, .6, .8, 1),
                       label = scales::label_percent(),
                       guide = guide_colorsteps(direction = "vertical",
                                                title.position = "top",
                                                title.hjust = 0.5,
                                                show.limits = TRUE
                       )) +
  coord_sf(xlim = c(110, 155),
           ylim = c(-45, -8)) +
  labs(title = title, caption = "Atlas of Living Australia, May 2026") +
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(family = "roboto"),
    plot.caption = element_text(family = "roboto", hjust = 1),
    legend.title = element_text(family = "roboto", margin = margin(b = 4, unit = "mm")),
    legend.text = element_text(family = "roboto"),
    plot.background = element_rect(fill = "white", colour = "transparent"),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
    plot.margin = margin(t=1, l=1, b=1, r=1, unit = "cm")
  ); beepr::beep(sound = 2)


# save
showtext_opts(dpi = 300)
ggsave(
  file = here::here("projects", "data-poor-areas", "plots", "2026-05_chlorophyta.png"),
  dpi = 300,
  height = 7.5,
  width = 8.5
)


