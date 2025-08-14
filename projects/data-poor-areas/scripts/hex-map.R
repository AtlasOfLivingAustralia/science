

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

galah_config(email = "dax.kellie@csiro.au")



## create grid

# get a map and project to WGS84
oz_wgs84 <- ozmap_data(data = "country") |>
  st_transform(crs = st_crs("WGS84"))

## check map
ggplot(oz_wgs84) + geom_sf()

# create grid
oz_grid <- st_make_grid(oz_wgs84,
                        what = "polygons",
                        cellsize = 1.0,
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

# a test
galah_call() |>
  identify("reptilia") |>
  filter(cl1048 == "NSW North Coast") |>
  # group_by(taxonConceptID) |>
  atlas_species()


# NOTE: Currently this creates a list only to species level
get_species_list <- function(ibra_region_name) {
  
  result <- galah_call() |>
    identify("aves") |>
    filter(cl1048 == {ibra_region_name}) |>
    atlas_species()
  
  # add name to results
  result |>
    mutate(
      ibra_region = ibra_region_name
    )
  
}

big_list <- purrr::map(ibra_region_names, get_species_list) |>
  bind_rows(); beepr::beep(sound = 2)

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
  ); beepr::beep(sound = 2)
  
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


# download number of species in each hexagon
get_species_counts <- function(hexagon) {
  
  # get counts
  result <- galah_call() |>
    geolocate(hexagon) |>
    identify("aves") |>
    # filter(decimalLongitude > 110) |>
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
grid_and_species <- oz_grid_sf |>
  # slice(133) |>
  group_split(hex_id) |>
  map(\(df)
      df |>
        mutate(
          get_species_counts(x)
        )
        ) |>
  bind_rows(); beepr::beep(sound = 2)

# proportion of total species in area found in hexagon
hex_prop_species <- grid_and_species |>
  left_join(hex_ibra_total, 
            join_by(hex_id == hex_id)) |>
  mutate(
    prop_total_species = count/n
  )


## PLOT

ggplot() +
  geom_sf(
    data = hex_prop_species,
    mapping = aes(fill = prop_total_species),
    alpha = 1,
    color = "grey60") + 
  scale_fill_distiller(name = "Proportion of \n'complete' species \nin IBRA region",
                       type = "seq",
                       palette = "GnBu",
                       # trans = pseudo_log_trans(0.001, 10), # NOTE: This scale is pseudo log-transformed
                       direction = 1,
                       na.value = "grey70",
                       limits = c(0, 1),
                       breaks = c(0, .2, .4, .6, .8, 1),
                       label = scales::label_percent(),
                       guide = guide_colorsteps(direction = "vertical",
                                                # label.position = "left",
                                                title.position = "top",
                                                title.hjust = 0.5,
                                                show.limits = TRUE
                                                )) +
  theme_void()

# TODO: calculate difference from matching region's complete list

