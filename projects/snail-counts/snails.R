# -----------------------------------------------------#
## Comparing observations of 2 snail genera in Sydney
## Author: Dax Kellie
## Date: 1 September 2023
# -----------------------------------------------------#

#| This script creates a map of Sydney local areas, comparing regions with greater
#| numbers of Austrochloritis or Sauroconcha

#| Data for this script can be found in the Science & Decision Support folder on Microsoft Teams:
#| ./Data/science/projects/snail-counts

# load packages
library(galah)
library(tidyverse)
library(sf)
library(here)
library(ozmaps)
library(rmapshaper)
library(scales)
library(ggnewscale)
library(ggtext)
library(glue)

galah_config(email = "dax.kellie@csiro.au")


## SNAILS
search_taxa("Sauroconcha", "Austrochloritis")

# Record counts
galah_call() |>
  identify("Sauroconcha", "Austrochloritis") |>
  group_by(genus) |>
  galah_apply_profile(ALA) |> # set of data cleaning filters
  atlas_counts()

# Download observations
snails <- galah_call() |>
  identify("Sauroconcha", "Austrochloritis") |>
  galah_apply_profile(ALA) |>
  galah_select(group = "basic", genus) |>
  atlas_occurrences() |>
  drop_na(decimalLongitude, decimalLatitude) # remove obs with missing coordinates


# Un-zip folder if you wanna do it with code
# zip_folder <- here("projects", "snail-counts", "data", "nsw_loc_gda2020.zip")
# output_dir <- here("projects", "snail-counts", "data", "GDA2020")
# unzip(zip_folder, exdir = output_dir) 


# read in shapefile
sydney <- st_read(here("projects",
                       "snail-counts",
                       "data",
                       "GDA2020",
                       "nsw_localities.shp")) |>
  ms_simplify(keep = 0.1) |> 
  st_transform(crs = st_crs("WGS84")) |> 
  st_make_valid()

sydney

# testing whether it plots correctly
ggplot() +
  geom_sf(data = sydney,
          fill = NA,
          colour = "grey60") +
  theme_void()

# wow that's a lot of shapes!

# add points on top
ggplot() +
  geom_sf(data = sydney,
          fill = NA,
          colour = "grey60") +
  geom_point(data = snails,
             aes(x = decimalLongitude,
                 y = decimalLatitude,
                 colour = genus),
             alpha = 0.5) +
  theme_void()

# ok so there are lots that are outside of this range too
# those points will go away in the next step


#### Data wrangling

# Separate snails into different data frames
snails_sauro <- snails |>
  filter(genus == "Sauroconcha")

snails_austro <- snails |>
  filter(genus == "Austrochloritis")
  
# convert them to spatial objects, set projection
snail_sauro_sf <- snails_sauro |> 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
           crs = st_crs("WGS84"))

snail_austro_sf <- snails_austro |> 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
           crs = st_crs("WGS84"))

# count points
sydney_counts_sauro <- sydney |> 
  mutate(sauro_count = pmap_dbl(.l = list(x = sydney$geometry),
                               .f = function(x) {
                                 lengths(st_intersects(x, snail_sauro_sf))
                               }))

sydney_counts_austro <- sydney |> 
  mutate(austro_count = pmap_dbl(.l = list(x = sydney$geometry),
                                .f = function(x) {
                                  lengths(st_intersects(x, snail_austro_sf))
                                }))


## Merge data

# merge counts into single data frame
merged_snails <- sydney_counts_austro |>
  as_tibble() |>
  select(LOC_NAME, austro_count) |>
  right_join(sydney_counts_sauro, by = join_by(LOC_NAME)) |>
  st_as_sf()

# add column with difference in count
# Positive numbers indicate more austro snails
# Negative numbers indicate more sauro snails
merged_snails_diff <- merged_snails |>
  mutate(count_diff = austro_count - sauro_count)



#### Map

# Add info to make nicer title
snails_title <- glue(
  "Regions of New South Wales with comparitively higher number of  
  <span style='color:{snails_palette$austro}'><b>Austro snails</b></span> 
  or 
  <span style='color:{snails_palette$sauro}'><b>Sauro snails</b></span>
  in New South Wales local regions")

snails_palette <- list("sauro" = "red",
                       "austro" = "blue")

# Make map
ggplot() +
  geom_sf(
    data = merged_snails_diff,
    mapping = aes(fill = count_diff),
    alpha = 1,
    color = NA) +
  scale_fill_distiller(name = "Difference in record count",
                       type = "div",
                       palette = "RdBu",
                       trans = pseudo_log_trans(0.001, 10), # NOTE: This scale is pseudo log-transformed
                       direction = 1,
                       na.value = "grey90",
                       breaks = c(-100, -10, 0, 10, 100),
                       guide = guide_colorsteps(direction = "horizontal",
                                                label.position = "top",
                                                title.position = "bottom",
                                                title.hjust = 0.5)) +
  theme_void() +
  # add nice label
  ggnewscale::new_scale_color() +
  scale_colour_manual(values = snails_palette) +
  labs(title = snails_title) +
  # aesthetic touches
  theme(legend.position = "bottom",
        legend.key.width = unit(25, 'mm'),
        plot.title = element_markdown(size = 16, hjust = 0.5),
        plot.background = element_rect(fill = 'white', colour = 'white'))


# Save
ggsave(here("projects",
            "snail-counts",
            "map_snails.png"))
