# -----------------------------------------------------------------#
# Title: Gondwana rainforests - Download & save data
# Author: Dax Kellie
# -----------------------------------------------------------------#

# TODO: Notes for me
# - Download top 5 plants, fungi, reptiles, birds, mammals, inverts
# - Get images of them and save their urls + license info in a separate csv
# - plot them on a log scale (in order? or maybe use the beeswarm code from t shapiro)
# - Add map highlighting location of gondwana rainforests


# packages
# remotes::install_github("AtlasOfLivingAustralia/galah-R@dev") # v2.1.2
library(galah)
library(sf)
library(rmapshaper)
library(ozmaps)
library(ggplot2)
library(dplyr)

# --- Gondwana rainforest area

# Australian heritage sites
# From: https://fed.dcceew.gov.au/datasets/erin::national-heritage-list-spatial-database-nhl-public/about
heritage <- sf::st_read(here::here("data",
                                   "National_Heritage_List_Spatial_Database_(NHL)_-_public",
                                   "National_Heritage_List_Spatial_Database_(NHL)_-_public.shp")
                        ) |>
  rmapshaper::ms_simplify(keep = 0.01) |>
  sf::st_make_valid() |>
  sf::st_transform(crs = 4326)

# filter to gondwana
gondwana <- heritage |>
  filter(stringr::str_detect(NAME, "Gond"))


ggplot() +
  geom_sf(data = ozmap_states |>
            filter(NAME %in% c("New South Wales", "Queensland"))) +
  geom_sf(data = gondwana,
          aes(fill = "green")) +
  coord_sf(ylim = c(-35, -20))
  
gondwana_simple <- gondwana |>
  rmapshaper::ms_simplify(keep = 0.05)

## --- observations

# get counts in a loop by row because there are 2 shapefiles
# plants
counts_plants <- gondwana_simple |>
  group_split(row_number()) |>
  purrr::map(\(polygon)
             galah_call() |>
               identify("plantae") |>
               filter(year > 2020) |>
               apply_profile(ALA) |>
               galah_geolocate(polygon) |>
               group_by(scientificName) |>
               atlas_counts()
             ) |>
  bind_rows()

counts_plants <- counts_plants |> 
  group_by(scientificName) |>
  summarise(count = sum(count)) |>
  slice_max(n = 5, order_by = count) |>
  mutate(
    group = "plants",
    common_name = search_taxa(scientificName)$vernacular_name
    )

# fungi
counts_fungi <- gondwana_simple |>
  group_split(row_number()) |>
  purrr::map(\(polygon)
             galah_call() |>
               identify("fungi") |>
               filter(year > 2020) |>
               apply_profile(ALA) |>
               galah_geolocate(polygon) |>
               group_by(scientificName) |>
               atlas_counts()
  ) |>
  bind_rows()

counts_fungi <- counts_fungi |> 
  group_by(scientificName) |>
  summarise(count = sum(count)) |>
  slice_max(n = 5, order_by = count) |>
  mutate(
    group = "fungi",
    common_name = NA # search_taxa(scientificName)$vernacular_name returns nothing
  )



# reptiles
counts_reptiles <- gondwana_simple |>
  group_split(row_number()) |>
  purrr::map(\(polygon)
             galah_call() |>
               identify("reptilia") |>
               filter(year > 2020) |>
               apply_profile(ALA) |>
               galah_geolocate(polygon) |>
               group_by(scientificName) |>
               atlas_counts()
  ) |>
  bind_rows()

counts_reptiles <- counts_reptiles |> 
  group_by(scientificName) |>
  summarise(count = sum(count)) |>
  slice_max(n = 5, order_by = count) |>
  mutate(
    group = "reptiles",
    common_name = search_taxa(scientificName)$vernacular_name
  )

# birds
counts_birds <- gondwana_simple |>
  group_split(row_number()) |>
  purrr::map(\(polygon)
             galah_call() |>
               identify("aves") |>
               filter(year > 2020) |>
               apply_profile(ALA) |>
               galah_geolocate(polygon) |>
               group_by(scientificName) |>
               atlas_counts()
  ) |>
  bind_rows()

counts_birds <- counts_birds |> 
  group_by(scientificName) |>
  summarise(count = sum(count)) |>
  slice_max(n = 5, order_by = count) |>
  mutate(
    group = "birds",
    common_name = search_taxa(scientificName)$vernacular_name
  )
  
# mammals
counts_mammals <- gondwana_simple |>
  group_split(row_number()) |>
  purrr::map(\(polygon)
             galah_call() |>
               identify("animalia") |>
               filter(class != "Aves",
                      year > 2020) |>
               apply_profile(ALA) |>
               galah_geolocate(polygon) |>
               group_by(scientificName) |>
               atlas_counts()
  ) |>
  bind_rows()

counts_mammals <- counts_mammals |> 
  group_by(scientificName) |>
  summarise(count = sum(count)) |>
  slice_max(n = 5, order_by = count) |>
  mutate(
    group = "mammals",
    common_name = search_taxa(scientificName)$vernacular_name
  )
  

# invertebrates
counts_inverts <- gondwana_simple |>
  group_split(row_number()) |>
  purrr::map(\(polygon)
             galah_call() |>
               identify("animalia") |>
               filter(phylum != "Chordata",
                      year > 2020) |>
               apply_profile(ALA) |>
               galah_geolocate(polygon) |>
               group_by(scientificName) |>
               atlas_counts()
  ) |>
  bind_rows()

counts_inverts <- counts_inverts |>
  group_by(scientificName) |>
  summarise(count = sum(count)) |>
  slice_max(n = 5, order_by = count) |>
  mutate(
    group = "invertebrates",
    common_name = search_taxa(scientificName)$vernacular_name
  )

# MERGE
top_taxa <- list(
  counts_birds, counts_fungi, 
  counts_inverts, counts_plants, 
  counts_mammals, counts_reptiles
  ) |>
  bind_rows() |>
  mutate(
    common_name = case_when(
      is.na(common_name) ~ scientificName,
      .default = common_name
      )
  )

# search_taxa(top_taxa$scientificName)

# save list
write.csv(top_taxa, here::here("comms",
                               "2025-05-22_rainforests",
                               "data",
                               "top-taxa.csv"))

# images were added manually
# urls + licensing info added in top-taxa-images.csv





