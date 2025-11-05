# ------------------------------------------------------------------------- #
# Title: Species with records exclusively from Australian Museum in the ALA
# Author: Dax Kellie
# Date: 15 August 2025
# ------------------------------------------------------------------------- #

# At the request of Paul Flemons from the Australian Museum, this script finds
# the species where their data comes exclusively from contributions from 
# the Australian Museum. At the end, there are some functions that run
# loops that take quite a while to download all the species information and
# all the records.

library(galah)
library(dplyr)
galah_config(email = "dax.kellie@csiro.au")

# Get name of australian museum resource data
search_all(fields, "dataResourceName") |>
  search_values("museum")

galah_call() |>
  filter(dataResourceName == "Australian Museum provider for OZCAM") |>
  atlas_counts(type = "species")

galah_call() |>
  filter(dataResourceName == "Australian Museum provider for OZCAM") |>
  group_by(species) |>
  atlas_counts(limit = 1e7)

# Number of observations identified to the rank of 'species'
# (in case the discrepancy between the total obs count above is unclear)
aus_museum <- galah_call() |>
  filter(
    dataResourceName == "Australian Museum provider for OZCAM",
    taxonRank == "species"
  ) |>
  group_by(scientificName) |>
  atlas_counts(limit = 1e7)

not_aus_museum <- galah_call() |>
  filter(
    dataResourceName != "Australian Museum provider for OZCAM",
    taxonRank == "species"
  ) |>
  group_by(scientificName) |>
  atlas_counts(limit = 1e7)

# Species unique to humanobs data
aus_museum_only <- aus_museum |>
  filter(!scientificName %in% not_aus_museum$scientificName)


# test
galah_call() |>
  identify(aus_museum$species[1]) |>
  group_by(dataResourceName) |>
  atlas_counts()

# test
galah_call() |>
  identify(aus_museum_only$species[12]) |>
  group_by(dataResourceName) |>
  atlas_counts()

# 1. Species where Australian Museum supplies only available record
aus_museum_only |>
  filter(count < 2)

# 2. number of species that consist of only AM records
aus_museum_only |> nrow()


## Retrieve species info on species exclusive to Australian Museum

get_species_info <- function(species_name) {

  result <- galah_call() |>
    filter(scientificName == species_name) |>
    atlas_species()

  return(result)

}

species_info <- aus_museum_only |>
  pull(scientificName) |>
  purrr::map(
    \(species_name)
    get_species_info(species_name)
  ) |>
  bind_rows(); beepr::beep(2)

# join with record counts
aus_museum_only_complete <- species_info |>
  left_join(aus_museum_only, join_by(species_name == scientificName))


# check that everything merged correctly (i.e., no NA counts)
aus_museum_only_complete |>
  filter(is.na(count))

aus_museum_only_complete |>
  filter(count < 2)

aus_museum_only |>
  filter(stringr::str_detect(scientificName, "Homoneura"))


# save
write.csv(
  aus_museum_only_complete,
  file = here::here("data", "2025-08-15_ala-species_am-only.csv")
)

nanoparquet::write_parquet(
  aus_museum_only_complete,
  here::here("data", "2025-08-15_ala-species_am-only.parquet")
)



# Download all occurrence records for these AM-exclusive species
get_occurrences <- function(species_name) {
  result <- galah_call() |>
    filter(
      dataResourceName == "Australian Museum provider for OZCAM",
      taxonRank == "species",
      scientificName == species_name
    ) |>
    select(group = "basic", basisOfRecord) |>
    atlas_occurrences()

  return(result)
}

# NOTE: This takes ~2 hours to run
aus_museum_occs <- aus_museum_only |>
  pull(scientificName) |>
  purrr::map(
    \(species_name) {
      get_occurrences(species_name)
    }
  ) |>
  bind_rows(); beepr::beep(2)


# check that result is correct
aus_museum_only |>
  summarise(total = sum(count))

nrow(aus_museum_occs)


# save
write.csv(
  aus_museum_occs,
  file = here::here("data", "2025-08-15_ala-species_am-only_records.csv")
)

nanoparquet::write_parquet(
  aus_museum_occs,
  here::here("data", "2025-08-15_ala-species_am-only_records.parquet")
)



