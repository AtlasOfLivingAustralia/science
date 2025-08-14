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
  filter(dataResourceName == "Australian Museum provider for OZCAM") |>
  group_by(species) |>
  atlas_counts(limit = 1e7)

not_aus_museum <- galah_call() |>
  filter(dataResourceName != "Australian Museum provider for OZCAM") |>
  group_by(species) |>
  atlas_counts(limit = 1e7)

# Species unique to humanobs data
aus_museum_only <- aus_museum |>
  filter(!species %in% not_aus_museum$species)


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


