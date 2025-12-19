
library(galah)
library(dplyr)

griis <- search_all(lists, "dr9884") |> show_values()

qld_non_native <- search_all(lists, "dr32213") |> 
  show_values(all_fields = TRUE) |>
  filter(Qld == 1)

galah_call() |>
  filter(taxonConceptID %in% griis$lsid[1:50]) |>
  group_by(scientificName) |>
  geolocate(lat = -16.99,
            lon = 145.42,
            radius = 20,
            type = "radius") |>
  atlas_counts()



library(galah)
library(dplyr)
library(ggplot2)
library(tidyr)

search_taxa("Anoplognathus Leach, 1815")

chistmas_beetles <- galah_call() |>
  identify("Anoplognathus Leach, 1815") |>
  filter(year > 1949) |>
  group_by(year) |>
  atlas_counts()



