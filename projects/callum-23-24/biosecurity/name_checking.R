library(readr)
library(tidyverse)

sr_test <- read_csv("dummy_testing_files/species_records_test_2814.csv") |>
  select(decimalLatitude, decimalLongitude, dataResourceName, scientificName, genus, species, subspecies, search_term, correct_name) |>
  distinct()
sr_main <- read_csv("dummy_testing_files/species_records_main_2814.csv") |>
  select(decimalLatitude, decimalLongitude, dataResourceName, scientificName, genus, species, subspecies, search_term, correct_name) |>
  distinct()

sr_test_long <- read_csv("dummy_testing_files/species_records_test_365.csv") |>
  select(decimalLatitude, decimalLongitude, eventDate, dataResourceName, scientificName, genus, species, subspecies, search_term, correct_name) |>
  distinct()
sr_main_long <- read_csv("dummy_testing_files/species_records_main_365.csv") |>
  select(decimalLatitude, decimalLongitude, eventDate, dataResourceName, scientificName, genus, species, subspecies, search_term, correct_name) |>
  distinct()

in_test_not_main <- sr_test_long |>
  filter(!(scientificName %in% sr_main_long$scientificName)) |>
  select(scientificName, correct_name, search_term) |>
  distinct()

in_main_not_test <- sr_main_long |>
  filter(!(scientificName %in% sr_test_long$scientificName)) |>
  select(scientificName, correct_name, search_term) |>
  distinct()
