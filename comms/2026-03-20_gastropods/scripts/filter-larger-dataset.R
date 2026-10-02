# ---------------------------------------------------------------------------- #
# Refine larger dataset to smaller gastropod dataset
# ---------------------------------------------------------------------------- #

# This script uses the complete taxonomic species checklist dataset and wrangles 
# it into a smaller, usable gastropod-only dataset

library(readxl)
library(dplyr)
library(tidyr)

species_all <- read_excel(
  here::here("data", "AllalaSpeciesChecklist25-2025-12-11.xlsx"),
  sheet = 6,
) |>
  janitor::clean_names() |>
  select(-simon_records)

species_with_counts <- read_excel(
  here::here("data", "AllalaSpeciesChecklist25-2025-12-11.xlsx"),
  sheet = 5,
) |>
  janitor::clean_names()

species_joined <- species_with_counts |>
  select(species_name, vernacular_name, number_of_records) |>
  right_join(species_all, 
             join_by(species_name == scientific_name)
  )

gastropods <- species_joined |>
  filter(stringr::str_detect(class, 
                             stringr::fixed("gastropoda", ignore_case=TRUE))
  ) |>
  replace_na(list(number_of_records = 0)) # replace NAs with 0s

# save
nanoparquet::write_parquet(gastropods, here::here("comms", "2026-03-20_gastropods", "data-processed", "gastropoda.parquet"))