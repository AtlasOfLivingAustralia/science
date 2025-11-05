# -------------------------------------------------------- #
# Title: Download NatureMapr data for NSW Biodiversity Trust analysis (James Brazill-Boast request)
# Author: Dax
# Date: 2025-10-22
# -------------------------------------------------------- #

# This script downloads records associated with a species list on the ALA, 
# filters to select IBRA subregions, then downloads the data & generates a DOI.

# Data required for this download is on Teams:
# https://csiroau.sharepoint.com/:f:/r/sites/AtlasofLivingAustraliaTeams/Shared%20Documents/Teams/Science%20and%20Decision%20Support/Data/science/projects/2025-10-20_nsw-biodiversity-trust?csf=1&web=1&e=h65eWe

# ---

# packages
library(galah)
library(here)
library(readr)
library(dplyr)
library(purrr)

galah_config(email = "dax.kellie@csiro.au", run_checks = FALSE)

# search for species list on ALA
search_all(lists, "2025 NSW Biodiversity")

# list of species is on the ALA: 
# https://lists.ala.org.au/speciesListItem/list/dr33163


# load ibra regions
selected_regions <- readr::read_csv(here("projects",
                                         "2025-10-20_nsw-biodiversity-trust",
                                         "data",
                                         "selected-ibra-regions.csv")) |>
  janitor::clean_names()


selected_regions$sub_name_7 |> length()

# test query by returning counts
galah_call() |>
  filter(
    species_list == "dr33163",
    dataResourceName == "NatureMapr",
    cl1049 %in% selected_regions$sub_name_7
    ) |>
  group_by(cl1049) |>
  atlas_counts()


# Download data
result <- galah_call() |>
  filter(
    species_list == "dr33163",
    dataResourceName == "NatureMapr",
    cl1049 %in% selected_regions$sub_name_7
  ) |>
  select(
    group = "basic",
    dataResourceName, 
    dataResourceUid,
    basisOfRecord,
    cl1049 # IBRA subregions
    # cl1048  # IBRA regions
  ) |> 
  atlas_occurrences()

result

attributes(result)$doi
# https://doi.org/10.26197/ala.2a3b0122-e225-4925-8c94-102f1640cb6f





## All data (not just NatureMapr)

# test query by returning counts
galah_call() |>
  filter(
    species_list == "dr33163",
    # dataResourceName == "NatureMapr",
    cl1049 %in% selected_regions$sub_name_7
  ) |>
  group_by(cl1049) |>
  atlas_counts()


## Download

get_records <- function(region) {
  # Download data
  result <- galah_call() |>
    filter(
      species_list == "dr33163",
      cl1049 == {{region}}
    ) |>
    select(
      group = "basic",
      dataResourceName, 
      dataResourceUid,
      basisOfRecord,
      cl1049 # IBRA subregions
      # cl1048  # IBRA regions
    ) |> 
    atlas_occurrences(mint_doi = TRUE)
  
  result_with_doi <- result |>
    mutate(
      doi = attributes(result)$doi
    )
  
  # print the doi for good measure
  cli::cli_inform("{attributes(result)$doi}")
  
  return(result_with_doi)
}

# Download data
# download occurrence records & dois

regions_list <- as.list(selected_regions$sub_name_7)

tictoc::tic()
occs <- map(regions_list, get_records)

# merge
occs_combined <- occs |>
  bind_rows()
tictoc::toc(); beepr::beep(2)

# see dois that make up data
dois <- occs_combined |>
  distinct(cl1049, doi)

dois

# save
write.csv(dois,
          file = here("projects",
                      "2025-10-20_nsw-biodiversity-trust",
                      "data",
                      "downloaded-data",
                      "2025-10-28_doi-list_all-records.csv"))
