

# packages
library(galah)
library(here)
library(readr)
library(dplyr)
library(purrr)

galah_config(email = "dax.kellie@csiro.au", run_checks = FALSE)

# load species list
species <- readr::read_csv(here("projects",
                                "2025-10-20_nsw-biodiversity-trust",
                                "data",
                                "BCTspeciesList.csv")) |>
  janitor::clean_names()


# load ibra regions
selected_regions <- readr::read_csv(here("projects",
                                        "2025-10-20_nsw-biodiversity-trust",
                                        "data",
                                        "selected-ibra-regions.csv")) |>
  janitor::clean_names()


# test query by returning counts
galah_call() |>
  filter(
    taxonConceptID %in% species$scientific_name[101:200],
    dataResourceName == "NatureMapr",
    cl1049 %in% selected_regions$sub_name_7
    ) |>
  select(
    group = "basic",
    dataResourceName, 
    dataResourceUid, 
    cl1049, # IBRA subregions
    cl1048  # IBRA regions
    ) |> 
  group_by(
    scientificName
    ) |>
  atlas_counts()


## DOWNLOAD DATA

# The query errors when it's too long
# split into list with dataframes of 50 rows, so queries only have 50 sp. each
species_split <- species |>
  group_split(grp = as.integer(gl(n(), 50, n())), .keep = FALSE)

get_records <- function(df) {
  
  # download occurrence records
  result <- galah_call() |>
    filter(
      taxonConceptID %in% df$guid,
      dataResourceName == "NatureMapr",
      cl1049 %in% selected_regions$sub_name_7
    ) |>
    select(
      group = "basic",
      dataResourceName, 
      dataResourceUid 
      # cl1049, # IBRA subregions
      # cl1048  # IBRA regions
    ) |> 
    atlas_occurrences(mint_doi = TRUE)
  
  # attach doi associated with download in its own column
  result_with_doi <- result |>
    mutate(
      doi = attributes(result)$doi
    )
  
  # print the doi for good measure
  cli::cli_inform("{cat(attributes(result)$doi)}")
  
  return(result_with_doi)
}

# download occurrence records & dois
occs <- species_split |>
  map(
    \(df)
    get_records(df)
    ) 

# merge
occs_combined <- occs |>
  bind_rows()


# see dois that make up data
dois <- occs_combined |>
  distinct(doi)

dois


# save
write.csv(occs_combined, 
          file = here("projects",
                      "2025-10-20_nsw-biodiversity-trust",
                      "data",
                      "downloaded-data",
                      "2025-10-20_ala-records.csv"))

write.csv(dois,
          file = here("projects",
                      "2025-10-20_nsw-biodiversity-trust",
                      "data",
                      "downloaded-data",
                      "2025-10-20_doi-list.csv"))
  
  


