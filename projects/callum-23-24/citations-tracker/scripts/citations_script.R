# Extracting scientific publications from GBIF that use ALA data

##### Libraries #####
library(tidyverse)
library(rgbif)
library(httr2)
library(readr)
library(purrr)
library(tibble)

##### Get Datasets #####
dataset_df <- request("https://api.gbif.org/v1/dataset/search/export?publishingCountry=AU&format=CSV") |>
  req_perform() |>
  resp_body_string() |>
  read_csv()

##### Takes ~2hrs to run #####
all_papers <- map(
  .x = 1:nrow(dataset_df),
  .f = function(x) {
    lit_counts <- lit_count(datasetKey = dataset_df$dataset_key[x])
    offset_seq <- seq(0, lit_counts, by = 500)
    
    this_dataset <- map(
      .x = 1:length(offset_seq),
      .f = function(y) {
        lit_api <- paste0("https://api.gbif.org/v1/literature/search?gbifDatasetKey=", 
                          dataset_df$dataset_key[x], 
                          "&limit=500&offset=", 
                          offset_seq[y])
        lit_df <- (request(lit_api) |>
              req_perform() |>
              resp_body_json())$results
        Sys.sleep(2)
        return(lit_df)
      }) |>
      list_flatten() |>
      map(.f = function(y) {
            paper_df <- y |> tibble() |> t() |> data.frame() |> tibble()
            colnames(paper_df) <- names(y)
            return(paper_df)}) |>
      list_rbind() |>
      mutate(dataset = dataset_df$title[x])
  },
  .progress = TRUE
) |>
  list_rbind()

save(all_papers, file = "path/to/all_papers.rds")
load("path/to/all_papers.rds")

##### Clean Dataframe #####
all_papers_clean <- all_papers |>
  select(-gbifDownloadKey, -gbifOccurrenceKey, -gbifNetworkKey, -gbifProjectIdentifier, -gbifProgramme, -tags) |>
  rowwise() |>
  mutate(
    across(-c(authors, dataset),
           function(x) unlist(x) |> paste0(collapse = " | "))) |>
  mutate(authors = map(.x = authors,
                       .f = function(x) paste0(x$firstName, " ", x$lastName)) |>
           unlist() |> paste0(collapse = ", ")) |>
  distinct() |>
  mutate(true_holder = TRUE) |>
  pivot_wider(names_from = dataset, values_from = true_holder, values_fill = FALSE)

write_csv(all_papers_clean, file = "path/to/all_papers_clean.csv")
