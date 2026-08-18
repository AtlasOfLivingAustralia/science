

library(here)
library(tidyverse)
library(janitor)
library(lubridate)

# Load data
data <- read_csv(here("projects", 
                      "literature-tracking", 
                      "data", 
                      "2026-06-25_ala-publications.csv"
))

# Wrangle
# extract relevant date data in correct format, select columns
dataclean <- data |>
  clean_names() |> # clean column names
  mutate(
    date = lubridate::ymd(date, truncated = 2),
    date_added_clean = lubridate::ymd_hms(date_added),
    date_added_month = month(date_added_clean, label = TRUE),
    date_added_year = year(date_added_clean)
  ) |>
  select(title, date, place, publication_year, item_type, 
         url, manual_tags, date_added, date_added_clean, 
         date_added_month, date_added_year)

dataclean_filtered <- dataclean |>
  filter(if_any(everything(), ~ str_detect(., regex("department", ignore_case = TRUE))))

dataclean_filtered |>
  filter(duplicated(title))


google_scholar <- readRDS(here::here("projects", 
                                     "literature-tracking", 
                                     "data",
                                     "gscholar_search.RDS")) |> 
  tibble::as_tibble()

# References on Google Scholar that are not in publication tracker
google_scholar |>
  filter(!title %in% dataclean$title) |>
  View()

