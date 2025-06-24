# Literature tracker spatial services

library(here)
library(tidyverse)
library(janitor)
library(lubridate)

# Load data
data <- read_csv(here("projects", 
                      "literature-tracking", 
                      "data", 
                      "2025-05-20_publications.csv"
))

# Wrangle
# extract relevant date data in correct format, select columns
dataclean <- data |>
  clean_names() |> # clean column names
  mutate(
    date_added_clean = lubridate::ymd_hms(date_added),
    date_added_month = month(date_added_clean, label = TRUE),
    date_added_year = year(date_added_clean)
  ) |>
  select(title, date, publication_year, item_type, 
         url, manual_tags, date_added, date_added_clean, 
         date_added_month, date_added_year)

dataclean


# clean
prefix <- c("1 - ", "2 - ", "3 - ", "4 - ", "5 - ", "6 - ") # text to remove

# remove prefix
tags_clean <- dataclean %>% # remove numbers
  mutate( 
    tags = str_remove_all(manual_tags, 
                          paste(prefix, collapse = "|")) 
  )

# Separate tags into separate rows (allowing more than one row per paper)
ALA_tags <- tags_clean %>%
  mutate(tags = strsplit(as.character(tags), ";")) %>% # split names where there is a semicolon
  unnest(tags) %>% # make them occupy multiple rows
  mutate(
    tags = str_trim(tags) # trim whitespace around string 
  )


spatial_tags <- ALA_tags |>
  filter(tags == c("Spatial Portal", "Map", "SPECIES DISTRIBUTION")) |> 
  filter(!duplicated(title)) |>
  group_by(publication_year) |> 
  count() |>
  drop_na(publication_year)


dataclean |> 
  group_by(publication_year) |>
  count() |>
  left_join(spatial_tags, join_by(publication_year)) |>
  rename("total" = "n.x",
         "spatial" = "n.y") |>
  replace_na(list(spatial = 0)) |>
  arrange(desc(publication_year)) |>
  mutate(prop = spatial/total) |>
  ggplot() + 
  geom_bar(aes(x = publication_year,
               y = prop),
           stat = "identity",
           fill = "#D39268") + 
  scale_y_continuous(labels = scales::percent) +
  pilot::theme_pilot() + 
  labs(subtitle = "Proportion of publication tagged with spatial tags") +
  theme(
    legend.position = "none"
  )

spatial_tags |> # ungroup() |> summarise(total = sum(n))
  ggplot(aes(x = publication_year,
             y = n)
         ) +
  geom_bar(stat = "identity",
           fill = "#2F3C3C") +
  pilot::theme_pilot() + 
  labs(subtitle = "Tagged as 'SPECIES DISTRIBUTION', 'Spatial Portal', 'Map'") +
  theme(
    legend.position = "none"
  )

ALA_tags |>
  filter(stringr::str_detect(tags, "Max"))

ALA_tags |>
    filter(tags == c("SPECIES DISTRIBUTION")) |> 
    filter(!duplicated(title)) |>
    group_by(publication_year) |> 
    count() |>
    drop_na(publication_year) |> # ungroup() |> summarise(total = sum(n))
  ggplot(aes(x = as.factor(publication_year),
             y = n)
  ) +
    geom_bar(stat = "identity",
             fill = "#2F3C3C") +
    pilot::theme_pilot() + 
    labs(subtitle = "Tagged as 'SPECIES DISTRIBUTION'") +
    theme(
      legend.position = "none"
    )


