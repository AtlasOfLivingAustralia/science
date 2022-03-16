
# Total plant records ALA
# Author: Shandiya Balasubramaniam
# Date: 2022-03-10

library(galah)
library(here)
library(dplyr)
library(ggplot2)


# get data -----
# 1. all records by year 
records_all <- galah_call() |>
  galah_identify("plantae", "chlorophyta") |>
  galah_filter(profile = "ALA", year >= 1900) |> 
  galah_group_by(year) |> 
  atlas_counts(limit = NULL) |> 
  mutate(year = as.integer(year)) |> 
  arrange(year) |> 
  mutate(cumul_all = cumsum(count))


# 2. records by type 
search_field_values("basisOfRecord")

# human observations
records_hobs <- galah_call() |>
  galah_identify("plantae", "chlorophyta") |>
  galah_filter(profile = "ALA", basisOfRecord == "HUMAN_OBSERVATION", year >= 1900) |> 
  galah_group_by(year) |> 
  atlas_counts(limit = NULL) |> 
  mutate(year = as.integer(year)) |> 
  arrange(year) |> 
  mutate(cumul_hobs = cumsum(count))

# preserved specimens
records_pre_spec <- galah_call() |>
  galah_identify("plantae", "chlorophyta") |>
  galah_filter(profile = "ALA", basisOfRecord == "PRESERVED_SPECIMEN", year >= 1900) |> 
  galah_group_by(year) |> 
  atlas_counts(limit = NULL) |> 
  mutate(year = as.integer(year)) |> 
  arrange(year) |> 
  mutate(cumul_pre_spec = cumsum(count))


# plot -----

source(here("projects", "plant-conservation-conf", "scripts", "theme_plant.R"))

p <- ggplot() +
  geom_line(data = records_all,
            aes(x = year, y = cumul_all),
            size = 1.2,
            colour = "#B9553C") +
  geom_line(data = records_hobs,
            aes(x = year, y = cumul_hobs),
            size = 0.8,
            colour = "#555555",
            linetype = "dashed") +
  geom_line(data = records_pre_spec,
            aes(x = year, y = cumul_pre_spec),
            size = 0.8,
            colour = "#555555",
            linetype = "dashed") +
  labs(x = "Year",
       y = "Plant records in ALA") +
  scale_y_continuous(labels = scales::comma) +
  annotate("text", x = 2030, y = 16e6, label = "All Records",
           colour = "#B9553C", size = 4.5) +
  annotate("text", x = 2030, y = 12e6, label = "Human \nObservations",
           colour = "#555555", size = 4) +
  annotate("text", x = 2030, y = 40e5, label = "Museum \nSpecimens",
           colour = "#555555", size = 4) +
  coord_cartesian(clip = "off") +
  theme_plant()
  
ggsave(here(
  "projects",
  "plant-conservation-conf",
  "plots",
  "cumulative-records.png"),
  p, height = 10, width = 10, units = "in")

