
# Cumulative records ALA from 1900
# Author: Shandiya Balasubramaniam
# Date: 2022-03-29


library(galah)
library(here)
library(dplyr)
library(ggplot2)
library(geomtextpath)


# get data -------
# 1. all records by year 
records_all <- galah_call() |>
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
  galah_filter(profile = "ALA", basisOfRecord == "HUMAN_OBSERVATION", year >= 1900) |> 
  galah_group_by(year) |> 
  atlas_counts(limit = NULL) |> 
  mutate(year = as.integer(year)) |> 
  arrange(year) |> 
  mutate(cumul_hobs = cumsum(count))

# preserved specimens
records_pre_spec <- galah_call() |>
  galah_filter(profile = "ALA", basisOfRecord == "PRESERVED_SPECIMEN", year >= 1900) |> 
  galah_group_by(year) |> 
  atlas_counts(limit = NULL) |> 
  mutate(year = as.integer(year)) |> 
  arrange(year) |> 
  mutate(cumul_pre_spec = cumsum(count))


# plot -----
source(here("projects", "ala101", "scripts", "theme_ala101.R"))

p <- ggplot() +
  geom_textline(data = records_all,
                aes(x = year, y = cumul_all),
                size = 7,
                colour = "#C44D34",
                linecolour = "#C44D34",
                label = "All Records", 
                fontface = "bold") +
  geom_textline(data = records_hobs,
                aes(x = year, y = cumul_hobs),
                size = 5,
                colour = "#637073",
                linecolour = "#637073",
                label = "Human Observations",
                linetype = "dashed",
                fontface = "bold",
                hjust = 0.8) +
  geom_textline(data = records_pre_spec,
                aes(x = year, y = cumul_pre_spec),
                size = 5,
                colour = "#637073",
                linecolour = "#637073",
                label = "Museum Specimens",
                linetype = "dashed",
                fontface = "bold",
                hjust = 0.98) +
  labs(x = "",
       y = "No. of records") +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(clip = "off") +
  theme_ala101() 


ggsave(here(
  "projects",
  "ala101",
  "plots",
  "cumulative-records.png"),
  p, height = 10, width = 10, units = "in")

