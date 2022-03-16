
# Plant records by family ALA
# Author: Shandiya Balasubramaniam
# Date: 2022-03-11

library(galah)
library(here)
library(dplyr)
library(ggplot2)
library(ggrepel)

# get data ---------

# count of records by year and family from 1900 
family_counts <- galah_call() |>
  galah_identify("plantae", "chlorophyta") |>
  galah_filter(profile = "ALA", year >= 1900) |> 
  galah_group_by(year, family) |> 
  atlas_counts(limit = NULL) 

# cumulative counts by family from 1900
family_cumulative <- family_counts |> 
  mutate(year = as.integer(year)) |> 
  arrange(year) |> 
  group_by(family) |>
  mutate(cumulative_count = cumsum(count))

# plot ------

source(here("projects", "plant-conservation-conf", "scripts", "theme_plant.R"))

# all families
p1 <- ggplot() +
  geom_line(data = family_cumulative,
            aes(x = year, y = cumulative_count, group = family),
            colour = "lightgrey") +
  labs(x = "Year",
       y = "Plant records in ALA") +
  scale_y_continuous(labels = scales::comma) +
  theme_plant()

ggsave(here(
  "projects",
  "plant-conservation-conf",
  "plots",
  "cumulative-by-family.png"),
  p1, height = 10, width = 10, units = "in",)

# top 5
top_4 <- galah_call() |>
  galah_identify("plantae", "chlorophyta") |>
  galah_filter(profile = "ALA", year >= 1900) |> 
  galah_group_by(family) |> 
  atlas_counts(limit = 4) 

# # bottom 20 (families with only 1 record)
# bottom_20 <- galah_call() |>
#   galah_identify("plantae", ) |>
#   galah_filter(profile = "ALA", year >= 1900) |> 
#   galah_group_by(family) |> 
#   atlas_counts(limit = NULL) |> 
#   slice_tail(n = 20)

# categorise families
ranked <- family_cumulative |> 
  mutate(rank = case_when(
    family %in% top_4$family ~ "top4",
    TRUE ~ "other"))

# plot top 4 families 
p2 <- ggplot() +
  geom_line(data = ranked,
            aes(x = year, y = cumulative_count, group = family, colour = rank)) +
  annotate("text", x = 2030, y = top_4$count[1], label = top_4$family[1],
           colour = "#B9553C", size = 4) + 
  annotate("text", x = 2030, y = top_4$count[2], label = top_4$family[2],
           colour = "#B9553C", size = 4) + 
  annotate("text", x = 2030, y = top_4$count[3], label = top_4$family[3],
           colour = "#B9553C", size = 4) + 
  annotate("text", x = 2030, y = top_4$count[4], label = top_4$family[4],
           colour = "#B9553C", size = 4) + 
  coord_cartesian(clip = "off") +
  scale_colour_manual(values = c("lightgrey", "#B9553C")) +
  labs(x = "Year",
       y = "Records in ALA") +
  scale_y_continuous(labels = scales::comma) +
  theme_plant() +
  theme(legend.position = "none")

ggsave(here(
  "projects",
  "plant-conservation-conf",
  "plots",
  "cumulative-top4.png"),
  p2, height = 10, width = 10, units = "in")

# need to figure out what to do about invalid taxonomy
# not used because some families are not valid
# plot bottom 20 families
# ggplot() +
#   geom_line(data = ranked,
#             aes(x = year, y = cumulative_count, group = family), 
#             colour = "lightgrey") +
#   geom_point(data = filter(ranked, family %in% bottom_20$family),
#              aes(x = year, y = cumulative_count),
#              colour = "#B9553C",
#              size = 2) +
#   coord_cartesian(clip = "off") +
#   geom_text_repel(data = filter(ranked, family %in% bottom_20$family),
#             aes(x = year, y = cumulative_count, label = family),
#             colour = "#B9553C",
#             size = 4,
#             max.overlaps = Inf,
#             xlim = c(-Inf, Inf),
#             ylim = c(-Inf, Inf)) +
#   labs(x = "Year",
#        y = "Records in ALA") +
#   scale_y_continuous(labels = scales::comma, expand = ) +
#   theme_plant() +
#   theme(legend.position = "none")



  
  

