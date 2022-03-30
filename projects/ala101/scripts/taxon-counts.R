
# Counts of records within major groups from 1970 onwards
# Author: Shandiya Balasubramaniam
# Date: 2022-03-30

library(galah)
library(here)
library(dplyr)
library(ggplot2)
library(shadowtext)

# overview of counts of records in different groups -------
# some notes on filtering and grouping
# vertebrates = Classes Aves, Reptilia, Amphibia, Mammalia, Actinopterygii, Chondrichthyes
# invertebrates = ! Phylum Chordata within Kingdom Animalia
# plants = Kingdom Plantae 
# fungi = Kingdom Fungi

# vertebrates counts -------
vert_groups <- c("Reptilia",
                 "Amphibia",
                 "Aves",
                 "Mammalia",
                 "Actinopterygii",
                 "Chondrichthyes")

vert_counts <- galah_call() |> 
  galah_identify(vert_groups) |> 
  galah_filter(profile = "ALA",
               year >= 1970) |> 
  galah_group_by(class) |> 
  atlas_counts() |> 
  mutate(categories = case_when(
    class == "Aves" ~ "Birds",
    class == "Mammalia" ~ "Mammals",
    class == "Actinopterygii" | class == "Chondrichthyes" ~ "Fishes",
    class == "Amphibia" ~ "Amphibians",
    class == "Reptilia" ~ "Reptiles")) |> 
  group_by(categories) |> 
  summarise(count = sum(count))


# invertebrates counts ------
invert_counts <- galah_call() |> 
  galah_identify("Animalia") |>
  galah_filter(profile = "ALA",
               year >= 1970, 
               phylum != "Chordata") |>
  atlas_counts() 
  

# plants counts ------
plant_counts <- galah_call() |> 
  galah_identify("Plantae") |>
  galah_filter(profile = "ALA",
               year >= 1970) |> 
  atlas_counts()


# fungi counts ------
fungi_counts <- galah_call() |> 
  galah_identify("Fungi") |>
  galah_filter(profile = "ALA",
               year >= 1970) |> 
  atlas_counts()

# tidy before plotting -------
all_groups <- bind_rows(list(verts = vert_counts, 
                             inverts = invert_counts, 
                             plants = plant_counts, 
                             fungi = fungi_counts),
                        .id = "type")


tidy_groups <- all_groups |>
  mutate(categories = case_when(
    type == "inverts" ~ "Invertebrates",
    type == "plants" ~ "Plants",
    type == "fungi" ~ "Fungi",
    TRUE ~ categories)) |> 
  select(-type)
  
  
# plot --------
source(here("projects", "ala101", "scripts", "theme_ala101.R"))

# vertical barplot
p_vertical <- ggplot() +
  geom_col(data = tidy_groups,
           aes(x = reorder(categories, count), y = count),
           fill = "#003A70") +
  geom_shadowtext(data = tidy_groups,
                  aes(x = categories, y = count, label = scales::comma(count)),
                  bg.colour = "white", 
                  colour = "#212121", 
                  vjust = -0.3,
                  size = 5) +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::comma) + 
  theme_ala101() +
  theme(panel.grid.major.x = element_blank())

ggsave(here("projects",
            "ala101",
            "plots",
            "taxon_counts_vertical.png"),
       p_vertical, height = 10, width = 15, units = "in")

# horizontal bar plot
p_horizontal <- ggplot() +
  geom_col(data = tidy_groups,
           aes(x = reorder(categories, count), y = count),
           fill = "#003A70") +
  geom_shadowtext(data = tidy_groups,
                  aes(x = categories, y = count, label = scales::comma(count)),
                  bg.colour = "white", 
                  colour = "#212121", 
                  hjust = -0.1,
                  size = 5) +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 5e+07, by = 1e+07)) +
  expand_limits(y = 57000000) +
  coord_flip(clip = "off") +
  theme_ala101() +
  theme(panel.grid.major.y = element_blank())

ggsave(here("projects",
            "ala101",
            "plots",
            "taxon_counts_horizontal.png"),
       p_horizontal, height = 10, width = 15, units = "in")




