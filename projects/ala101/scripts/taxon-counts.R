
# Counts of records within major groups from 1970 onwards
# Author: Shandiya Balasubramaniam
# Date: 2022-03-29

library(galah)
library(here)
library(dplyr)
library(ggplot2)

# OVERVIEW vertebrates, invertebrates, plants, fungi -------
# some notes on filtering and grouping
# vertebrates = Phylum Chordata
# invertebrates = ! Phylum Chordata within Kingdom Animalia
# plants = Kingdom Plantae AND Phylum Chlorophyta
# fungi = Kingdom Fungi

# get counts
verts <- galah_call() |> 
  galah_identify("Chordata") |>
  galah_filter(year >= 1970) |> 
  atlas_counts() 

inverts <-  galah_call() |> 
  galah_identify("Animalia") |>
  galah_filter(year >= 1970, 
               phylum != "Chordata") |>
  atlas_counts() 
  
plants <- galah_call() |> 
  galah_identify("Plantae", "Chlorophyta") |>
  galah_filter(year >= 1970) |> 
  atlas_counts()

fungi <- galah_call() |> 
  galah_identify("Fungi") |>
  galah_filter(year >= 1970) |> 
  atlas_counts() 

four_groups <- bind_rows(list(Vertebrates = verts, 
                              Invertebrates = inverts, 
                              Plants = plants, 
                              Fungi = fungi), 
                         .id = "group")


# plot 
source(here("projects", "ala101", "scripts", "theme_ala101.R"))

# vertical barplot
p1_v <- ggplot() +
  geom_col(data = four_groups,
           aes(x = group, y = count),
           fill = "#003A70") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::comma) +
  theme_ala101() +
  theme(panel.grid.major.x = element_blank())

ggsave(here("projects",
            "ala101",
            "plots",
            "overview-counts-v.png"),
       p1_v, height = 10, width = 10, units = "in")

# same plot, just horizontal
p1_h <- ggplot() +
  geom_col(data = four_groups,
           aes(x = group, y = count),
           fill = "#003A70") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + 
  theme_ala101() +
  theme(panel.grid.major.y = element_blank())

ggsave(here("projects",
            "ala101",
            "plots",
            "overview-counts-h.png"), 
       p1_h, height = 10, width = 10, units = "in")


# VERTEBRATES -------
# mammals, birds, fish (bony and cartilaginous), reptiles, amphibians

vert_groups <- c("Reptilia",
                 "Amphibia",
                 "Aves",
                 "Mammalia",
                 "Actinopterygii",
                 "Chondrichthyes")

vert_counts <- galah_call() |> 
  galah_identify(vert_groups) |> 
  galah_filter(year >= 1970) |> 
  galah_group_by(class) |> 
  atlas_counts() |> 
  mutate(group = case_when(
    class == "Aves" ~ "Birds",
    class == "Mammalia" ~ "Mammals",
    class == "Actinopterygii" | class == "Chondrichthyes" ~ "Fishes",
    class == "Amphibia" ~ "Amphibians",
    class == "Reptilia" ~ "Reptiles")) |> 
  group_by(group) |> 
  summarise(count = sum(count))

# vertical barplot
p2_v <- ggplot() +
  geom_col(data = vert_counts,
           aes(x = reorder(group, count), y = count),
           fill = "#003A70") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::comma, 
                     limits = c(0, 6e+07)) +
  theme_ala101() +
  theme(panel.grid.major.x = element_blank())

ggsave(here("projects",
            "ala101",
            "plots",
            "vert-counts-v.png"),
       p2_v, height = 10, width = 10, units = "in")

# same plot, just horizontal
p2_h <- ggplot() +
  geom_col(data = vert_counts,
           aes(x = reorder(group, count), y = count),
           fill = "#003A70") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 6e+07)) +
  coord_flip() + 
  theme_ala101() +
  theme(panel.grid.major.y = element_blank())

ggsave(here("projects",
            "ala101",
            "plots",
            "vert-counts-h.png"), 
       p2_h, height = 10, width = 10, units = "in")


# INVERTEBRATES -------
# arthropods, mollusks, annelid and cnidarians

invert_groups <- c("Mollusca",
                 "Arthropoda",
                 "Annelida",
                 "Cnidaria")

invert_counts <- galah_call() |> 
  galah_identify(invert_groups) |> 
  galah_filter(year >= 1970) |> 
  galah_group_by(phylum) |> 
  atlas_counts()

# vertical barplot
p3_v <- ggplot() +
  geom_col(data = invert_counts,
           aes(x = reorder(phylum, count), y = count),
           fill = "#003A70") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::comma) +
  theme_ala101() +
  theme(panel.grid.major.x = element_blank())

ggsave(here("projects",
            "ala101",
            "plots",
            "invert-counts-v.png"),
       p3_v, height = 10, width = 10, units = "in")

# same plot, just horizontal
p3_h <- ggplot() +
  geom_col(data = invert_counts,
           aes(x = reorder(phylum, count), y = count),
           fill = "#003A70") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + 
  theme_ala101() +
  theme(panel.grid.major.y = element_blank())

ggsave(here("projects",
            "ala101",
            "plots",
            "invert-counts-h.png"), 
       p3_h, height = 10, width = 10, units = "in")


# PLANTS (incomplete) ------- 
# angiosperms, gymnosperms, pteridophytes, bryophyte

plant_groups <- c("Pteridophyta",
                   "Bryophyta",
                   #angiosperms "",
                   #gymnosperms"")

plant_counts <- galah_call() |> 
  galah_identify(plant_groups) |> 
  galah_filter(year >= 1970) |> 
  galah_group_by(?) |> 
  atlas_counts()





all_subsets <- bind_rows(vert_counts, invert_counts) 
