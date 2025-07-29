# Shandiya Balasubramaniam 
# 28 July 2025
# Data visualisation for National Tree Day 2025
# 50 years (1976-2025) of tree data for 4 iconic species 

library(galah)
library(tidyverse)
library(ambient)
library(geomtextpath)
library(patchwork)
library(showtext)
font_add_google("Open Sans", "open_sans")
showtext_auto()
source("functions.R")

# get data ------
tree_spp <- tibble(species = c("Eucalyptus camaldulensis", 
                               "Eucalyptus pauciflora", 
                               "Eucalyptus regnans", 
                               "Ficus macrophylla"),
                   vernacular = c("River Red Gum", 
                                  "Snow Gum", 
                                  "Mountain Ash", 
                                  "Moreton Bay Fig")) 

counts_after_1976 <- galah_call() |> 
  identify(tree_spp$species) |> 
  filter(year >= 1976) |> 
  group_by(year, species) |> 
  atlas_counts() |> 
  mutate(year = as.numeric(year))

counts_before_1976 <- galah_call() |> 
  identify(tree_spp$species) |> 
  filter(year < 1976) |> 
  group_by(species) |> 
  atlas_counts()

tree_counts <- counts_before_1976 |> 
  mutate(year = 1975) |> 
  bind_rows(counts_after_1976) |> 
  group_by(species) |> 
  group_split(species) %>%           
  set_names(map(., ~.x$species[1]))

saveRDS(tree_counts, "tree_counts.rds")

# wrangle into the right format for plotting --------

# data for rings
tree_rings <- map(tree_counts, calculate_radius)

# data for year labels at 20-year intervals
year_labels <- tree_rings |> 
  map(\(df) df |> 
        filter(year %in% c(1980, 1990, 2000, 2010, 2020)) |> 
        group_by(year) |> 
        slice(175))

# data for tree names
tree_labels <- tree_spp |>
  group_by(species) |>
  group_split()

tree_names <- map2(.x = tree_rings, 
                   .y = tree_labels, 
                   .f = get_name_coords)



# colours
rings_pal <- colorRampPalette(c("#fdf7c2", "#f5c664", "#c38c2f"))(8)
text_col <- "#4D3712"
bg_pal <- c("#991218", "#3f5575", "#cd7400", "#647835")


# plot ------
# TODO: turn this into a proper purrr workflow
# also text kerning is suboptimal but have tried every single solution known to humankind :/
redgum <- plot_rings(tree_rings$`Eucalyptus camaldulensis`, 
           year_labs = year_labels$`Eucalyptus camaldulensis`, 
           name_labs = tree_names$`Eucalyptus camaldulensis`, 
           bg_pal = bg_pal[1])

ggsave("redgum.png", redgum, width = 4, height = 4, units = "in")

snowgum <- plot_rings(tree_rings$`Eucalyptus pauciflora`, 
                      year_labs = year_labels$`Eucalyptus pauciflora`, 
                      name_labs = tree_names$`Eucalyptus pauciflora`, 
                      bg_pal = bg_pal[2])

ggsave("snowgum.png", snowgum, width = 4, height = 4, units = "in")

ash <- plot_rings(tree_rings$`Eucalyptus regnans`, 
                  year_labs = year_labels$`Eucalyptus regnans`, 
                  name_labs = tree_names$`Eucalyptus regnans`, 
                  bg_pal = bg_pal[3])

ggsave("ash.png", ash, width = 4, height = 4, units = "in")

fig <- plot_rings(tree_rings$`Ficus macrophylla`, 
                  year_labs = year_labels$`Ficus macrophylla`, 
                  name_labs = tree_names$`Ficus macrophylla`, 
                  bg_pal = bg_pal[4])

ggsave("fig.png", fig, width = 4, height = 4, units = "in")

(ash + fig) / (snowgum + redgum)
ggsave("tree_rings.png", width = 8, height = 8, units = "in")
