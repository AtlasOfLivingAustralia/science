
# Density of plant records in IBRA and IMCRA regions
# Author: Shandiya Balasubramaniam
# Date: 2022-03-16

library(galah)
library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(showtext)
library(ggnewscale)

# get all the data ----------

# get IBRA and IMCRA colnames
fields <- show_all_fields()

# get counts 
ibra_counts <- galah_call() |>
  galah_identify("plantae", "chlorophyta") |>
  galah_filter(profile = "ALA") |> 
  galah_group_by("cl1048") |>      # IBRA regions
  atlas_counts()

imcra_counts <- galah_call() |>
  galah_identify("plantae", "chlorophyta") |>
  galah_filter(profile = "ALA") |> 
  galah_group_by("cl966") |>      # IMCRA bioregions
  atlas_counts()


# spatial layers 
imcra_shp <- st_read(here(
  "projects",
  "plant-conservation-conf",
  "data",
  "imcra_mesoscale_bioregions",
  "imcra4_meso.shp"))

ibra_shp <- st_read(here(
  "projects",
  "plant-conservation-conf",
  "data",
  "IBRA7_regions",
  "ibra7_regions.shp"))

# check distribution of counts to decide on transformation
hist(imcra_counts$count)
hist(ibra_counts$count)


# join spatial and count data
imcra_join <- imcra_counts |> 
  full_join(y = imcra_shp, by = c("cl966" = "MESO_NAME")) |> 
  rename("imcra" = "cl966") |> 
  select(imcra, count, AREA_KM2, geometry) |> 
  mutate(density_log10 = log10(count / AREA_KM2)) |> 
  select(imcra, density_log10, geometry) |> 
  st_as_sf()

ibra_join <- ibra_counts |> 
  full_join(y = ibra_shp, by = c("cl1048" = "REG_NAME_7")) |> 
  rename("ibra" = "cl1048") |> 
  select(ibra, count, SQ_KM, geometry) |> 
  mutate(density_log10 = log10(count / SQ_KM)) |> 
  select(ibra, density_log10, geometry) |> 
  st_as_sf()

# to avoid constantly downloading data and joining
# saveRDS(ibra_join, here("projects", "plant-conservation-conf", "data", "ibra_density.rds"))
# saveRDS(imcra_join, here("projects", "plant-conservation-conf", "data", "imcra_density.rds"))
imcra_join <- readRDS(here("projects", "plant-conservation-conf", "data", "imcra_density.rds"))
ibra_join <- readRDS(here("projects", "plant-conservation-conf", "data", "ibra_density.rds"))


# plot -------

# make it look nice
font_add_google("Lato", "lato")
showtext_auto()

# map
p <- ggplot() + 
  geom_sf(data = imcra_join,
          aes(fill = density_log10),
          colour = NA) +
  scale_fill_distiller(name = "IMCRA",
                       type = "seq",
                       palette = "BuPu",
                       direction = 1,
                       labels = c("0.001", "0.01", "0.1", "1", "10"),
                       guide = guide_colorsteps(direction = "horizontal",
                                                label.position = "bottom",
                                                title.position = "left"))+
  new_scale_fill() +
  geom_sf(data = ibra_join,
          aes(fill = density_log10),
          colour = NA) +
  scale_fill_distiller(name = "IBRA",
                       type = "seq",
                       palette = "YlOrBr",
                       direction = 1,
                       labels = c("0.1", "1", "10", "100"),
                       guide = guide_colorsteps(direction = "horizontal",
                                                label.position = "bottom",
                                                title.position = "left")) +  
  annotate("text", x = 133, y = -45, label = "No. of records per square km", size = 8) +
  coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
  theme_void() +
  theme(text = element_text(family = "lato"),
        title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(12, 'mm'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'))
    

ggsave(here(
  "projects",
  "plant-conservation-conf",
  "plots",
  "choropleth-ibra-imcra_reversed.png"),
  p, height = 10, width = 10, units = "in")




