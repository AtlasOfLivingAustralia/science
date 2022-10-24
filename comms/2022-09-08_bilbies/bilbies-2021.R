#-------------------------------#
# Title: Bilby density map
# Author: Dax Kellie
# Date: 8 Sep 2022
#-------------------------------#

# This code was a test recreate the style of Cedric Scherer's "Squirrel 
# Sightings in Central Park" map. 
# Code: https://github.com/z3tt/TidyTuesday/blob/main/R/2019_44_Squirrels.Rmd
# Dataviz: https://github.com/Z3tt/TidyTuesday/tree/main/plots/2019_44


#------------------------------------------------------------------------------#


# packages

library(galah)
library(tidyverse)
library(sf)
library(ozmaps)
library(ggpointdensity)
library(showtext)
library(here)
library(rmapshaper)


# Get counts

galah_call() |>
  galah_identify("Thylacomyidae") |>
  galah_filter(year > 2020,
               profile = "ALA") |>
  galah_group_by(cl1048) |>
  # galah_group_by(cl1049) |>
  atlas_counts()


# Download bilbies data

galah_config(email = "dax.kellie@csiro.au")

bilbies <- galah_call() |>
  galah_identify("Thylacomyidae") |>
  galah_filter(year > 1999,
               profile = "ALA") |>
  atlas_occurrences()


# Get maps

## NSW
aus_wgs84 <- ozmap_data(data = "states") |>
  st_transform(crs = st_crs("WGS84"))

nsw <- aus_wgs84 |>
  filter(NAME == "New South Wales")


## Roads

### Code to unzip folders

# choose the folder to unzip
zipF <- here("comms",
             "2022-09-08_bilbies",
             "data",
             "64570_sh55-12_shp.zip")

# define the folder where the zip file should be unzipped to
outDir <- here("comms",
               "2022-09-08_bilbies",
               "data")

unzip(zipF,exdir=outDir)  # unzip your file


### Find the necessary shape file
roads_shp <- st_read(here("comms",
                          "2022-09-08_bilbies",
                          "data",
                          "GEODATA_TOPO250K_TILE_DATA",
                          "H5512",
                          "Shapefiles",
                          "Transport",
                          "h5512_roads.shp"),
                     quiet = TRUE) |>
  rmapshaper::ms_simplify(keep=0.1)


# ---

# Map

base_size <- 12
half_line <- base_size/2

ggplot(bilbies) +
  geom_sf(data = aus_wgs84, color = "grey70") +
  geom_sf(data = roads_shp, color = "grey65", size = 1.5)+
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), size = 9, color = "grey55") +
  ggpointdensity::geom_pointdensity(aes(decimalLongitude, decimalLatitude),
                                    adjust = 0.001,
                                    # transform = scales::log10_trans(),
                                    size = 7) +
  rcartocolor::scale_color_carto_c(palette = "Sunset",
                                   direction = -1,
                                   # breaks = c(4, 44.5, 86),
                                   # labels = c("low", "**<span style='font-family:Cambria'>←  </span> Clustering of Sightings <span style='font-family:Cambria'>  →</span>**", "high"),
                                   name = "Bilby Observations",
                                   guide = guide_colorbar(direction = "horizontal",
                                                          barheight = unit(3, units = "mm"),
                                                          barwidth = unit(100, units = "mm"),
                                                          draw.ulim = FALSE,
                                                          ticks.colour = "transparent",
                                                          title.position = 'top',
                                                          title.hjust = 0.5,
                                                          label.hjust = 0.5)) +
  coord_sf(xlim = c(149.2, 149.28), ylim = c(-30.6, -30.52)) +
  
  # All of these theme options are from Cedric Scherer's theme template
  # https://github.com/z3tt/TidyTuesday/blob/main/R/tidy_grey.R
  theme(line = element_line(colour = "grey85", size = 0.4, linetype = 1, lineend = "butt"), 
        rect = element_rect(fill = "grey20", colour = "grey85", size = 0.4, linetype = 1), 
        text = element_text(face = "plain", colour = "white", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),  debug = FALSE),
        axis.line = element_blank(), 
        axis.line.x = NULL, 
        axis.line.y = NULL, 
        axis.text = element_text(size = base_size * 1.1, colour = "grey85"), 
        axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1), 
        axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line/2), vjust = 0), 
        axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1), 
        axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line/2), hjust = 0), 
        axis.ticks = element_line(colour = "grey85", size = 0.3), 
        axis.ticks.length = unit(half_line/2, "pt"), 
        axis.ticks.length.x = NULL, 
        axis.ticks.length.x.top = NULL, 
        axis.ticks.length.x.bottom = NULL, 
        axis.ticks.length.y = NULL, 
        axis.ticks.length.y.left = NULL, 
        axis.ticks.length.y.right = NULL, 
        axis.title.x = element_text(margin = unit(c(3.5, 0, 0, 0), "mm"), vjust = 1, size = base_size * 1.3, face = "bold"), 
        axis.title.x.top = element_text(margin = margin(b = half_line), vjust = 0), 
        axis.title.y = element_text(angle = 90, margin = unit(c(0, 3.5, 0, 0), "mm"), vjust = 1, size = base_size * 1.3, face = "bold"), 
        axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line), vjust = 0), 
        legend.background = element_rect(colour = NA), 
        legend.spacing = unit(0.4, "cm"), 
        legend.spacing.x = NULL, 
        legend.spacing.y = NULL, 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"), 
        legend.key = element_rect(fill = "grey20", colour = "grey20"), 
        legend.key.size = unit(1.2, "lines"), 
        legend.key.height = NULL, 
        legend.key.width = NULL, 
        legend.text = element_text(size = rel(0.9)), 
        legend.text.align = NULL, 
        legend.title = element_text(hjust = 0, size = rel(1)), 
        legend.title.align = NULL, 
        legend.position = "bottom", 
        legend.direction = NULL, 
        legend.justification = "center", 
        legend.box = NULL, 
        legend.box.margin = margin(0, 0, 0, 0, "cm"), 
        legend.box.background = element_blank(), 
        legend.box.spacing = unit(0.4, "cm"), 
        panel.background = element_rect(fill = NA, colour = NA), 
        panel.border = element_rect(colour = "grey85", fill = NA, size = rel(1)),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "transparent"), 
        panel.grid.minor = element_line(colour = "transparent"), 
        panel.spacing = unit(base_size/2, "pt"), 
        panel.spacing.x = NULL, 
        panel.spacing.y = NULL, 
        panel.ontop = FALSE, 
        strip.background = element_rect(fill = "grey20", colour = "grey85"), 
        strip.text = element_text(colour = "white", size = base_size * 1.1, face = "bold"), 
        strip.text.x = element_text(margin = margin(t = half_line, b = half_line)), 
        strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)), 
        strip.placement = "inside", 
        strip.placement.x = NULL, 
        strip.placement.y = NULL, 
        strip.switch.pad.grid = unit(0.1, "cm"), 
        strip.switch.pad.wrap = unit(0.1, "cm"), 
        plot.background = element_rect(colour = NA), 
        plot.title = element_text(size = base_size * 1.8, hjust = 0, vjust = 1, face = "bold", margin = margin(b = half_line * 1.2)), 
        plot.subtitle = element_text(size = base_size, hjust = 0, vjust = 1, margin = margin(b = half_line * 0.9)), 
        plot.caption = element_text(size = rel(0.9), hjust = 1, vjust = 1, margin = margin(t = half_line * 0.9), color = "white"), 
        plot.margin = margin(base_size, base_size, base_size, base_size), complete = T,
        plot.tag = element_text(size = rel(1.5), face = "bold", hjust = 0.5, vjust = 0.5), 
        plot.tag.position = "topleft")
