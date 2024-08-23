# ---
# title: Make and save a point density map for occurrence records from galah 
# authors: Olivia Torresan, Dax Kellie 
# date: 23 Aug 2024
# ---

# This script makes a point density occurrence map of records from the ALA.  
# These are point based maps, though, highly overlapped points are highlighted in a different colour.
# You might use a point density map in cases where geographic precision of overlapping densities is important.
# Hex maps also work well but they organise density measurements into predetermined regions; so they aren't as precise. 

# To make a point density map you will need to enter: 

#1. The taxon of interest using the galah package 
#2. A colour palette to signal less/more overlapping points 
#3. A choice colour for legend label purposes (background is transparent)



#------------ Edit these parameters for a hex occurrence map ---------------------#

file_name <- "emu-map"

# choose a species or larger taxonomic group here that you want to visualise 
library(galah)

galah_config(email = Sys.getenv("ALA_EMAIL")) 

taxon <- galah_call() |>
  galah_identify("Dromaius novaehollandiae") |> #enter the taxon of interest here --------------#
  galah_apply_profile(ALA) |>
  atlas_occurrences()
taxon

# two colours should be selected from the above ALA colours below (preferably light to dark)

point_colour_1 <- "#C3EDEF"

point_colour_2 <- "#076164"

# depending on the background you select for the visualisation (output is transparent) the text colour may need to be adjusted accordingly 

legend_text_colour <- "#212121"

# load packages 

library(ggplot2)
library(here)
library(tidyr)
library(dplyr)
library(ozmaps)
library(sf)
library(hexbin)
library(showtext)
library(svglite)
library(ggpointdensity)


source(here("R", "point_density_map_function.R"))



#----------- These are the colour options for ALA communications content ----------------#

# ALA primary colours
c("#F26649", "#C44D34", "#637073")

# ALA secondary colours 
c("#F2F2F2", "#9D9D9D", "#212121")

# ALA extended colours
c("#FFC557", "#B7CD96", "#68DAD5", "#003A70", "#A191B2", "#691C32")

# ALA expanded colours 
c("#EB9D07", "#076164", "#1B5D81", "#5B397D", "#FFEDCF", "#38613D", "#C3EDEF", "#921D11")



#----------- Run the code below to produce a hex map with the above parameters ----------------#


# load font 
font_add_google("Roboto")
showtext_auto(enable = TRUE) 


#make graph 

make_pdensity_map(taxon, point_colour_1, point_colour_2, legend_text_colour)

# a .svg will be saved in the science comms folder in ./comms/quick_visualisations/hex-map.svg

showtext_opts(dpi = 320)

ggsave(file = here::here("comms", 
                         "quick_visualisations", 
                         paste0(file_name, ".svg", sep = "")), 
       dpi = 320,
       bg = "transparent",  
       height = 10, 
       width = 10,
       unit = "in")

