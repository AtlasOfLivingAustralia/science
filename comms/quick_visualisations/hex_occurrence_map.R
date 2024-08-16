# ---
# title: Make and save a hex map for occurrence records from galah 
# authors: Olivia Torresan, Dax Kellie 
# date: 14 Aug 2024
# ---

# This script makes a hex occurrence map of records from the ALA.  
# Hex maps use coloured hexagons to depict # records versus point based measures.  
# Therefore, they can be less visually "precise" (coloured on # occurrences in each hexagon). 
# They are visually appealing, though, and useful in some contexts (e.g. lots of overlapping points)

# To make a hex map you will need to enter: 

#1.The taxon of interest using the galah package 
#2. A set of binary colours to signal less/more records 
#3. A choice colour for legend label purposes (background is transparent)


#------------ Edit these parameters for a hex occurrence map ---------------------#

file_name <- "lizard-map"

# choose a species or larger taxonomic group here that you want to visualise 
library(galah)

galah_config(email = Sys.getenv("ALA_EMAIL")) 

taxon <- galah_call() |>
  galah_identify("Lacertilia") |> #enter the taxon of interest here --------------#
  galah_apply_profile(ALA) |>
  atlas_occurrences()
taxon

# two colours should be selected from the above ALA colours below (preferably light to dark)

hex_colour_1 <- "#FFEDCF"

hex_colour_2 <- "#C44D34"

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


source(here("R", "hex_map_function.R"))



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

make_hex_map(taxon, hex_colour_1, hex_colour_2, legend_text_colour)

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




