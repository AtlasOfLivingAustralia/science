# ---
# title: Make and save a stacked bar plot for occurrence records from galah 
# authors: Olivia Torresan, Dax Kellie 
# date: 11 Sep 2024
# ---

# This script makes a stacked bar plot for occurrence data from galah. 
# Stacked bar plots use multiple coloured bars to depict proportions of # records for a selected species and/or data provider. 
# To make a stacked bar plot you will need to enter: 

#1. The graph title 
#2. The taxon of interest using the galah package 
#3. A set of binary colours to signal proportions in the bars 
#4. A choice colour for legend label purposes (background is transparent)

#------------ Edit these parameters for a stacked bar plot ---------------------#


file_name <- "bird-stackbar"

# what should the title be?

custom_title <- "<span style='font-size:18pt'><span style='color:bar_colour_1'>**Total**</span> and <span style='color:bar_colour_2'>**yearly**</span> eBird observations in the ALA </span>"

# what taxon are you interested in and how much data do you want to present?

library(galah)

taxon <- galah_call() |>
  galah_filter(dataResourceName == "eBird Australia") |>
  galah_group_by(year) |>
  galah_filter(year <= "2021") |>
  atlas_counts(limit = NULL)

# two colours should be selected from the above ALA colours for the main stacked bar + the smaller 

bar_colour_1 <- "#FFEDCF"

bar_colour_2 <- "#C44D34"

# select a year to highlight the number of records on the bar 

highlight <- 2021

# depending on the background you select for the visualisation (output is transparent) the text colour may need to be adjusted accordingly 

legend_text_colour <- "#212121"


# load packages 

library(here)
library(tidyverse)
library(geomtextpath)
library(scales)
library(pilot)
library(showtext)
library(ggtext)
library(scales)
library(pilot)


source(here("R", "stacked_bar_plot_function.R"))


#make graph 

make_stacked_bar_plot(taxon, bar_colour_1, bar_colour_2, highlight, legend_text_colour)


# save the bar plot


showtext_opts(dpi = 320)

ggsave(file = here::here("comms", 
                         "quick_visualisations", 
                         paste0(file_name, ".svg", sep = "")), 
       dpi = 320,
       bg = "transparent",  
       height = 10, 
       width = 10,
       unit = "in")
