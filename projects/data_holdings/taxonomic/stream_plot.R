## ---------------------------
## Title: Taxonomic stream plot of ALA
## Author: Dax Kellie
## Date Created: 2021-06-07
## ---------------------------

rm(list = ls())

# packages
# remove.packages("galah")
# remotes::install_github("AtlasOfLivingAustralia/galah") # installs latest version
library(galah)
library(tidyverse)
library(data.table)

theme_set(theme_void())

ala_config(email = "dax.kellie@csiro.au")




#------------------------------------------------------#
#                       Get Data
#------------------------------------------------------#

phyla <- ala_counts(group_by = "phylum", limit = 500)
phylum_names <- pull(phyla, phylum)

years <- select_filters(year = seq(2010, 2020))

# counts by year for all phyla
result_list <- phylum_names %>% 
  map(~ala_counts(
    taxa = select_taxa(list(phylum = .x)),
    filters = years,
    group_by = "year"))

# add species names to list
phylum_list <- tibble(
  phylum = phylum_names,
  y = result_list
)

# convert back to df
phylum_df <- phylum_list %>% unnest(y) %>% select(-name)




#------------------------------------------------------#
#                       Wrangle
#------------------------------------------------------#

# add full list of years
year_df <- phylum_names %>%  # create df of all years & spp we want
  map_dfr(., ~{data.frame(year = rep(2010:2020),
                          phylum = .x)})

phylum_df$year <- as.integer(phylum_df$year) # make class the same
phylum_df_years <- year_df %>% left_join(phylum_df) 

# add kingdoms
kingdom_and_phyla <- select_taxa(phylum_names) %>% dplyr::select(kingdom, phylum)
kingdom_and_phyla <- kingdom_and_phyla %>% 
  mutate(kingdom = ifelse(phylum == "Chlorophyta", "Plantae", kingdom)) # add specific kingdom

# merge and remove NAs/duplicates
phylum_joined <-phylum_df_years %>% 
  full_join(kingdom_and_phyla) %>% # merge
  replace_na(list(count = 0, kingdom = "Unidentified", phylum = "Unidentified")) %>% # replace NAs
  filter(!is.na(year)) %>% # a few columns removed for not having any data at the phylum level
  distinct() # delete duplicates






#------------------------------------------------------#
#                       Plot
#------------------------------------------------------#

library(ggstream)
library(viridis)

glimpse(phylum_tidy)

phylum_tidy <- phylum_joined %>%
  mutate(across(where(is.character), as.factor))

# Plotting function
stream_plot_function <- function(kingdom_name){ # automate plotting
phylum_tidy %>% 
  drop_na() %>%
  filter(kingdom == as.character(kingdom_name)) %>%
  group_by(year, phylum, kingdom) %>% 
  dplyr::summarise(total = sum(count)) %>% filter(total > 1000) %>% 
  ggplot(aes(x = year,
             y = total,
             fill = phylum)) + 
  geom_stream(color = "black", 
              extra_span = .1, 
              type = "mirror", bw = .8) + 
  geom_stream_label(aes(label = phylum), 
                    size = 4, type = "mirror", 
                    colour = "black") +
  scale_fill_viridis_d(option = "D") +
  scale_x_continuous(name = "Year", 
                     limits = c(2010, 2020), 
                     breaks = c(2010, 2015, 2020)) + 
  scale_y_continuous(name = "Total observations per year", 
                     labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = "none") 
}



phylum_tidy %>% group_by(kingdom) %>% summarise(count = n()) # list of kingdoms


# Select kingdom and plot
stream_plot_function("Chromista")


