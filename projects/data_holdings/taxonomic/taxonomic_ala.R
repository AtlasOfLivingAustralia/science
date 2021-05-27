## ------------------------------------------------------#
## Title: Taxonomic ALA Investigation
## Author: Dax Kellie
## Date Created: 2021-05-18
## ------------------------------------------------------#

rm(list = ls())

# Code to draw a sunburst plot

# packages
library(galah)
library(data.table)
library(ggplot2)
library(viridisLite)
library(ggrepel)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)




#---------------------------------------------------------------#
#           Find record counts from ALA using Galah
#---------------------------------------------------------------#

# import taxonomic data

# Get counts down to family level
families <- ala_counts(group_by = "family", limit = 6416) # use counts to find all families
family_names <- dplyr::pull(families, family) # extract list of all family names
# Note: 
#We have to use ala_counts() (not select_taxa()) because ala_counts() retains all levels of taxonomy

#___________________________________________________________________________________________#
# Find counts

# counts_ala <- select_taxa(term = family_names, counts = TRUE) # takes ~10 mins to run
# saveRDS(counts_ala, file = "df_familycounts_ala.rds")
#___________________________________________________________________________________________#

# Call saved data file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to current directory
counts_ala <- readRDS(file = "data/df_familycounts_ala.rds") # load family counts

# Data wrangling
dt_counts_ala <- lazy_dt(counts_ala) # convert to lazy data.table for dtplyr
dt_counts_ala <- dt_counts_ala %>% 
  select(kingdom, phylum, class, order, family, count) %>% # select cols we want
  arrange(kingdom, phylum, class, order, family, count) %>% # sort alphabetically/numerically
  as.data.table() 

counts_ala_df <- as.data.frame(dt_counts_ala)[apply(dt_counts_ala[, 1:5], 1, function(a){!any(is.na(a))}), ]
counts_ala_df$ymax <- cumsum(counts_ala_df$count)
counts_ala_df$ymin <- c(0, counts_ala_df$ymax[seq_len(nrow(counts_ala_df) - 1)])

count_df <- counts_ala_df





#---------------------------------------------------------------#
#           Tidyverse way to make sunburst plot
#---------------------------------------------------------------#

## Kingdom

# get data
data_kingdom <- count_df %>%
  group_by( kingdom ) %>%
  summarise( tot_count = sum( count ) ) %>%
  ungroup() %>%
  mutate(
    ymax = cumsum( tot_count ),
    ymin = lag( ymax, n = 1, default = 0 )
  )

# label position
data_kingdom <- data_kingdom %>% mutate(running=cumsum(tot_count), pos = running - tot_count/2)

# plot
kingdom_Circle <- ggplot( data_kingdom ) +
  geom_rect(
    aes( xmin = 2, xmax = 3, ymin = ymin, ymax = ymax, fill = kingdom ),
    color = "white"
  ) +
  scale_fill_viridis_d(option = "plasma", alpha = 1, begin = 0.7) +
  xlim( 0, 7 ) + theme_void()

kingdom_Circle + coord_polar( theta = "y" ) # test colour


## Phylum

# get data and merge with kingdom data
data_phylum <- count_df %>%
  group_by( kingdom, phylum ) %>%
  summarise( tot = sum( count ) ) %>%
  left_join( data_kingdom %>% select( kingdom, tot_count) ) %>%
  ungroup() %>%
  mutate(
    ymax = cumsum(tot),
    ymin = lag( ymax, n = 1, default = 0 )
  ) %>%
  filter( !is.na( phylum ) )

phylum_Circle <-
  geom_rect(
    data = data_phylum,
    aes( xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, fill = kingdom), 
    color = "white"
  )


## Class

# get data and merge with phylum data
data_class <- count_df %>%
  group_by( kingdom, phylum, class ) %>%
  summarise( tottot = sum( count ) ) %>%
  left_join( data_phylum %>% select( kingdom, phylum, tot_count) ) %>%
  ungroup() %>%
  mutate(
    ymax = cumsum(tottot),
    ymin = lag( ymax, n = 1, default = 0 )
  ) %>%
  filter( !is.na( class ) )

# outer label position
data_class$labelPosition <- (data_class$ymax + data_class$ymin) / 2

class_Circle <-
  geom_rect(
    data = data_class,
    aes( xmin = 4, xmax = 5, ymin = ymin, ymax = ymax, fill = kingdom),
    color = "white"
  )

## Order

# get data and merge with phylum data
data_order <- count_df %>%
  group_by( kingdom, phylum, class, order ) %>%
  summarise( tot_order = sum( count ) ) %>%
  left_join( data_class %>% select( kingdom, phylum, class, tot_count) ) %>%
  ungroup() %>%
  mutate(
    ymax = cumsum(tot_order),
    ymin = lag( ymax, n = 1, default = 0 )
  ) %>%
  filter( !is.na( class ) )

# outer label position
data_order$labelPosition <- (data_order$ymax + data_order$ymin) / 2

order_Circle <-
  geom_rect(
    data = data_order,
    aes( xmin = 5, xmax = 6, ymin = ymin, ymax = ymax, fill = kingdom),
    color = "white"
  )


## Full plot

p <- kingdom_Circle + phylum_Circle + class_Circle + order_Circle +
  coord_polar( theta = "y" ) + 
  theme(legend.position = "none") + 
  ggtitle("Taxonomic breakdown of observations in the ALA", 
          subtitle = "Level: Order") +
  geom_text_repel(data = data_kingdom, # kingdom labels
                  aes(label=paste(kingdom), x=2.5, y=pos), 
                  size = 3, 
                  color = "white",
                  bg.color = "grey30", # shadow color
                  bg.r = 0.14, # shadow radius
                  max.overlaps = Inf) + 
  geom_text_repel(data = data_order %>% filter(tot_order > 500000), # family labels
                  mapping = aes(x = 7, y = labelPosition, label = paste(order)),
                  size = 2.3,
                  color = "black",
                  hjust = 0.5,
                  max.overlaps = 2,
                  show.legend = FALSE)

ggsave("taxonomic_ala_order.png", p,
       width = 25, height = 25, units = "cm")

#---------------------------------#
#           Useful Links
#---------------------------------#

# code to make tidyverse sunburst plot
# https://www.sdinnovation.com.au/blog/create-sunburst-diagram-in-r-with-ggplot-and-dplyr
# https://medium.com/optima-blog/create-basic-sunburst-graphs-with-ggplot2-7d7484d92c61

# Possibly some solution to the labelling problems
# https://stackoverflow.com/questions/60642476/piedonut-does-not-display-some-pie-labels

# ggrepel examples
# https://ggrepel.slowkow.com/articles/examples.html
