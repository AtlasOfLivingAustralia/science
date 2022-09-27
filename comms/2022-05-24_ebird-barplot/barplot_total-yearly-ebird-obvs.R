#---------------------------------------------------#
# Title: Total + yearly eBird records bar plot
# Author: Dax Kellie
# Date: 25 May, 2022
#---------------------------------------------------#

library(galah)
library(tidyverse)
library(geomtextpath)
library(scales)
library(pilot)
library(showtext)
library(ggtext)


# download counts of eBird records
birds <- galah_call() |>
  galah_filter(dataResourceName == "eBird Australia") |>
  galah_group_by(year) |>
  atlas_counts(limit = NULL)

# add cumulative sum column
birds <- birds %>%
  arrange(-desc(year)) %>%
  mutate(
    total_count = cumsum(count)
  ) %>%
  arrange(desc(year))

# double check total number on ALA
galah_call() |>
  galah_filter(dataResourceName == "eBird Australia") |>
  atlas_counts()



#### PLOT ####

bar_plot <- birds %>%
  filter(year>= 2010) %>%
  ggplot() +
  geom_bar(aes(x = year, y = total_count),
           stat = "identity",
           fill = pilot::pilot_color("navy"),
           size = 1,
           width = 0.98) +
  geom_bar(aes(x = year, y = count),
           stat = "identity",
           fill = pilot::pilot_color("orange"),
           size = 0.5,
           width = 0.98) +
  pilot::geom_text_pilot(data = birds %>% filter(year == 2021),
                         mapping = aes(x = year,
                                       y = count,
                                       label = number(count,
                                                      accuracy = 0.1,
                                                      scale_cut = cut_short_scale())),
                         hjust = "center",
                         nudge_y = -900000,
                         size = 5) +
  pilot::geom_text_pilot(data = birds %>% filter(year == 2021),
                         mapping = aes(x = year,
                                       y = total_count,
                                       label = number(total_count,
                                                      accuracy = 0.1,
                                                      scale_cut = cut_short_scale())),
                         hjust = "center",
                         nudge_y = -900000,
                         size = 5) +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = c(0,0)) +
  scale_x_discrete(breaks = c(2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021),
                   expand = c(0,0)) +
  labs(
    title = "<span style='font-size:21pt'>
    <span style='color:#204466;font-weight:bold;'>Total</span> and
    <span style='color:#f28100;font-weight:bold;'>yearly</span> eBird observations in the ALA
    </span>",
    x = "Year", y = "Number of Observations") +
  pilot::theme_pilot(grid = "h",
                     axes = "b") +
  theme(axis.line.x.bottom = element_line(size = 1.2),
        text = element_text(family = "roboto"),
        # title = element_text(size = 30),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -1.1),
        plot.title = element_markdown(lineheight = 1.1),
        legend.text = element_markdown(size = 11),)

bar_plot


# add silhouette

library(rphylopic)
# Add kangaroo silhouette in corner with {rphylopic} package
bird1 <- name_search(text = "Hirundo", options = "namebankID")[[1]] # find names
name_images(uuid = bird1$uid[1])  # list images

bird1_id <- name_images(uuid = bird1$uid[1])$same  # get individual image id
bird1_pic <- image_data(bird1_id, size = 256)[[1]] # get actual icon, define size. Don't run this alone

# add image to plot
bar_plot_phylo <- bar_plot +
  add_phylopic(bird1_pic, alpha = 1, x = 3, y = 25e6, ysize = 1e7)

bar_plot_phylo


# save

ggsave(bar_plot_phylo, file = here::here("comms", "2022-05-24_ebird-barplot", "bar-plot.png"),
       height = 7, width = 10)


# end

