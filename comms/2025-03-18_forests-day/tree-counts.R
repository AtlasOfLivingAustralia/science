# ------------------------------------------------------------------ #
# Title: Eucalypts in Australia protected areas (for International Forest Day)
# Author: Dax Kellie
# Date: 18 March 2025
# ------------------------------------------------------------------ #

library(galah)
library(dplyr)
library(here)
library(sf)
library(ggplot2)
library(purrr)

galah_config(email = "dax.kellie@csiro.au")

# search_all(fields, "capad")
# search_all(fields, "cl11033") |>
#   show_values()

# Eucalyptus observations
counts <- galah_call() |>
  identify("eucalyptus") |>
  filter(year > 1999) |>
  apply_profile(ALA) |>
  group_by(cl111033) |>
  atlas_counts()

# Extract protected area names
capad_names <- search_all(fields, "cl111033") |>
  show_values() |>
  pull(cl111033)

# Retrieve species count by protected area and bind rows
get_counts <- function(capad_names){
  
  # get counts
  result <- galah_call() |>
    identify("Eucalyptus") |>
    filter(year > 1999,
           cl111033 == {capad_names}) |>
    apply_profile(ALA) |>
    atlas_species() |>
    count()
  
  # light formatting to catch errors
  tibble(name = {capad_names}, n_species = result |> pull("n"))
}

n_species <- map(capad_names, get_counts) |>
  bind_rows()

# join eucalyptus counts with species number counts
counts_joined <- counts |>
  left_join(n_species, join_by(cl111033 == name))



# Add label, specify Indigenous Protected Areas for plotting later
labels <- counts_joined |>
  arrange(desc(n_species)) |>
  mutate(
    label = case_when(
      n_species > 500 ~ glue::glue("{scales::comma(n_species)} species recorded"),
      .default = NA_character_
    ),
    ipa = case_when(
    cl111033 == "Indigenous Protected Area" ~ "IPA",
    cl111033 == "Aboriginal Area" ~ "IPA",
    .default = "not"
    )
  )

# species count in Indigenous Protected Areas
ipa_species <- labels |>
  filter(ipa == "IPA") |>
  summarise(total = sum(n_species))

# ipa_obs <- labels |>
#   filter(ipa == "IPA") |>
#   summarise(total = sum(count)) |>
#   pull(total)

# handy-dandy position/size filter
counts_joined |>
  arrange(cl111033) |>
  mutate(n_row = row_number()) |>
  filter(cl111033 == "Indigenous Protected Area")

## PLOT

# caption
credits <- 
  glue::glue("
       **Dataviz by Dax Kellie** <br><br>
       Data consists of observations of genus *Eucalyptus* <br><br>
       Protected area types are from the Collaborative Australian Protected Areas Database<br>
       Data downloaded using the galah package <br>
       
       ") |> paste0()

library(showtext)
font_add_google("Roboto", "roboto")
showtext_auto()

ggplot() +
  geom_segment(data = labels,
               aes(x = cl111033,
                   xend = cl111033,
                   y = 0,
                   yend = count),
               colour = "#3e1f0d",
               alpha = 0.8) + 
  geom_point(data = labels,
             aes(x = cl111033,
                 y = count,
                 size = n_species,
                 fill = ipa),
             shape = 21,
             alpha = 0.7,
             # fill = "#707b5b",
             colour = "NA") + 
  scale_fill_manual(
    values = c("#853b25", "#707b5b")
    ) +
  scale_y_log10(limits = c(1, 1e7), labels = scales::comma_format()) +
  scale_x_discrete(expand = c(-1.05, 0)) +
  # geom_point(aes(x = 25,
  #                y = 103338,
  #                size = 520),
  #            shape = 21,
  #            stroke = 1.2,
  #            fill = NA,
  #            colour = "#3A442A",
  #            alpha = 0.7
  #            ) +
  # annotations
  ggrepel::geom_text_repel(aes(label = glue::glue("
                                                  {ipa_species} species
                                                  in all 
                                                  Indigenous
                                                  Protected 
                                                  Areas
                                                  "),
                               x = 25,
                               y = 2162,
                               colour = "#853b25",
                               colour = after_scale(colorspace::darken(colour, .6))),
                           nudge_x = c(-5.6),
                           nudge_y = c(1.3),
                           segment.size = 0,
                           segment.curvature = c(0.001),
                           family = "roboto",
                           lineheight = 0.9
                           ) +
  annotate("text",
           x = 25, y = 4000000, label = "526 species recorded in National Parks",
           colour = "#3A442A",
           family = "roboto") +
  # legend
  geom_point(aes(x = 52,
                 y = 5000000,
                 size = 10),
             colour = "#707b5b",
             alpha = 0.7) +
  annotate("text",
           x = 55, y = 5000000, label = "-- 10 species",
           colour = "#3A442A",
           family = "roboto") +
  geom_point(aes(x = 52,
                 y = 700000,
                 size = 100),
             colour = "#707b5b",
             alpha = 0.7) +
  annotate("text",
           x = 56, y = 700000, label = "-- 100 species",
           colour = "#3A442A",
           family = "roboto") +
  scale_size(range = c(1, 40)) + # scale points - IMPORTANT
  labs(y = "Number of Observations",
       x = "Protected Area",
       title = "Eucalypts in Australia's protected areas",
       subtitle = "Observations and unique species of *Eucalyptus* in different types of protected areas (CAPAD) since 2000",
       caption = credits) +
  pilot::theme_pilot(grid = "h",
                     axes = "b") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 15),
    plot.subtitle = ggtext::element_markdown(size = 16, margin = unit(c(0, 0, 1, 0), "cm")),
    text = element_text(family = "roboto"),
    plot.title = element_text(size = 20),
    plot.margin = unit(c(1,1,1,1),"cm"),
    # panel.background = element_rect(fill = "#EB9D07", color = NA),
    plot.background = element_rect(fill = "#EEEBEA", colour = NA),
    plot.caption = ggtext::element_textbox_simple(
      colour = "grey10",
      size = 8,
      halign = 1,
      lineheight = 0.4,
      margin = margin(t = 0.2,
                      r = -0.25, 
                      unit = "cm")
    )
    )

# save
showtext_opts(dpi = 320)
ggsave(here::here("comms", "2025-03-18_forests-day", "plots", "eucalyptus-trees.png"),
       height = 11, width = 14, unit = "in", dpi = 320)


