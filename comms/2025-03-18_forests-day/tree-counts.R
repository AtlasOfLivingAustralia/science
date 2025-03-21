# ------------------------------------------------------------------ #
# Title: Eucalypts in Australia's bioregions (for International Forest Day)
# Author: Dax Kellie
# Date: 21 March 2025
# ------------------------------------------------------------------ #

library(galah)
library(dplyr)
library(here)
library(sf)
library(ggplot2)
library(purrr)
library(tidyr)

galah_config(email = "dax.kellie@csiro.au")

# search_all(fields, "ibra")

# Eucalyptus observations
counts <- galah_call() |>
  identify("eucalyptus") |>
  filter(year > 1999) |>
  apply_profile(ALA) |>
  group_by(cl1048) |>
  atlas_counts()

# Extract protected area names
ibra_names <- search_all(fields, "cl1048") |>
  show_values() |>
  pull(cl1048)

# Retrieve species count by protected area and bind rows
get_counts <- function(ibra_names){
  
  # get counts
  result <- galah_call() |>
    identify("Eucalyptus") |>
    filter(year > 1999,
           cl1048 == {ibra_names}) |>
    apply_profile(ALA) |>
    atlas_species() |>
    count()
  
  # light formatting to catch errors
  tibble(name = {ibra_names}, n_species = result |> pull("n"))
}

n_species <- map(ibra_names, get_counts) |>
  bind_rows()

# join eucalyptus counts with species number counts
counts_joined <- counts |>
  left_join(n_species, join_by(cl1048 == name))



# Add label, specify Indigenous Protected Areas for plotting later
counts_extra <- counts_joined |>
  arrange(desc(n_species)) |>
  mutate(
    n_row = row_number(),
    top_species = case_when(
      n_species > 160 ~ as.character(n_row),
      .default = NA_character_
    ),
    state = case_when(
      cl1048 == "South Eastern Highlands" ~ "NSW/VIC",
      cl1048 == "Sydney Basin" ~ "NSW",
      cl1048 == "Mallee" ~ "WA",
      .default = NA_character_
    )
  )

counts_extra

## PLOT

# caption
credits <- 
  glue::glue("
       **Dataviz by Dax Kellie** <br><br>
       Data consists of observations of genus *Eucalyptus* <br><br>
       *Interim Biogeographic Regionalisation for Australia (IBRA) terrestrial bioregions<br>
       Data downloaded using the galah package <br>
       
       ") |> paste0()


library(showtext)
font_add_google("Roboto", "roboto")
showtext_auto()

ggplot() +
  geom_segment(data = counts_extra,
               aes(x = cl1048,
                   xend = cl1048,
                   y = 0,
                   yend = count),
               colour = "#3e1f0d",
               alpha = 0.8) + 
  geom_point(data = counts_extra,
             aes(x = cl1048,
                 y = count,
                 size = n_species,
                 # fill = ipa
                 ),
             fill = "#707b5b",
             shape = 21,
             alpha = 0.7,
             # fill = "#707b5b",
             colour = "NA") + 
  geom_point(data = counts_extra |> drop_na(top_species),
             aes(x = cl1048,
                 y = count,
                 size = n_species,
                 colour = cl1048),
             shape = 21,
             fill = "transparent",
             colour = "#4f4f4f",
             stroke = 1
             ) +
  scale_y_log10(limits = c(1, 1e7), labels = scales::comma_format()) +
  scale_x_discrete(expand = c(-1.05, 0)) +
  ggrepel::geom_text_repel(
    data = counts_extra |> drop_na(top_species),
    aes(
      x = cl1048,
      y = count,
      label =
        glue::glue("
                 #{top_species} most species
                 {cl1048}, {state} | {n_species} sp.
                 ")
    ),
    colour = "#4f4f4f",
    nudge_x = c(-1.6),
    nudge_y = c(1.3),
    segment.size = 0.1,
    segment.curvature = c(0.001),
    family = "roboto",
    lineheight = 0.9
  ) +
  # legend
  geom_point(aes(x = 2,
                 y = 6000000,
                 size = 10),
             colour = "#707b5b",
             alpha = 0.7) +
  annotate("text",
           x = 8, y = 6000000, label = "-- 10 species",
           colour = "#3A442A",
           family = "roboto") +
  geom_point(aes(x = 2,
                 y = 1000000,
                 size = 100),
             colour = "#707b5b",
             alpha = 0.7) +
  annotate("text",
           x = 8, y = 1000000, label = "-- 100 species",
           colour = "#3A442A",
           family = "roboto") +
  scale_size(range = c(1, 32)) + # scale points - IMPORTANT
  labs(y = "Number of Observations",
       x = "IBRA* Bioregion",
       title = "Eucalypts in Australia's bioregions",
       subtitle = "Observations and unique species of *Eucalyptus* in IBRA Bioregions since 2000",
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
       height = 11, width = 15.3, unit = "in", dpi = 320)


