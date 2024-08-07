

# devtools::install_github("EvaMaeRey/ggcirclepack")
library(galah)
library(tidyverse)
library(ggcirclepack)
library(ggfittext)
library(showtext)

galah_config(email = "dax.kellie@csiro.au")


bee_counts <- galah_call() |>
  identify("apidae") |>
  filter(year > 2022) |>
  group_by(species, vernacularName) |>
  atlas_counts()

bee_counts <- bee_counts |>
  arrange(desc(count))

top_bees <- bee_counts |>
  slice_max(n = 10, order_by = count)

bee_counts |> select(species) |> print(n= 23)


font_add_google("Roboto", "roboto")


ggplot(data = bee_counts, 
       aes(id = vernacularName, 
           area = count, 
           fill = species
           )) +
  geom_circlepack(color = "black", linewidth = 0.2) +
  pilot::scale_fill_pilot() +
  scale_color_identity() +
  ggnewscale::new_scale_colour() +
  geom_circlepack_text(
    colour = "grey90",
    check_overlap = TRUE, family = "roboto", lineheight = 0.9) +
  coord_fixed() +
  labs(
    fill = "Bee species"
  ) +
  guides(
    size = "none",
  ) +
  theme_void(base_family = "roboto") +
  theme(
    legend.position = "none",
    # legend.title = element_blank(),
    legend.key.height = unit(1.5, "lines"),
    legend.text = element_text(size = 11),
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(face = "bold", size = 18),
    plot.caption = element_text(hjust = 0)
  )
