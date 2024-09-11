# remotes::install_github("EvaMaeRey/ggcirclepack")
# remotes::install_version("ggplot2", version = "3.4.4", repos = "http://cran.us.r-project.org")
library(galah)
library(ggplot2)
library(dplyr)
library(glue)
library(tibble)
library(ggcirclepack)
library(ggfittext)
library(patchwork)
library(purrr)
library(stringr)
library(forcats)
library(showtext)

galah_config(email = "dax.kellie@csiro.au")
source(here::here("comms", "2024-09-10_animalia", "class_common-names.R"))

# counts of animals by order
counts <- galah_call() |>
  identify("animalia") |>
  filter(year == 2024) |>
  group_by(order) |>
  atlas_counts()

# counts of animals by class
# but only to extract all taxonomic class names in 2024
class <- galah_call() |>
  identify("animalia") |>
  filter(year == 2024) |>
  group_by(class) |>
  atlas_counts() |>
  pull(class)

# remove a few homonyms (worms) that will be impossible to see in final dataviz anyway
class <- class[!class %in% c("Polychaeta", "Oligochaeta")]

# download all orders by class, appending class name
# We need this so we can match order and class names with observation number
split_counts <- class |>
  map(\(name) {
      result <- galah_call() |>
        identify(name) |>
        filter(year == 2024) |>
        group_by(order) |>
        atlas_counts()
      
      result |>
        mutate(class = paste0(name))
  }
        ) |>
  bind_rows()

taxonomic_names <- split_counts |>
  select(order, class)


# pick the top counts, trying to include lots of major groups
new_counts <- counts |>
  left_join(taxonomic_names, join_by(order)) |>
  mutate(fill = ifelse(count > 8000, class, NA))

# merge with common names tibbles
# (loaded from class_common-names.R)
new_counts <- new_counts |>
  left_join(class_common_names, join_by(fill == class)) |>
  full_join(order_common_names, join_by(order)) |> 
  mutate(
    order_common_name = ifelse(is.na(order_common_name), 
                               class, order_common_name))


# ---------------------------------#
## PLOT

# fonts
font_add_google("Roboto", "roboto")
showtext_auto()

# palette
pal <- MetBrewer::met.brewer("Redon", 8)

# Bubble chart
bubble <- ggplot(data = new_counts, 
       aes(id = order_common_name |> str_wrap(20), 
           area = count, 
           fill = fill
           )) +
  geom_circlepack(color = "grey70", linewidth = 0.2) +
  ggnewscale::new_scale_colour() +
  geom_circlepack(aes(color = after_scale(colorspace::lighten(fill, .5))), linewidth = 0.6) +
  geom_circlepack_text(aes(color = ifelse(!is.na(fill), "white", "grey80"), 
                           family = "roboto",
                           lineheight = 0.8)) +
  scale_fill_manual(values = pal, labels = c(class_common_names$class_common_name, NA), na.value = "grey30") +
  scale_color_identity() +
  coord_fixed() +
  guides(
    size = "none",
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.key.height = unit(1.5, "lines"),
    legend.text = element_text(size = 13, family = "roboto", colour = "white"),
    plot.background = element_rect(fill = "#222322", color = NA),
    plot.title = element_text(face = "bold", size = 18),
    plot.caption = element_text(hjust = 0)
  )


# Bar chart
bar <- new_counts |>
  filter(!is.na(fill)) |>
  ggplot() + 
  geom_bar(aes(x = fct_reorder(order, count), 
               y = count,
               fill = fill,
               colour = fill,
               colour = after_scale(colorspace::lighten(fill, .5))
               ),
           stat = "identity",
           linewidth = 0.8) + 
  geom_bar_text(aes(x = order, y = count,
                    label = glue::glue("{scales::comma(count)}")
                    ),
                min.size = 3, 
                outside = FALSE,
                family = "roboto", colour = "grey80") +
  geom_text(aes(x = order, y = count + 10000,
                    label = glue::glue("{order_common_name}")
                    ),
                hjust = 0,
                family = "roboto", colour = "white") +
  ylim(c(0, 2000000)) + 
  scale_fill_manual(values = pal, labels = c(class_common_names$class_common_name, NA), na.value = "grey98") +
  coord_flip() +
  pilot::theme_pilot(grid = "",
                     axes = "") + 
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#222322", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# combine plots
# NOTE: If you hit an error with patchwork, install ggplot2 using the code at the top of this script
# (Error in Ops.data.frame(guide_loc, panel_loc):)
bubble + bar +
  plot_annotation(
    title = "Observations of Animals in 2024",
    subtitle = paste0("Number of *Animalia* observations recorded in the Atlas of Living Australia") |> 
      str_wrap(130),
    caption = "Data source: The {galah} package \n Dataviz: Dax Kellie \n Code source: Georgios Karamanis · github.com/gkaramanis/tidytuesday"
  ) +
  plot_layout(widths = c(2, 1.7)) &
  theme(
    plot.margin = unit(c(2, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "#101C28", colour = NA),
    plot.title = element_text(face = "bold", size = 26, family = "roboto", colour = "white", hjust = 0.5, margin=margin(12,0, 12, 0)),
    plot.subtitle = ggtext::element_markdown(family = "roboto", colour = "white", size = 14, hjust = 0.5, margin = margin(0,0,-20,0)),
    plot.caption = element_text(hjust = 1, , family = "roboto", color = "grey50")
  )


# save
showtext_opts(dpi = 320)
ggsave(here::here("comms", "2024-09-10_animalia", "plots", "bubbles_animalia.png"),
       height = 11, width = 16, unit = "in", dpi = 320)
