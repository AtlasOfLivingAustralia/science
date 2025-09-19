# ---------------------------------- #
# Title: Treemap of 20250 Animalia observations
# Author: Dax Kellie
# Date: 17/09/2025
# ---------------------------------- #


library(treemap)
library(galah)
library(ggplot2)
library(dplyr)
library(glue)
library(tibble)
# library(ggcirclepack)
library(ggfittext)
library(patchwork)
library(purrr)
library(stringr)
library(forcats)
library(showtext)

galah_config(email = "dax.kellie@csiro.au")
source(here::here("comms", "2025-09-17_biodiversity-month", "class_common-names.R"))

# counts of animals by order
counts <- galah_call() |>
  identify("animalia") |>
  filter(year == 2025) |>
  group_by(order) |>
  atlas_counts()

# counts of animals by class
# but only to extract all taxonomic class names in 2024
class <- galah_call() |>
  identify("animalia") |>
  filter(year == 2025) |>
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
      filter(year == 2025) |>
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
  mutate(fill = ifelse(count > 1, class, NA))

# merge with common names tibbles
# (loaded from class_common-names.R)
new_counts <- new_counts |>
  left_join(class_common_names, join_by(fill == class)) |>
  full_join(order_common_names, join_by(order)) |> 
  mutate(
    order_common_name = ifelse(is.na(order_common_name), 
                               class, order_common_name)
    ) |>
  tidyr::drop_na(fill) # limit number for visibility of text labels


# add colour palette
palette <- tibble(
  class_common_name = c("Fish", "Frogs & Salamanders", "Spiders", "Birds", "Crustaceans", "Slugs & Snails", "Insects", "Mammals", "Reptiles", "Other", NA),
  colour = c('#325d84', '#3D7B6F', '#CC875C', '#B28598', '#962F2F', '#8E4967', '#898a53', '#63463C', '#6F7D60', '#a0c5d5', '#A75E34')
)

cols <- rlang::set_names(palette$colour, palette$class_common_name)

# merge colours
new_counts_col <- new_counts |>
  left_join(palette, join_by(class_common_name == class_common_name))



# make first plot
layout_treemap <- treemap(
  dtf = new_counts_col,
  index = c("fill", "order_common_name"),
  vSize = "count",
  vColor = "colour",
  type = 'color' # {treemap}'s equivalent of scale_fill_identity()
)

# extract info
tm_plot_data <- layout_treemap$tm |> 
  mutate( # calculate end coordinates with height and width
    x1 = x0 + w,
    y1 = y0 + h
    ) |>
  mutate( # get center coordinates for labels
    x = (x0+x1)/2,
    y = (y0+y1)/2
    )

font_add_google("Roboto", "roboto")
showtext_auto()

squares <- ggplot(tm_plot_data, aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  # add fill and borders for groups and subgroups
  geom_rect(aes(fill = vColor),
            # show.legend = TRUE,
            colour = "transparent",
            alpha = .9
            ) +
  geom_rect(colour = "white",
            fill = "transparent"
            ) +
  scale_fill_identity() +
  # add labels
  ggfittext::geom_fit_text(aes(label = order_common_name |> str_wrap(15)), 
                           min.size = 1,
                           family = "roboto",
                           colour = "white",
                           padding.x = grid::unit(2, "mm")) +
  # options
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(
    legend.position = "right"
  )

squares

# Bar chart
bar <- new_counts_col |>
  ggplot() +
  geom_bar(
    aes(
      x = fct_reorder(order, count),
      y = count,
      fill = class_common_name,
      colour = fill,
      colour = after_scale(colorspace::lighten(fill, .2))
    ),
    stat = "identity",
    linewidth = 0.8
  ) +
  ggfittext::geom_bar_text(
    aes(
      x = order, y = count,
      label = glue::glue("{scales::comma(count)}")
    ),
    min.size = 3,
    outside = FALSE,
    family = "roboto", colour = "grey90"
  ) +
  geom_text(
    aes(
      x = order, y = count + 3000,
      label = glue::glue("{order_common_name}")
    ),
    hjust = 0,
    family = "roboto", colour = "#232323"
  ) +
  ylim(c(0, 270000)) +
  scale_fill_manual(values = c(
    "Fish" = "#325d84",
    "Frogs & Salamanders" = "#3D7B6F",
    "Spiders" = "#CC875C",
    "Birds" = "#B28598",
    "Crustaceans" = "#962F2F",
    "Slugs & Snails" = "#8E4967",
    "Insects" = "#898a53",
    "Mammals" = "#63463C",
    "Reptiles" = "#6F7D60",
    "Other" = "#a0c5d5"
    )) +
  scale_colour_manual(values = cols, labels = c(class_common_names$class_common_name, NA), na.value = "#A75E34") +
  # filter bars down for readability, but preserve groups for legend
  scale_x_discrete(
    limits = new_counts_col |> 
      filter(count > 5000) |>
      arrange(desc(-count)) |> # have to reverse arrange to maintain descending order. It's all very confusing but it works
      pull(order)
  ) +
  guides(colour="none",
         fill = guide_legend(title = "")) +
  coord_flip() +
  pilot::theme_pilot(
    grid = "",
    axes = ""
  ) +
  theme(
    legend.position = "left",
    legend.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

bar


## JOIN
squares + bar +
  plot_annotation(
    title = "Observations of Animals in 2025",
    subtitle = glue("Taxonomic breakdown of **{scales::comma(sum(new_counts$count))}** *Animalia* 
                      observations recorded in the Atlas of Living Australia <br> 1 Jan - 19 Sep, 2025") |> 
      str_wrap(130),
    caption = "Data source: The {galah} package \n Dataviz: Dax Kellie \n Code source: June Choe · https://yjunechoe.github.io/posts/2020-06-30-treemap-with-ggplot/  
    Note: Treemap (left) shows taxonomic orders with more than 1 record. Bar plot (right) shows taxonomic orders with more than 5,000 records."
  ) +
  plot_layout(widths = c(2, 1.7)) &
  theme(
    plot.margin = unit(c(2, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "#e3d2b7", colour = NA),
    plot.title = element_text(face = "bold", size = 26, family = "roboto", colour = "#232323", hjust = 0.5, margin=margin(12,0, 12, 0)),
    plot.subtitle = ggtext::element_markdown(family = "roboto", lineheight = 1.2, colour = "#232323", size = 14, hjust = 0.5, margin = margin(0,0,-20,0)),
    plot.caption = element_text(hjust = 1, , family = "roboto", color = "grey30")
  )


# save
showtext_opts(dpi = 320)
ggsave(here::here("comms", "2025-09-17_biodiversity-month", "plots", "squares_animalia.png"),
       height = 10, width = 15, unit = "in", dpi = 320)
