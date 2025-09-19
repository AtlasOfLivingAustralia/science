# ---------------------------------- #
# Title: Treemap of 2025 Plantae observations
# Author: Dax Kellie
# Date: 18/09/2025
# ---------------------------------- #

# remotes::install_version("ggplot2", version = "3.4.4", repos = "http://cran.us.r-project.org")
library(treemap)
library(galah)
library(ggplot2)
library(dplyr)
library(glue)
library(tibble)
library(ggfittext)
library(patchwork)
library(purrr)
library(stringr)
library(forcats)
library(showtext)

galah_config(email = "dax.kellie@csiro.au")
source(here::here("comms", "2024-09-25_plantae", "plant_common-names.R"))
# edit group common name
groups_clean <- groups_clean |>
  mutate(
    common_name = case_when(
      common_name == "Mosses, Hornworts, Liverworts (Non-Vascular Plants)" ~ "Mosses, Hornworts, Liverworts",
      .default = common_name
    )
  )

# counts of plants by family
counts <- galah_call() |>
  identify("plantae") |>
  filter(year == 2025) |>
  group_by("family") |>
  atlas_counts()

# counts of plants by class
# but only to extract all taxonomic class names in 2024
class <- galah_call() |>
  identify("plantae") |>
  filter(year == 2025) |>
  group_by("class") |>
  atlas_counts() |>
  dplyr::pull(class)


group_names <- groups_clean |>
  filter(!common_name %in% c("Dicots", "Monocots")) |>
  select(includes) |>
  drop_na() |>
  pull()


# remove unmatched groups that will be impossible to see in final dataviz anyway
group_names <- group_names[!group_names %in% c("Zygnematophyceae")]
# search_taxa("Zygnematophyceae")

# download all orders by taxon, appending family name
# We need this so we can match family and taxon names with observation number
split_counts <- group_names |>
  map(\(name) {
    result <- galah_call() |>
      identify(name) |>
      filter(year == 2025) |>
      group_by(family) |>
      atlas_counts()
    
    result |>
      mutate(taxon_name = paste0(name))
  }
  ) |>
  bind_rows()

taxonomic_names <- split_counts |>
  select(family, taxon_name)


# pick the top counts, trying to include lots of major groups
new_counts <- counts |>
  left_join(taxonomic_names, join_by(family)) |>
  filter(!duplicated(family)) |> # remove any duplicate family counts after loop
  filter(count > 1) # remove groups with small numbers of records


# merge with common names tibbles
# (loaded from plantae_common-names.R)
new_counts <- new_counts |>
  left_join(groups_clean, join_by(taxon_name == includes)) |>
  filter(common_name != "Dicots") |> # remove stray duplicate that shouldn't be there
  full_join(family_common_names, join_by(family)) |>
  mutate(
    family_common_name = ifelse(is.na(family_common_name),
                                taxon_name, family_common_name),
    common_name = ifelse(is.na(common_name),
                         "Other", common_name)
  )

# new_counts |>
#   filter(is.na(family_common_name)) |>
#   pull(family)

#866B52

# add colour palette
palette <- tibble(
  common_name = c("Flowering Plants", "Ferns", "Conifers, Pines, Kauri, Cedar", "Mosses, Hornworts, Liverworts", "Cycads", "Club-mosses", "Algae", "Other", NA),
  colour = c('#45462C', '#A0B28D', '#7099A2', '#B16260', '#496D94', '#823F5F', '#706327', '#42353F', '#A78072')
)

cols <- rlang::set_names(palette$colour, palette$common_name)

# merge colours
new_counts_col <- new_counts |>
  left_join(palette, join_by(common_name == common_name))


# make first plot
layout_treemap <- treemap(
  dtf = new_counts_col,
  index = c("common_name", "family_common_name"),
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
# mutate(primary_group = ifelse(is.na(order_common_name), 1.2, .5)) |> # mark primary groupings and set boundary thickness
# mutate(color = ifelse(is.na(class_common_name), NA, color)) # remove colors from primary groupings (since secondary is already colored)


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
  ggfittext::geom_fit_text(aes(label = family_common_name |> str_wrap(15)), 
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
  # filter(!is.na(common_name),
  #        count > 1000) |>
  # mutate(
  #   count = case_when(
  #     count <= 1000 ~ 0,
  #     .default = count
  #   )
  # ) |>
  ggplot() +
  geom_bar(
    aes(
      x = fct_reorder(family, count),
      y = count,
      fill = common_name,
      colour = common_name,
      colour = after_scale(colorspace::lighten(fill, .2))
    ),
    stat = "identity",
    linewidth = 0.8
  ) +
  ggfittext::geom_bar_text(
    aes(
      x = family, y = count,
      label = glue::glue("{scales::comma(count)}")
    ),
    min.size = 3,
    outside = FALSE,
    family = "roboto", colour = "grey90"
  ) +
  geom_text(
    aes(
      x = family, y = count + 3000,
      label = glue::glue("{family_common_name}")
    ),
    hjust = 0,
    family = "roboto", colour = "#232323",
    size = 3.5
  ) +
  ylim(c(0, 100000)) +
  scale_fill_manual(values = c(
    "Flowering Plants" = "#45462C",
    "Ferns" = "#A0B28D",
    "Conifers, Pines, Kauri, Cedar" = "#7099A2",
    "Mosses, Hornworts, Liverworts" = "#B16260",
    "Cycads" = "#496D94",
    "Club-mosses" = "#823F5F",
    "Algae" = "#706327",
    "Other" = "#A78072"
  )) +
  scale_colour_manual(values = cols, labels = c(new_counts_col$common_name, NA), na.value = "#63463C") +
  scale_x_discrete(
    limits = new_counts_col |> 
      filter(count > 3500) |>
      arrange(desc(-count)) |> # have to reverse arrange to maintain descending order. It's all very confusing but it works
      pull(family)
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




# combine plots
# NOTE: If you hit an error with patchwork, install ggplot2 using the code at the top of this script
# (Error in Ops.data.frame(guide_loc, panel_loc):)
squares + bar +
  plot_annotation(
    title = "Observations of Plants in 2025",
    subtitle = glue("Taxonomic breakdown of **{scales::comma(sum(new_counts$count))}** *Plantae* 
                      observations recorded in the Atlas of Living Australia <br> 1 Jan - 19 Sep, 2025") |> 
      str_wrap(130),
    caption = "Data source: The {galah} package \n Dataviz: Dax Kellie \n Code source: June Choe · https://yjunechoe.github.io/posts/2020-06-30-treemap-with-ggplot/  
    Note: Treemap (left) shows taxonomic families with more than 1 record. Bar plot (right) shows taxonomic families with more than 3,500 records."
  ) +
  plot_layout(widths = c(2, 1.5)) &
  theme(
    plot.margin = unit(c(t = 2, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "#f4f4de", colour = NA),
    plot.title = element_text(face = "bold", size = 26, family = "roboto", colour = "#232323", hjust = 0.5, margin=margin(12,0, 12, 0)),
    plot.subtitle = ggtext::element_markdown(family = "roboto", lineheight = 1.2, colour = "#232323", size = 14, hjust = 0.5, margin = margin(0,0,-20,0)),
    plot.caption = element_text(hjust = 1, , family = "roboto", color = "grey30")
  )


# save
showtext_opts(dpi = 320)
ggsave(here::here("comms", "2025-09-17_biodiversity-month", "plots", "squares_plantae.png"),
       height = 10, width = 15, unit = "in", dpi = 320)

