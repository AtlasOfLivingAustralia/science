# ---------------------------------- #
# Title: Bubble chart of 2024 Plantae observations
# Author: Dax Kellie
# Date: 25/09/2024
# ---------------------------------- #

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
source(here::here("comms", "2024-09-25_plantae", "plant_common-names.R"))

# counts of plants by family
counts <- galah_call() |>
  identify("plantae") |>
  filter(year == 2024) |>
  group_by("family") |>
  atlas_counts()

# counts of plants by class
# but only to extract all taxonomic class names in 2024
class <- galah_call() |>
  identify("plantae") |>
  filter(year == 2024) |>
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
        filter(year == 2024) |>
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
  filter(!duplicated(family)) # remove any duplicate family counts after loop

# new_counts |> filter(!is.na(fill))

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



# ---------------------------------#
## PLOT

# fonts
font_add_google("Roboto", "roboto")
showtext_auto()

# palette
pal <- MetBrewer::met.brewer("Redon", 7)

# Bubble chart
bubble <-
  ggplot(data = new_counts,
       aes(id = family_common_name |> str_wrap(20), 
           area = count, 
           fill = common_name |> str_wrap(35)
           )) +
  geom_circlepack(color = "grey70", linewidth = 0.2) +
  ggnewscale::new_scale_colour() +
  geom_circlepack(aes(color = after_scale(colorspace::lighten(fill, .5))), linewidth = 0.6) +
  geom_circlepack_text(aes(color = ifelse(!is.na(fill), "white", "grey80"), 
                           family = "roboto",
                           lineheight = 0.8)) +
  scale_fill_manual(values = pal, 
                    na.value = "grey30") +
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

new_counts |>
  filter(family_common_name == "Brake ferns")

# Bar chart
bar <- 
  new_counts |>
  filter(!is.na(common_name),
         count > 3500) |>
  ggplot() + 
  geom_bar(aes(x = fct_reorder(family, count), 
               y = count,
               fill = common_name,
               # colour = common_name,
               # colour = after_scale(colorspace::lighten(common_name, .5))
               ),
           stat = "identity",
           linewidth = 0.8) + 
  geom_bar_text(aes(x = family, y = count,
                    label = glue::glue("{scales::comma(count)}")
                    ),
                min.size = 3, 
                outside = FALSE,
                family = "roboto", colour = "grey80") +
  geom_text(aes(x = family, y = count + 300,
                    label = glue::glue("{family_common_name}")
                    ),
                hjust = 0,
                family = "roboto", colour = "white") +
  ylim(c(0, 100000)) +
  scale_fill_manual(values = c(pal[6]), # choose palette manually to match bubble chart
                    na.value = "grey98") +
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
    title = "Observations of Plants in 2024",
    subtitle = glue("Taxonomic breakdown of **{scales::comma(sum(new_counts$count))}** *Plantae* 
                      observations recorded in the Atlas of Living Australia <br> 1 Jan - 25 Sep, 2024") |> 
      str_wrap(130),
    caption = "Data source: The {galah} package \n Dataviz: Dax Kellie \n Code source: Georgios Karamanis · github.com/gkaramanis/tidytuesday"
  ) +
  plot_layout(widths = c(2, 1.5)) &
  theme(
    plot.margin = unit(c(t = 2, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "#374E40", colour = NA),
    plot.title = element_text(face = "bold", size = 26, family = "roboto", colour = "white", hjust = 0.5, margin=margin(12,0, 12, 0)),
    plot.subtitle = ggtext::element_markdown(family = "roboto", lineheight = 1.2, colour = "white", size = 14, hjust = 0.5, margin = margin(0,0,-20,0)),
    plot.caption = element_text(hjust = 1, , family = "roboto", color = "grey70")
  )


# save
showtext_opts(dpi = 320)
ggsave(here::here("comms", "2024-09-25_plantae", "plots", "bubbles_plantae.png"),
       height = 10, width = 17, unit = "in", dpi = 320)
