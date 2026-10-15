# ---------------------------------------------------------------------------- #
# Title: Reptilia voronoi
# Author: Dax Kellie
# Date: 7 October, 2026
# ---------------------------------------------------------------------------- #

# packages
library(galah)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ltc) # devtools::install_github("loukesio/ltc-color-palettes")
library(marquee)
library(ggvmap) # remotes::install_github("loukesio/ggvmap")
library(showtext)
font_add_google("Roboto", "roboto")
showtext_auto()
galah_config(email = "dax.kellie@csiro.au")

# Download counts by family, genus
reptiles <- galah_call() |>
  identify("reptilia") |>
  filter(taxonRank %in% c("species", "subspecies"),
         year > 2024,
         decimalLongitude >= 110, decimalLongitude <= 155, # remove points outside of australia
         decimalLatitude >= -45, decimalLatitude <= -10
         ) |>
  apply_profile(ALA) |>
  group_by(order, family) |>
  count() |>
  collect()

# Add some common name labels
# These were mostly generated with the help of claude
reptile_families <- tribble(
  ~family,            ~common_name,
  "Scincidae",        "Skinks",
  "Agamidae",         "Dragons",
  "Elapidae",         "Elapids",
  "Varanidae",        "Goannas & Monitors",
  "Gekkonidae",       "Geckos",
  "Pythonidae",       "Pythons",
  "Colubridae",       "Colubrids",
  "Diplodactylidae",  "Austral geckos",
  "Pygopodidae",      "Legless lizards",
  "Carphodactylidae", "Knob-tailed & leaf-tailed geckos",
  "Typhlopidae",      "Blind snakes",
  "Homalopsidae",     "Water snakes",
  "Acrochordidae",    "File snakes",
  "Iguanidae",        "Iguanas",
  "Boidae",           "Boas",
  "Chelidae",         "Side-necked turtles",
  "Cheloniidae",      "Sea turtles",
  "Dermochelyidae",   "Leatherback sea turtles",
  "Emydidae",         "Pond turtles",
  "Carettochelyidae", "Pig-nosed turtles",
  "Chelydridae",      "Snapping turtles",
  "Crocodylidae",     "Crocodiles",
  "Alligatoridae",    "Alligators and caimans"
)

# get total and proportion, add labels
reptiles_wrangled <- reptiles |>
  mutate(
    total = sum(count),
    prop = (count/total)*100
  ) |> 
  # add common name labels
  left_join(
    reptile_families,
    join_by(family == family)
    )


reptiles_wrangled |> summarise(total = sum(count))


# PLOTS #-----------------------------------------

## voronoi
vm <- voronoi_map(
  reptiles_wrangled$prop, 
  labels = reptiles_wrangled$common_name,
  group = reptiles_wrangled$common_name, 
  clip = clip_circle(),
  seed = 244, 
  max_iter = 190
  )

big <- ggvmap(vm, palette = "casa natal", autoscale = TRUE, min_area = 0.02,
       wrap = 10, family = "roboto", label_size = 3.8) |>
  vm_add_labels(fmt = \(v) paste0(round(v, 1), "%"), autoscale = TRUE,
                min_area = 0.02, col = "grey90", size = 3.7)
big


# extract palette from plot to match in barplot
d1 <- layer_data(big, 1)
pal1 <- unique(data.frame(label = big@data$label, fill = d1$fill))
pal1 <- setNames(pal1$fill, pal1$label)
pal1


## Bar chart
bar <- reptiles_wrangled |>
  ggplot() +
  geom_bar(
    aes(
      x = forcats::fct_reorder(common_name, count),
      y = count,
      fill = common_name,
      colour = after_scale(colorspace::lighten(fill, .1))
    ),
    stat = "identity",
    linewidth = 0.8
  ) +
  ggfittext::geom_bar_text(
    aes(
      x = common_name, y = count,
      label = glue::glue("{scales::comma(count)}")
    ),
    min.size = 3,
    outside = FALSE,
    family = "roboto", colour = "grey90"
  ) +
  geom_text(
    aes(
      x = common_name, y = count + 1000,
      label = glue::glue("{common_name}")
    ),
    hjust = 0,
    family = "roboto", colour = "#232323"
  ) +
  ylim(c(0, 60000)) +
  scale_fill_manual(values = pal1) +
  guides(colour="none",
         fill = guide_legend(title = "")) +
  coord_flip() +
  pilot::theme_pilot(
    grid = "",
    axes = ""
  ) +
  theme(
    legend.position = "none",
    legend.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )


# Join
library(patchwork)

# Title
main_title <- marquee::marquee_grob(
    "**Reptilia observations by taxonomic family (2025-2026)**",
    style = marquee::classic_style(base_size = 20, align = "center"),
    x = 0.5, 
    y = 0.5,
    width = unit(1, "npc"),
    hjust = "center", 
    vjust = "center"
  )

# plots + final formatting
(big + bar) + 
  plot_layout(widths = c(2, 1)) + 
  # plot_layout(heights = c(1, 9.5)) +
  plot_annotation(
    title = marquee::marquee_glue("**Reptile observations (2025-2026)**"),
    subtitle = marquee::marquee_glue("Grouped by taxonomic family"),
    caption = marquee_glue("Left: Proportion of *Reptilia* occurrence records 
                           Right: Number of *Reptilia* occurrence records   
                           1 Jan 2025 - 13 Oct 2026"),
    theme = theme(
      plot.background = element_rect(fill = "#DDD3A0", colour = "transparent"),
      panel.background = element_rect(fill = "#DDD3A0", colour = "transparent"),
      plot.title = element_marquee(hjust = 0.5, size = 20, colour = "#232323"),
      plot.subtitle = element_marquee(hjust = 0.5, size = 16, colour = "#232323"),
      plot.margin = unit(c(1, 1, 1, 3), "cm"),
      plot.caption = element_marquee(colour = "#232323", width = unit(.17, "npc"))
    )
  )


# save
showtext_opts(dpi = 300)
ggsave(here::here("comms",
                  "2026-10-07_reptile-day",
                  "reptiles.png"),
       height = 10,
       width = 18,
       dpi = 300,
       )


# ltc("casa_natal")





# ------------- Other unused ideas ------------------------------------- #

# This section creates 3 small voronoi plots, one for each reptilia order,
# then places them with the main plot, scaled by the proportion of records they 
# contributed to the overall number of occurrence records

### Smaller components by order

reptiles |> distinct(order)

# proportional contribution to total reptilia records
reptiles |> 
  mutate(total = sum(count)) |>
  group_by(order, total) |>
  summarise(sum_count = sum(count)) |>
  mutate(prop = sum_count/ total)


# SMOL PLOTS
# squamata
reptiles_squam <- reptiles |>
  filter(order == "Squamata") |>
  mutate(
    total = sum(count),
    prop = (count/total)*100
  )

vm_squam <- voronoi_map(
  reptiles_squam$prop, 
  labels = reptiles_squam$family,
  group = reptiles_squam$order, 
  clip = clip_circle(),
  seed = 5, 
  max_iter = 80
)

small_squam <- ggvmap(vm_squam, palette = "sylvie", autoscale = TRUE, min_area = 0.9,
       wrap = 10, family = "roboto", label_size = 4.3) + 
  labs(title = "Squamata") +
  theme(
    plot.title = element_marquee(hjust = 0.5)
    )

# Testudines
reptiles_test <- reptiles |>
  filter(order == "Testudines") |>
  mutate(
    total = sum(count),
    prop = (count/total)*100
  )

vm_test <- voronoi_map(
  reptiles_test$prop, 
  labels = reptiles_test$family,
  group = reptiles_test$order, 
  clip = clip_circle(),
  seed = 5, 
  max_iter = 80
)

small_test <- ggvmap(vm_test, palette = "kiss", autoscale = TRUE, min_area = 0.9,
                      wrap = 10, family = "roboto", label_size = 4.3) + 
  labs(title = "Testudines (10%)") +
  theme(plot.title = element_text(hjust = 0.5))


# Crocodylia
reptiles_croc <- reptiles |>
  filter(order == "Crocodylia") |>
  mutate(
    total = sum(count),
    prop = (count/total)*100
  )

vm <- voronoi_map(
  reptiles_croc$prop, 
  labels = reptiles_croc$family,
  group = reptiles_croc$order, 
  clip = clip_circle(),
  seed = 5, 
  max_iter = 80
)

small_croc <- ggvmap(vm, palette = "minou", autoscale = TRUE, min_area = 0.999,
                     wrap = 10, family = "roboto") + 
  labs(title = "Crocodylia (2%)") +
  theme(plot.title = element_text(hjust = 0.5))


# JOIN BIG WITH SMOL

library(patchwork)

big / (small_squam + small_test + small_croc)

# Title
main_title <- wrap_elements(
  panel = marquee::marquee_grob(
    "**Reptilia observations (2025-2026)**",
    style = marquee::classic_style(base_size = 20, align = "center"),
    x = 0.5, 
    y = 0.5,
    width = unit(1, "npc"),
    hjust = "center", 
    vjust = "center"
  )
)

# subheader
order_label <- wrap_elements(
  panel = marquee::marquee_grob(
    "**Proportion of records by order**",
    style = marquee::classic_style(base_size = 14, align = "center"),
    x = 0.5, 
    y = 0.5,
    width = unit(1, "npc"),
    hjust = "center", 
    vjust = "center"
  )
)


row_small <- plot_spacer() + small_squam + plot_spacer() +
  plot_spacer() + small_test + plot_spacer() +
  plot_spacer() + small_croc + plot_spacer() +
  plot_layout(widths = c(6, 88, 6,
                         45, 10, 45,
                         49, 2, 49))

final <- main_title / big / order_label / row_small +
  plot_layout(heights = c(1, 9.5, 0.6, 3)) + 
  plot_annotation(
    theme = theme(
      plot.margin = margin(50, 50, 50, 50),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

final




# This section identifies any newly recorded species/subspecies in this 2025-26 
# time period when compared to everything else on the ALA

# species/subspecies only found in this specific period

reptiles_after_2025 <- galah_call() |>
  identify("reptilia") |>
  filter(
    taxonRank %in% c("species", "subspecies"),
    year >= 2025
    ) |>
  group_by(scientificName) |>
  count() |>
  collect()

reptiles_before_2025 <- galah_call() |>
  identify("reptilia") |>
  filter(
    taxonRank %in% c("species", "subspecies"),
    year <= 2024
    ) |>
  group_by(scientificName) |>
  count() |>
  collect()

reptiles_after_2025 |>
  filter(!scientificName %in% reptiles_before_2025$scientificName)

# 2 species with new records in the ALA ever. noice

# ---

# Why are there alligators in australia?
occs <- galah_call() |>
  identify("alligatoridae") |>
  filter(
    taxonRank %in% c("species", "subspecies"),
    year >= 2025
  ) |>
  group_by(scientificName) |>
  collect()

# looking at the record on the ALA, it's from the USA
occs$recordID
