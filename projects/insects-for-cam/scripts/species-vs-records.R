# Title: Insects and record numbers
# Author: Dax


library(galah)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(waffle)   
library(ggtext)
library(geomtextpath) # for labels
library(marquee)
library(showtext)
font_add_google("Roboto", "roboto")
showtext_auto()


# Download data
all_animals <- read_excel(here::here("projects", "insects-for-cam", "data", "AllAnimaliaForDax.xlsx")) |>
  janitor::clean_names()

# select all insects
# clean differences in upper vs sentence case
insects <- all_animals |>
  filter(stringr::str_detect(class, 
                             stringr::fixed("insecta", ignore_case=TRUE))
  ) |>
  mutate(
    order = stringr::str_to_sentence(order),
    order = case_when(
      order == "Lepioptera" ~ "Lepidoptera",
      .default = order
    )
  )

# categorise orders of insects by how many records they have (10+, 1-10, 0)
insects_record_class <- insects |>
  mutate(
    n_class = case_when(
      total_records_from_all_species >= 10 ~ ">= 10 records",
      total_records_from_all_species >= 1 ~ ">= 1 record",
      total_records_from_all_species < 1 ~ "No records"
    )
  ) |>
  group_by(order, n_class) |>
  count()

# Get all of the possible groups because some were missing
complete_groups <- insects_record_class |>
  ungroup() |>
  expand(order, n_class)

# Join to see missing groups, replace NAs with 0
insects_joined <- complete_groups |>
  full_join(insects_record_class) |>
  replace_na(list(n = 0)) |>
  group_by(order) |>
  mutate(
    total = sum(n), # total by group for ordering plot
    percent = round((n/total) * 100, 0)
    )

insects_rounded <- insects_joined |>
  ungroup() |>
  mutate(
    n_rounded = ((plyr::round_any(n, 100, f = ceiling)) / 100 )
  )

insects_label = insects_joined |>
  select(order, n_class, total, percent) |>
  pivot_wider(names_from = n_class,
              values_from = percent) |>
  janitor::clean_names() |>
  mutate(
    percent_label = marquee_glue("**{#2c5f22 {x10_records}%}** | **{#698c4c {x1_record}%}** | **{.grey60 {no_records}%}**"),
    y_pos = ((plyr::round_any(total, 100, f = floor)) / 1000 ) + 4
  )

total_descending <- insects_rounded |>
  arrange(desc(total)) |>
  distinct(order) |>
  # filter(order != "Coleoptera") |>
  pull(order)

legend_order <- c(">= 10 records",
                  ">= 1 record",
                  "No records")

custom_palette <- c(
  ">= 10 records" = '#305027', 
  ">= 1 record" = '#5b7749', 
  "No records" = 'grey80'
)


title_text <- marquee_glue("Taxonomic completeness of *Insecta* records")
subtitle_text <- marquee_glue("Number of species with {#305027 **10+ records**}, {#5b7749 **1+ records**} or {.grey60 **no records**} in the Atlas of Living Australia
                           
                           1 {.#698c4c {cli::symbol$square_small_filled}} = 100 species (rounded up)")


ggplot() +
  waffle::geom_waffle(
    data = insects_rounded |>
      mutate(n_class = forcats::fct_relevel(n_class, legend_order)) |>
      arrange(match(n_class, legend_order)),
    aes(values = n_rounded, fill = n_class),
    n_rows = 10,        # number of squares in each row
    colour = "white",   # border color
    flip = TRUE,
    radius = grid::unit(0, "npc")
  ) +
  facet_wrap(~ forcats::fct_relevel(order, total_descending), 
             ncol = 13,
             strip.position = "bottom") +
  coord_equal() +
  pilot::theme_pilot(grid = "",
                     axes = "l") + 
  scale_fill_manual(values = custom_palette) +
  # labels
  scale_y_continuous(breaks = c(10.5, 20.5),
                     labels = c("10,000", "20,000")) +
  labs(y = "Number of species",
       # title = title_text,
       subtitle = subtitle_text
       ) +
  geom_marquee(insects_label,
               mapping = aes(x = 5.5, y = y_pos, label = percent_label),
               size = 5,
               style = classic_style(body_font = "roboto")) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    legend.position = "none",
    text = element_text(family = "roboto"),
    plot.title = element_marquee(size = 30),
    plot.subtitle = element_marquee(size = 27),
    strip.text.x = element_text(family = "roboto", size = 18)
  )

## Individual facet point highlighted
# plot +
#   geom_point(data = data.frame(x = 10, y = 21, order = "Coleoptera"),
#              aes(x = x, y = y),
#              shape = 16,
#              colour = "red"
#              ) +
#   geom_text(data = data.frame(x = 7, y = 24, order = "Coleoptera", label = "10 species"),
#             aes(x = x, y = y, label = label), 
#             size = 4,
#             colour = "Red")

#FIXME: Download actual species numbers using galah

# save
showtext_opts(dpi = 350)
ggsave(here::here("projects",
                  "insects-for-cam",
                  "plots", 
                  "waffle_spec-vs-rec.png"),
       dpi = 350,
       height = 14,
       width = 28)

