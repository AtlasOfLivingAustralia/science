
library(galah)
library(dplyr)
library(tidyr)
library(readxl)
library(marquee)
library(ggplot2)
library(waffle) # remotes::install_github("hrbrmstr/waffle")
library(ggtext)
library(geomtextpath) # for labels
library(httr2)
library(showtext)
font_add_google("Roboto", "roboto")
showtext_auto()
galah_config(email = "dax.kellie@csiro.au")

species_all <- read_excel(
  here::here("comms", "2026-03-20_gastropods", "data", "AllalaSpeciesChecklist25-2025-12-11.xlsx"),
  sheet = 6,
  ) |>
  janitor::clean_names() |>
  select(-simon_records)

species_with_counts <- read_excel(
  here::here("comms", "2026-03-20_gastropods", "data", "AllalaSpeciesChecklist25-2025-12-11.xlsx"),
  sheet = 5,
) |>
  janitor::clean_names()

species_joined <- species_with_counts |>
  select(species_name, vernacular_name, number_of_records) |>
  right_join(species_all, 
            join_by(species_name == scientific_name)
            )

gastropods <- species_joined |>
  filter(stringr::str_detect(class, 
                             stringr::fixed("gastropoda", ignore_case=TRUE))
  ) |>
  replace_na(list(number_of_records = 0)) # replace NAs with 0s

# gastropods |>
#   select(species_name, number_of_records, class, order, family) |>
#   filter(is.na(order)) |>
#   distinct(order)

# ---
# There are lots of gastropods that don't have an order
# To fix this, we'll go with the next lowest taxonomic rank from class: Subclass
# Subclass seems to be complete (or mostly complete). 
# We'll back-fill those missing orders with their subclass
#
# I think the most straightforward way to get this info is to get all the 
# genera of ever species, download their taxonomic information, extract subclass, 
# then join that info with our
# ---

# get all taxonomic ranks from class to family
tree <- galah_call() |>
  identify("gastropoda") |>
  filter(rank == family) |>
  atlas_taxonomy()

subclasses <- tree |>
  filter(rank == "subclass") |>
  distinct(name) |>
  pull(name) |>
  as.list()

# extract family names in each subclass and return as tibble
get_families <- function(subclass_name) {
  
  result <- galah_call() |>
    identify({{subclass_name}}) |>
    filter(rank == family) |>
    atlas_taxonomy()
  
  families <- result |>
    filter(rank == "family") |>
    distinct(name) |>
    pull(name)
  
  tibble::tibble(
    subclass = paste0(subclass_name),
    family = families
  )
}

subclass_df <- subclasses |>
  purrr::map(
    \(name)
    get_families(name)
  ) |>
  bind_rows()

# Join subclass ranks to larger dataframe
gastropods_filled <- gastropods |>
  left_join(subclass_df,
            join_by(family == family))


# ---
# There are still a few names without subclass because they also don't have a family
# Let's grab their respective genus and return their subclass
# ---
unassigned_names <- gastropods_filled |>
  filter(is.na(subclass)) |>
  distinct(genus) |>
  pull(genus)

# Attempt to deal with homonyms
x <- tibble::tibble(
  genus = unassigned_names,
  class = "gastropoda"
  ) 

genera <- x |>
  group_split(genus) |>
  purrr::map(
    \(gastro_tibble)
    search_taxa(
      tibble::tibble(gastro_tibble)
    )
  ) |>
  bind_rows()

unassigned_ids <- genera |> 
  filter(rank == "genus") |>
  pull(taxon_concept_id)

# function that pings BIE API for classification
get_status <- function(taxon_concept_id) {
  
  base_url <- "https://api.ala.org.au/species/species/"
  url <- paste0(base_url, taxon_concept_id)
  
  response_data <- request(url) |>
    req_perform() |>
    resp_body_json() |>
    purrr::pluck("classification") |>
    bind_rows()
  
  return(response_data)
}

# a table of genus with matching subclass
unassigned_genus <- unassigned_ids |>
  purrr::map(
    \(name)
    get_status(name)
  ) |>
  bind_rows() |>
  select(genus, subclass)


# Join remaining subclass ranks to larger dataframe, then...
gastropods_filled_more <- gastropods_filled |>
  rows_patch(unassigned_genus, by = "genus", unmatched = "ignore") |>
  # ...fill missing orders with subclass...
  mutate(
    order_filled = case_when(
      is.na(order) ~ as.character(glue::glue("{subclass} (subclass)")),
      .default = order
    )
  ) |>
  # ...and fix names
  mutate(
    order_filled = stringr::str_to_title(order_filled),
    subclass = stringr::str_to_title(subclass)
  )



#### Counts

# get them order counts
# total_records <- gastropods_filled_more |>
#   replace_na(list(number_of_records = 0)) |>
#   group_by(order_filled) |>
#   summarise(n_records = sum(number_of_records)) |>
#   mutate(
#     total = sum(n_records),
#     percent = round((n_records/total) * 100, 1),
#     percent_label = case_when(
#       percent > 3 ~ glue::glue("{percent}%", sep = ""),
#       .default = " ")
#   ) |>
#   arrange(desc(percent)) |>
#   mutate(
#     lab.ypos = cumsum(percent) - 0.5 * percent
#   )

# total_records


# categorise orders of gastropods by how many records they have (10+, 1-10, 0)
gastro_record_class <- gastropods_filled_more |>
  mutate(
    n_order = case_when(
      number_of_records >= 100 ~ ">= 100 records",
      number_of_records >= 10 ~ ">= 10 records",
      number_of_records >= 1 ~ ">= 1 record",
      number_of_records < 1 ~ "No records"
    )
  ) |>
  group_by(subclass, n_order) |>
  count()

# Get all of the possible groups because some were missing
complete_groups <- gastro_record_class |>
  ungroup() |>
  expand(subclass, n_order)

# Join to see missing groups, replace NAs with 0
gastro_joined <- complete_groups |>
  full_join(gastro_record_class) |>
  replace_na(list(n = 0)) |>
  group_by(subclass) |>
  mutate(
    total = sum(n), # total by group for ordering plot
    percent = round((n/total) * 100, 0)
  )

# Round very low numbers so they actually appear on the plot
gastro_rounded <- gastro_joined |>
  ungroup() |>
  mutate(
    n_rounded = ((plyr::round_any(n, 10, f = ceiling)) / 10 )
  )

gastro_label <- gastro_joined |>
  select(subclass, n_order, total, percent) |>
  pivot_wider(names_from = n_order,
              values_from = percent) |>
  janitor::clean_names() |>
  mutate(
    percent_label = marquee_glue("**{#bb66b8 {x100_records}%}** **{#845b98 {x10_records}%}** **{#624776 {x1_record}%}** **{#483251 {no_records}%}**"),
    y_pos = ((plyr::round_any(total, 10, f = floor)) / 100 ) + 4
  )

total_descending <- gastro_rounded |>
  arrange(desc(total)) |>
  distinct(subclass) |>
  pull(subclass)

legend_order <- c(
  ">= 100 records",
  ">= 10 records",
  ">= 1 record",
  "No records"
  )

custom_palette <- c(
  ">= 100 records" = '#bb66b8', 
  ">= 10 records" = '#845b98', 
  ">= 1 record" = '#624776', 
  "No records" = '#483251'
)


title_text <- marquee_glue("Taxonomic completeness of *Gastropoda* records")
subtitle_text <- marquee_glue("Number of species with {#bb66b8 **100+ records**}, {#845b98 **10+ records**}, {#624776 **1+ records**} or {#483251 **no records**}  
                               in the Atlas of Living Australia
                           
                           \u25A0 = 10 species (rounded up)")


ggplot() +
  waffle::geom_waffle(
    data = gastro_rounded |>
      mutate(n_class = forcats::fct_relevel(n_order, legend_order)) |>
      arrange(match(n_order, legend_order)),
    aes(values = n_rounded, fill = n_order),
    n_rows = 10,        # number of squares in each row
    colour = "#231820",   # border color
    size = .35,
    flip = TRUE,
    radius = grid::unit(.05, "npc")
  ) +
  facet_wrap(~ forcats::fct_relevel(subclass, total_descending), 
             ncol = 13,
             strip.position = "bottom") +
  coord_equal() +
  pilot::theme_pilot(grid = "",
                     axes = "l") + 
  scale_fill_manual(values = custom_palette) +
  # labels
  scale_y_continuous(breaks = c(10.5, 20.5, 30.5, 40.5),
                     labels = c("1,000", "2,000", "3,000", "4,000")) +
  labs(y = "Number of species",
       title = title_text,
       subtitle = subtitle_text
  ) +
  geom_marquee(gastro_label,
               mapping = aes(x = 5.5, y = y_pos, label = percent_label),
               size = 3.5,
               colour = "white",
               style = classic_style(body_font = "roboto")) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y = element_text(size = 18, colour = "white"),
    axis.title.y = element_text(size = 19, colour = "white"),
    legend.position = "none",
    text = element_text(family = "roboto"),
    plot.title = element_marquee(size = 21, colour = "white"),
    plot.subtitle = element_marquee(size = 19, colour = "white"),
    strip.text.x = element_text(family = "roboto", size = 10.5, colour = "white"),
    strip.background.x = element_rect(colour = NA, fill = "#231820"),
    plot.background = element_rect(colour = NA, fill = "#231820"),
    panel.background = element_rect(colour = NA, fill = "#231820"),
    plot.margin = margin(t = 2, r = 4, b = 6, l = -1, "cm")
    )




# save
showtext_opts(dpi = 350)
ggsave(here::here("comms",
                  "2026-03-20_gastropods",
                  "plots", 
                  "gastropod_taxa-complete.png"),
       dpi = 350,
       height = 11,
       width = 15)




# unrelated bats query
# galah_call() |>
#   identify("Chiroptera") |>
#   distinct(scientificName) |>
#   count() |>
#   collect()

