library(lubridate)
library(galah)
library(readr)
library(dplyr)
library(tidyverse)
library(rlang)
library(treemap)
library(plotly)
library(htmlwidgets)
library(sunburstR)

galah_config(run_checks = FALSE, verbose = TRUE)

##### number of records by month #####
order_by_month <- function(df) {
  df |> 
    mutate(order = paste0("2023-", month, "-01") |>
             parse_date_time(orders = "ybd") |>
             month()) |> 
    arrange(order) |>
    select(order, month, count)
}

count_by_month <- galah_call() |>
  filter(year == 2023) |>
  group_by(month) |>
  count() |>
  collect() |>
  order_by_month()

##### Number of species #####
galah_call(type = "species") |>
  filter(year == 2023) |>
  count() |>
  collect()

##### Top five species by clade #####
top_ten <- function(rank, taxa, not_in = FALSE) {
  top_five_spp <- galah_call() |>
    filter(year == 2023) |>
    (\(.) if (not_in)  filter(., {{rank}} != taxa) else filter(., {{rank}} == taxa))() |>
    group_by(species) |>
    count() |>
    slice_head(n = 10) |>
    collect()
  
  search_taxa(top_five_spp$species) |>
    mutate(count = top_five_spp$count) |>
    select(-search_term) |>
    relocate(species, vernacular_name, count)
}

top_mammals <- top_ten("class", "Mammalia")[1:5,] |> write_csv("top_mammals.csv")
top_birds <- top_ten("class", "Aves")[1:5,] |> write_csv("top_birds.csv")
top_amphibians <- top_ten("class", "Amphibia")[1:5,] |> write_csv("top_amphibians.csv")
top_reptiles <- top_ten("class", "Reptilia")[1:5,] |> write_csv("top_reptiles.csv")
top_fish <- top_ten("class", c("Actinopterygii", "Chondrichthyes"))[1:5,] |> write_csv("top_fish.csv")
top_inverts <- top_ten("phylum", "Chordata", not_in = TRUE)[c(2,3,4,7,8),] |> write_csv("top_inverts.csv") # returns a plant first and then a butterfly
top_plants <- top_ten("kingdom", "Plantae")[1:5,] |> write_csv("top_plants.csv")
top_fungi <- top_ten("kingdom", "Fungi")[1:5,] |> write_csv("top_fungi.csv")

##### Records by month for species #####
species_months <- function(species) {
  galah_call() |>
    filter(year == 2023,
           species == species) |>
    group_by(month) |>
    count() |>
    collect() |>
    order_by_month()
}

species_months(top_mammals$species[1])
species_months(top_birds$species[1])
species_months(top_amphibians$species[1])
species_months(top_reptiles$species[1])
species_months(top_inverts$species[2])
species_months(top_plants$species[1])
species_months(top_fungi$species[1])

##### Sunburst Plot #####
Animalia (Animals)
  - Chordata (Vertebrates)
    - Aves (Birds)
    - Mammalia (Mammals)
    - Reptilia (Reptiles)
    - Amphibia (Amphibians)
    - Actinopterygii + Chondrichthyes + Sarcopterygii (Fish?)
    - Others
  - Arthropoda (Arthropods)
  - Other
Plantae (Plants)
Fungi (Fungi)
Other

##### Create data for sunburst #####
tree_data <- galah_call() |>
  filter(year == 2023) |>
  group_by(kingdom, phylum, class) |>
  count() |>
  collect()

tree_data_clean <- tree_data |>
  mutate(kingdom = ifelse(kingdom %in% c("Animalia", "Plantae", "Fungi"),
                          kingdom,
                          "Other"),
         phylum = ifelse(kingdom == "Animalia",
                         phylum,
                         NA),
         phylum = ifelse(phylum %in% c("Chordata", "Arthropoda"),
                         phylum,
                         "Other"),
         class = ifelse(phylum == "Chordata",
                        class,
                        NA),
         class = ifelse(class %in% c("Aves", "Actinopterygii", "Mammalia",  "Reptilia", "Amphibia", "Chondrichthyes", "Sarcopterygii"),
                        class,
                        "Other"),
         kingdom = case_when(
           kingdom == "Animalia" ~ "Animals",
           kingdom == "Plantae" ~ "Plants",
           kingdom == "Fungi" ~ "Fungi",
           .default = kingdom
         ),
         phylum = case_when(
           phylum == "Chordata" ~ "Vertebrates",
           phylum == "Arthropoda" ~ "Arthropods",
           kingdom %in% c("Other", "Plants", "Fungi") ~ NA,
           .default = phylum
         ),
         class = case_when(
           class == "Aves" ~ "Birds",
           class %in% c("Actinopterygii", "Chondrichthyes", "Sarcopterygii") ~ "Fish",
           class == "Mammalia" ~ "Mammals",
           class == "Reptilia" ~ "Reptiles",
           class == "Amphibia" ~ "Amphibians",
           phylum %in% c("Other", "Arthropods") | is.na(phylum) ~ NA,
           .default = class
         )
  ) |>
  group_by(kingdom, phylum, class) |>
  summarise(count = sum(count)) |>
  ungroup()

colour_palette <- 
  c(
    "#E0AA51", # Animalia
    "#DE4BC3", # Fungi
    "#895C81", # Other
    "#6CDE4B" # Plantae
  )

taxa_colours <- (tree_data_clean |>
                   treemap(
                     index = c("kingdom", "phylum", "class"),
                     vSize = "count",
                     draw = FALSE,
                     palette = colour_palette
                   ))$tm |>
  arrange(kingdom, phylum, class) |>
  select(-level) |>
  distinct() |>
  select(kingdom, phylum, class, color)

sunburst_data <- rbind(
    tree_data_clean |>
      group_by(kingdom, phylum, class) |>
      summarise(count = sum(count), .groups = "drop") |>
      mutate(id = paste(kingdom, phylum, class),
             parent = paste(kingdom, phylum)) |>
      left_join(taxa_colours, by = c("kingdom", "phylum", "class")) |>
      rename(names = class) |>
      select(id, parent, names, count, color),
    tree_data_clean |>
      group_by(kingdom, phylum) |>
      summarise(count = sum(count), .groups = "drop") |>
      mutate(id = paste(kingdom, phylum),
             parent = kingdom,
             class = NA) |>
      left_join(taxa_colours, by = c("kingdom", "phylum", "class")) |>
      rename(names = phylum) |>
      select(id, parent, names, count, color),
    tree_data_clean |>
      group_by(kingdom) |>
      summarise(count = sum(count), .groups = "drop") |>
      mutate(id = paste(kingdom),
             parent = "TOTAL",
             phylum = NA, class = NA) |>
      left_join(taxa_colours, by = c("kingdom", "phylum", "class")) |>
      rename(names = kingdom) |>
      select(id, parent, names, count, color),
    tree_data_clean |>
      summarise(count = sum(count), .groups = "drop") |>
      mutate(id = "TOTAL", parent = "", names = "TOTAL", color = "#F2F2F2") |>
      select(id, parent, names, count, color)
  ) |>
  filter(!is.na(names)) |>
  arrange(desc(id))

sunburst_plotly <- plot_ly(
  data = sunburst_data,
  type = "sunburst",
  ids = ~id,
  labels = ~names,
  parents = ~parent,
  values = ~count,
  branchvalues = "total",
  maxdepth = 2,
  insidetextorientation = "radial",
  sort = FALSE,
  rotation = 90,
  marker = list(colors = ~color)
) |>
  layout(colorway = ~color)
  

# SunburstR plot
###### d2b SUNBURST ######
sunburstR_data <- tree_data_clean |>
  mutate(categories = paste(kingdom, phylum, class, sep = "-")) |>
  mutate(categories = gsub("-NA", "", categories)) |>
  left_join(taxa_colours, by = c("kingdom", "phylum", "class")) |>
  select(categories, count, color) |>
  as.data.frame()

sund2b(sunburstR_data,
       valueField = "count",
       showLabels = TRUE,
       rootLabel = "TOTAL",
       colors = htmlwidgets::JS(
         "function(name, d){return d.color || '#F2F2F2';}"
       ),
       breadcrumbs = sund2bBreadcrumb(enabled = FALSE))

sunburst(
  data = sunburstR_data,
  colors = sunburstR_data$color
)


##### ggplot basic sunburst #####
ggsun_king_data <- galah_call() |>
  filter(year == 2023) |>
  group_by(kingdom) |>
  count() |>
  collect() |>
  mutate(kingdom = ifelse(kingdom %in% c("Animalia", "Plantae", "Fungi"), kingdom, "Other")) |>
  group_by(kingdom) |>
  summarise(count = sum(count)) |>
  ungroup() |>
  arrange(desc(count)) |>
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         percent = 100 * fraction,
         ymin = c(0, head(ymax, n = -1)))

ggsun_clade_data <- tree_data |>
  mutate(kingdom = ifelse(kingdom %in% c("Animalia", "Plantae", "Fungi"),
                          kingdom,
                          "Other"),
         phylum = ifelse(kingdom == "Animalia",
                         phylum,
                         NA),
         clade = case_when(
           kingdom %in% c("Plantae", "Fungi", "Other") ~ kingdom,
           kingdom == "Animalia" & class %in% c("Aves", "Actinopterygii", "Mammalia",  "Reptilia", "Amphibia", "Chondrichthyes", "Sarcopterygii") ~ "Vertebrates",
           kingdom == "Animalia" ~ "Invertebrates"
         )
  ) |>
  group_by(clade) |>
  summarise(count = sum(count)) |>
  ungroup() |>
  mutate(clade = factor(clade, levels = c("Vertebrates", "Invertebrates", "Plantae", "Fungi", "Other"))) |>
  arrange(clade) |>
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         percent = 100 * fraction,
         ymin = c(0, head(ymax, n = -1)))

clade_sunburst <- ggplot(ggsun_clade_data) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = clade)) +
  scale_fill_manual(values = c("Vertebrates" = "#E0AA51", "Invertebrates" = "#F26649", "Plantae" = "#6CDE4B", "Fungi" = "#DE4BC3", "Other" = "#895C81")) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

ggsave(file = "clade_sunburst_nolabels.png", kingdom_sunburst,
       width = 5, height = 5, units = "in")
