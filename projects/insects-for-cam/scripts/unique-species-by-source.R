
library(galah)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(showtext)

showtext_auto()
font_add_google("Roboto", "roboto")

galah_config(email = "dax.kellie@csiro.au")

galah_call() |>
  identify("insecta") |>
  # filter(basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")) |>
  group_by(basisOfRecord) |>
  atlas_counts(type = "species")

# big overall totals
all_species <- galah_call() |>
  identify("insecta") |>
  # filter(basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")) |>
  atlas_counts(type = "species")

all_obs <- galah_call() |>
  identify("insecta") |>
  # filter(basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")) |>
  atlas_counts()


# total observations from human observations vs preserved specimen
galah_call() |>
  identify("insecta") |>
  filter(basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")) |>
  group_by(basisOfRecord) |>
  atlas_counts(type = "species")


# As a test, here are all the observations by taxonRank
ranks <- search_all(fields, "taxonRank") |>
  show_values()

galah_call() |>
  identify("insecta") |>
  filter(basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")) |>
  group_by(taxonRank) |>
  atlas_counts()


# Number of observations identified to the rank of 'species'
# (in case the discrepancy between the total obs count above is unclear)
specimen <- galah_call() |>
  identify("insecta") |>
  filter(basisOfRecord == c("PRESERVED_SPECIMEN")) |>
  group_by(species) |>
  atlas_counts(limit = 1e7)

humanobs <- galah_call() |>
  identify("insecta") |>
  filter(basisOfRecord == c("HUMAN_OBSERVATION")) |>
  group_by(species) |>
  atlas_counts(limit = 1e7)


# Species unique to humanobs data
only_humanobs <- humanobs |>
  filter(!species %in% specimen$species) |>
  nrow()

# Species unique to specimen data
only_specimen <- specimen |>
  filter(!species %in% humanobs$species) |>
  nrow()


# here is the big table
# sum of all records from each source of taxa ranked to species level
counts_by_source <- tibble::lst(humanobs, specimen) |>
  purrr::map(\(df)
             df |>
               summarise(unique_obs = sum(count))
  ) |>
  enframe(name = "source", value = "records") |>
  unnest(cols = records) |>
  mutate(
    unique_species = c(only_humanobs, only_specimen),
    source = case_when(source == "humanobs" ~ "Human Observations",
                       source == "specimen" ~ "Preserved Specimen"),
    total_species = all_species$count,
    total_obs = all_obs$count,
    percent_species = glue::glue("{scales::percent(unique_species/total_species)}"),
    percent_obs = glue::glue("{scales::percent(unique_obs/total_obs)}")
  )

counts_by_source



# PLOT ----------

custom_palette <- c("#4E613E", '#7A9491')

my_theme <- function() {
  theme(
    legend.position = "none",
    text = element_text(family = "roboto"),
    plot.title = ggtext::element_markdown(size = 22, margin = margin(b = 10)),
    plot.subtitle = ggtext::element_markdown(size = 15),
    axis.title = element_text(size = 19),
    axis.text = element_text(size = 18)
  )
}

# number of observations -------------

# create the start and end points of arrow
arrow_right <- tibble(
  xstart = 1.59, 
  xpoint = 1.7, 
  ystart = all_obs$count - 3e5, 
  ypoint = all_obs$count - 1e5  
)

arrow_left <- tibble(
  xstart = 1.41, 
  xpoint = 1.3, 
  ystart = all_obs$count - 3e5, 
  ypoint = all_obs$count - 1e5  
)

# plot
p_obs <- counts_by_source |>
  ggplot() +
  geom_bar(
    aes(
      x = source,
      y = total_obs
    ),
    fill = "transparent",
    colour = "grey70",
    stat = "identity",
    width = 0.7
  ) +
  geom_bar(
    aes(
      x = source,
      y = unique_obs,
      fill = source,
      colour = source
    ),
    stat = "identity",
    width = 0.7
  ) +
  geom_text(
    mapping = aes(
      x = source,
      y = unique_obs,
      colour = source,
      label = percent_obs
    ),
    size = 7,
    hjust = .5,
    vjust = -.5
  ) +
  annotate("text",
           x = 1.5, 
           y = all_obs$count - 3e5, 
           label = "Total\nin the ALA",
           alpha = 0.9,
           family = "roboto") +
  geom_curve( 
    data = arrow_right, 
    aes(x = xstart, y = ystart, xend = xpoint, yend = ypoint),
    arrow = arrow(length = unit(0.08, "inch")),
    linewidth = 1.1,
    color = "grey30",
    curvature = 0.4
  ) +
  geom_curve( 
    data = arrow_left, 
    aes(x = xstart, y = ystart, xend = xpoint, yend = ypoint),
    arrow = arrow(length = unit(0.08, "inch")),
    linewidth = 1.1,
    color = "grey30",
    curvature = -0.4
  ) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  scale_colour_manual(values = custom_palette) +
  scale_fill_manual(values = custom_palette) +
  pilot::theme_pilot(
    grid = "",
    axes = "l"
  ) +
  labs(
    y = "Number of observations",
    x = "Data source",
    title = "Number of *Insecta* records unique to data source"
  ) +
  my_theme()




# number of species -------------

# create the start and end points of arrow
arrow_right <- tibble(
  xstart = 1.59, 
  xpoint = 1.7, 
  ystart = all_species$count - 3e3, 
  ypoint = all_species$count - 9e2  
)

arrow_left <- tibble(
  xstart = 1.41, 
  xpoint = 1.3, 
  ystart = all_species$count - 3e3, 
  ypoint = all_species$count - 9e2  
)

# Plot
p_species <- counts_by_source |>
  ggplot() +
  geom_bar(
    aes(x = source,
        y = total_species),
    fill = "transparent",
    colour = "grey70",
    stat = "identity",
    width = 0.7
  ) +
  geom_bar(
    aes(x = source, 
        y = unique_species,
        fill = source,
        colour = source),
    stat = "identity",
    width = 0.7
  ) +
  geom_text(
    mapping = aes(x = source, 
                  y = unique_species, 
                  colour = source,
                  label = percent_species),
    size = 7,
    hjust = .5,
    vjust = -.5
  ) +
  annotate("text",
           x = 1.5, 
           y = all_species$count - 3e3, 
           label = "Total\nin the ALA",
           alpha = 0.9,
           family = "roboto") +
  geom_curve(
    data = arrow_right,
    aes(x = xstart, y = ystart, xend = xpoint, yend = ypoint),
    arrow = arrow(length = unit(0.08, "inch")),
    linewidth = 1.1,
    color = "grey30",
    curvature = 0.4
  ) +
  geom_curve(
    data = arrow_left,
    aes(x = xstart, y = ystart, xend = xpoint, yend = ypoint),
    arrow = arrow(length = unit(0.08, "inch")),
    linewidth = 1.1,
    color = "grey30",
    curvature = -0.4
  ) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  scale_colour_manual(values = custom_palette) +
  scale_fill_manual(values = custom_palette) +
  pilot::theme_pilot(
    grid = "",
    axes = "l"
  ) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  scale_color_manual(values = custom_palette) +
  scale_fill_manual(values = custom_palette) +
  pilot::theme_pilot(grid = "",
                     axes = "l") + 
  labs(
    y = "Number of species",
    x = "Data source",
    title = "Number of *Insecta* species unique to data source"
    ) +
  my_theme()


# save
showtext_opts(dpi = 350)

ggsave(plot = p_obs,
       here::here("projects",
                  "insects-for-cam",
                  "plots", 
                  "bar_unique-obs.png"),
       height = 10,
       width = 10,
       dpi = 350)

ggsave(plot = p_species,
       here::here("projects",
                  "insects-for-cam",
                  "plots", 
                  "bar_unique-species.png"),
       height = 10,
       width = 10,
       dpi = 350)


