
library(galah)
library(tidyverse)
library(geomtextpath) # remotes::install_github("AllanCameron/geomtextpath")
library(showtext)
showtext_auto()
font_add_google("Roboto", "roboto")

counts_by_type <- galah_call() |>
  filter(year >= 1925) |>
  group_by(year, basisOfRecord) |>
  atlas_counts()

counts_clean <- counts_by_type |>
  mutate(
  record_source = case_when(
    basisOfRecord %in% c("HUMAN_OBSERVATION") ~ "Human observations",
    basisOfRecord %in% c("PRESERVED_SPECIMEN", "MATERIAL_SAMPLE", "LIVING_SPECIMEN", "FOSSIL_SPECIMEN") ~ "Collections",
    basisOfRecord %in% c("MACHINE_OBSERVATION") ~ "Machine observations",
    .default = "Other"
    )
  ) |>
  filter(record_source != "Other") |> # for simplicity
  select(year, record_source, count) |>
  group_by(year, record_source) |>
  summarise(count = sum(count)) |>
  arrange(record_source, year) |>
  group_by(record_source) |>
  mutate(
    csum = cumsum(count)
  )

custom_palette <- c('#7A9491', '#305027', '#CEA17E')

ggplot(data = counts_clean,
       aes(x = as.integer(year), 
           y = csum, 
           colour = record_source,
           fill = record_source)) +
  geom_textline(
    aes(label = record_source),
    size = 8, 
    linewidth = 1.16,
    family = "roboto", 
    hjust = 0.8, 
    vjust = 0.5) +
  scale_colour_manual(values = custom_palette) +
  pilot::theme_pilot(grid = "h",
                     axes = "",
                     grid_color = "grey95") + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = c(2e7, 4e7, 6e7, 8e7, 1e8, 1.2e8),
                     limits = c(0, 1.2e8),
                     labels = scales::comma_format()) + 
  labs(
    x = "Year",
    y = "Number of records"
  ) + 
  theme(
    legend.position = "none",
    axis.title = element_text(family = "roboto", size = 18),
    axis.text = element_text(family = "roboto", size = 16),
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA)
  )


# save
showtext_opts(dpi = 350)
ggsave(here::here("projects",
                  "insects-for-cam",
                  "plots", 
                  "line_records-by-source.png"),
       dpi = 350,
       height = 10,
       width = 13)

