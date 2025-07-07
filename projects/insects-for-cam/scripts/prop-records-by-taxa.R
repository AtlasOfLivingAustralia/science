# Title: Insects - taxonomic breakdown & record numbers
# Author: Dax


library(galah)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(ggpubr) # for donutchart
library(geomtextpath) # for labels
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

total_records <- insects |>
  group_by(order) |>
  summarise(n_records = sum(total_records_from_all_species)) |>
  mutate(
    total = sum(n_records),
    percent = round((n_records/total) * 100, 1),
    percent_label = case_when(
      percent > 3 ~ glue::glue("{percent}%", sep = ""),
      .default = " ")
  ) |>
  arrange(desc(percent)) |>
  mutate(
    lab.ypos = cumsum(percent) - 0.5 * percent
  )

gr_palette <- c('#003a00', '#254607', '#3f5210', '#575e1a', '#6e6b26', '#857833', '#9a8543', '#ae9455', '#c0a36a', '#d0b381', '#dcc49d', '#e3d7be', '#e1ece5', '#e0f5f6', '#d1ebed', '#c2e1e4', '#b3d7db', '#a4cdd2', '#95c4ca', '#86bac1', '#76b1b9', '#67a7b0', '#569ea8', '#44949f', '#2f8b97')

p1 <- total_records |>
  ggplot(aes(x = 1, y = percent, fill = reorder(order, -percent))) +
  geom_col() +
  geom_textpath(aes(label = percent_label),
                position = position_stack(vjust = 0.5),
                angle = 90,
                size = 5.7,
                color = "white",
                family = "roboto") +
  # scale_fill_viridis_d() +
  scale_fill_manual(values = gr_palette) +
  xlim(-.5, 2) +
  pilot::theme_pilot(grid = "", axes = "") +
  coord_polar(theta = "y") + 
  guides(fill = guide_legend(title = "")) +
  # labs(title = "Taxonomic representation of *Insecta* records",
  #      subtitle = marquee::marquee_glue("Proportion of total *Insecta* records by group in the ALA (*N* = {scales::comma(nrow(insects))})")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 13, family = "roboto"),
        legend.title = element_text(size = 16),
        legend.background = element_rect(fill = "transparent", colour = NA),
        text = element_text(family = "roboto"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = marquee::element_marquee(size = 30),
        plot.subtitle = marquee::element_marquee(size = 22),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.margin = unit(c(t = 1, l = 1, b = 1, r = 1), "cm"))

p1


# save
showtext_opts(dpi = 350)
ggsave(here::here("projects",
                  "insects-for-cam",
                  "plots", 
                  "donut_records-by-taxa.png"),
       dpi = 350,
       height = 11,
       width = 11)
