
library(galah)
library(ggplot2)
library(dplyr)
library(glue)
library(marquee)
library(showtext)
font_add_google("Roboto Condensed", "roboto")
showtext_auto()

# This is almost entirely thanks to Ijeamaka Anyene. I've just tweaked some 
# code she made open, but all credit to her.
# Code: https://github.com/Ijeamakaanyene/tidytuesday/blob/master/scripts/2021_06_dubois_data.Rmd

counts <- galah_call() |>
  identify("gastropoda") |>
  group_by(genus) |>
  atlas_counts()

counts <- counts |>
  slice(1:10) # top ten


# Color palette pulled from original image
bcgrnd = "#E1D8C1"

shell_palette <- c(
  "Cerithium" = "#834019",
  "Conus" = "#ba3421",
  "Haliotis" = "#e3b798",
  "Limacina" = "#ab925e",
  "Nassarius" = "#15181f",
  "Nerita" = "#dc8943",
  "Lunella" = "#d8951f",
  "Cellana" = "#503e31",
  "Siphonaria" = "#ab6433",
  "Cornu" = "#ce9574"
)

# Choosing a round number near the highest count value
max_x = 78000 / 2
# Slope calculated using a middle value that's half-way-ish to the top number
slope = (12 - -1) / (0 - 30000)


snails_spiral <- counts |>
  mutate(genus = as.factor(genus),
         y = seq(10, by = -1.5, length.out = 10),
         x = 0) |>
  rowwise() |>
  mutate(xend = min(count, max_x),
         yend = slope * xend + y) |>
  mutate(y_2 = yend,
         x_2 = 0,
         xend_2 = if_else(count < max_x, NA_real_, count - max_x),
         yend_2 = slope * xend_2 + y_2)


snails_label = snails_spiral |>
  select(genus, count, y, x) |>
  mutate(
    record_count = scales::comma(count),
    label = marquee::marquee_glue("*{genus}* | *N* = {record_count}")
      )

ggplot(data = snails_spiral) +
  # first part of spiral
  geom_segment(aes(x = x, xend = xend,
                   y = y, yend = yend,
                   color = genus), 
               size = 3.1) +
  # second part of spiral
  geom_segment(aes(x = x_2, xend = xend_2,
                   y = y_2, yend = yend_2,
                   color = genus),
               size = 3.1) +
  marquee::geom_marquee(data = snails_label,
            aes(x = x, y = y,
                label = label),
            family = "roboto",
            size = 2.5,
            hjust = 1.1,
            vjust = 0.5,
            angle = 90) +
  labs(
    # title = "Most recorded gastropods in the Atlas of Living Australia",
       caption = "Source: Atlas of Living Australia  
       Viz by Dax Kellie") +
  scale_color_manual(values = shell_palette) +
  coord_polar(clip = "off", start = -pi/2) +
  ylim(-25, 15) +
  xlim(0, max_x) +
  theme_void() +
  theme(plot.background = element_rect(fill = bcgrnd, color = NA),
        panel.background = element_rect(fill = bcgrnd, color = NA),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 5, unit = "cm"),
        plot.title = element_text(family = "roboto", size = 12, colour = "#ab925e", hjust = 0.5),
        plot.caption = element_marquee(size = 5, family = "roboto"),
        legend.position = "none"
        ) 


# save
showtext_opts(dpi = 300)
ggsave(
  file = here::here("comms",
                    "2026-03-30_shell",
                    "plots",
                    "shell.png"),
  dpi = 300,
  width = 11,
  height = 7
)





