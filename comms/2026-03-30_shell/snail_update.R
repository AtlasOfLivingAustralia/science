
library(galah)
library(ggplot2)
library(dplyr)
library(glue)
library(geomtextpath)
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


##| Experimenting with a functional test to determine whether to use 
##| light or dark text for readability:

# Uses the WCAG relative luminance formula to measure how bright a colour appears.
text_colour_for_bg <- function(hex_colours, threshold = 0.25) {
  
  # Screen colours are gamma-compressed (sRGB): stored values aren't proportional
  # to actual light intensity. This step undoes that compression so that, e.g.,
  # RGB(128, 128, 128) is treated as ~22% brightness, not 50%.
  to_linear <- function(c) {
    ifelse(c <= 0.03928, c / 12.92, ((c + 0.055) / 1.055) ^ 2.4)
  }
  
  # convert hex to R, G, B channels (0-1 scale)
  converted_colours <- (col2rgb(hex_colours) / 255)
  
  converted_colours |>         
    t() |> # transpose so each row is one colour
    as_tibble(.name_repair = "minimal") |>
    set_names(c("r", "g", "b")) |>
    mutate(across(c(r, g, b), to_linear)) |> # undo gamma compression
    # Relative luminance: human eyes are most sensitive to green, least to blue,
    # so each channel is weighted accordingly
    mutate(luminance = 0.2126 * r + 0.7152 * g + 0.0722 * b,
           text_colour = if_else(luminance > threshold, "black", "#E1D8C1")
           ) |>
    pull(text_colour)
}

shell_palette <- c(
  "Cerithium" = "#834019",
  "Conus" = "#ba3421",
  "Haliotis" = "#e3b798",
  "Limacina" = "#ab925e",
  "Nassarius" = "#dc8943",
  "Nerita" = "#15181f",
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
  select(genus, count, y, x, xend, yend) |>
  mutate(
    record_count = scales::comma(count),
    label = glue("*{genus}* | *N* = {record_count}"),
    # place all labels at the same x position along their bar
    hjust_label = 7000 / xend,
    text_colour = text_colour_for_bg(shell_palette[as.character(genus)])
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
  geomtextpath::geom_textsegment(
            data = snails_label,
            aes(x = x, xend = xend,
                y = y, yend = yend,
                label = label,
                hjust = hjust_label,
                textcolour = text_colour),
            family = "roboto",
            size = 2.25,
            vjust = 0.5,
            linecolour = NA,
            rich = TRUE) +
  labs(
    # title = "Most recorded gastropods in the Atlas of Living Australia",
       caption = "Source: Atlas of Living Australia  
       Viz by Dax Kellie") +
  scale_color_manual(values = shell_palette) +
  coord_polar(clip = "off", start = -pi/2) +
  ylim(-25, 15) +
  xlim(0, max_x) +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
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
                    "shell_transparent.png"),
  dpi = 300,
  width = 11,
  height = 7
)





