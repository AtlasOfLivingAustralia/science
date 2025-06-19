# -----------------------------------------------------------------#
# Title: Gondwana rainforests - Make map + bubble plots
# Author: Dax Kellie
# -----------------------------------------------------------------#

library(tidyverse)
library(ggimage)
library(ggtext)
library(glue)
library(sysfonts)
library(showtext)
library(patchwork)
# spatial
library(sf)
library(ozmaps)
library(cowplot)

font_add_google("Roboto", "roboto")
showtext_auto()


# load data
top_taxa <- read_csv(here::here("comms",
                                "2025-05-22_rainforests",
                                "data",
                                "top-taxa-images.csv"))
top_taxa


# Bubble --------------------------------------------------------

ggplot() +
  geom_point(data = top_taxa,
             aes(y = count, x = 1, colour = group)) +
  scale_y_log10(labels = scales::label_comma())

# create positional arguments (similar to beeswarm)
df_plot <- top_taxa |>
  mutate(
    path = paste0(tolower(str_replace_all(word(scientificName, 1)," ","_")),".png"),
    # Numbers must be log-transformed for reasonable plotting
    # This attempts to group by log ranges
    log_group = ((plyr::round_any(log10(count), .3, f = floor)) / 100 ),
    ) |>
  select(scientificName, common_name, count, group, log_group, path) |>
  ungroup() |>
  arrange(desc(count)) |> # this is so very important for the final plot
  group_by(log_group) |> 
  mutate(group_count = n(),
         row = row_number() - 1,
         type = case_when(group_count %% 2 == 0 ~ "even", TRUE ~ "odd"),
         spacer = 2.8,
         max = 0 - ((group_count/2) - 0.5) * spacer,
         pos = max + spacer * row
         )  |>
  mutate(image = glue("./comms/2025-05-22_rainforests/images/{path}"))

p_bubble <- ggplot() +
  # size cannot be mapped to aes, set manually outside of aes
  geom_image(data = df_plot,
             mapping = aes(y = count, x = pos, image = image),
             position = position_jitter(width = 0, height = .12),
             size = 0.088,
             asp = 9.5/6) +
  scale_y_log10(labels = scales::label_comma()) +
  scale_x_continuous(limits = c(-10, 10)) +
  expand_limits(x = -10) +
  coord_flip() + 
  labs(y = "Number of records (log-scaled)",
       x = "Points spread at random for readability") +
  pilot::theme_pilot(axes = "l",
                     grid = "v",
                     grid_color = "#8ba973",
                     axis_line_color = "#8ba973") +
  theme(
    plot.background = element_rect(fill="#475842", color="#475842"),
    panel.background = element_blank(),
    text = element_text(colour = "#DEC6A6", family = "roboto"),
    axis.text.x = element_text(colour = "#DEC6A6", family = "roboto", size = 12),
    axis.title.x = element_text(colour = "#DEC6A6", family = "roboto", size = 13),
    # axis.line.x = element_line(colour = "#8ba973"),
    axis.text.y = element_blank(),
    axis.title.y = element_text(colour = "#DEC6A6", family = "roboto", size = 13),
    panel.grid = element_line(colour = "#8ba973")
  )




# Map ---------------------------------

aus <- ozmaps::ozmap_states |>
  st_transform(crs = 4326)

# --- Gondwana rainforest area

# Australian heritage sites
# From: https://fed.dcceew.gov.au/datasets/erin::national-heritage-list-spatial-database-nhl-public/about
heritage <- sf::st_read(here::here("data",
                                   "National_Heritage_List_Spatial_Database_(NHL)_-_public",
                                   "National_Heritage_List_Spatial_Database_(NHL)_-_public.shp")
) |>
  rmapshaper::ms_simplify(keep = 0.01) |>
  sf::st_make_valid() |>
  sf::st_transform(crs = 4326)

# filter to gondwana
gondwana <- heritage |>
  filter(stringr::str_detect(NAME, "Gond"))


# create the start and end points of arrow
arrow <- tibble(
  x1 = 157,    # starting x-coordinate
  x2 = 154,    # ending x-coordinate
  y1 = -28.8,  # starting y-coordinate
  y2 = -30     # ending y-coordinate
)

map <- ggplot() +
  geom_sf(data = aus,
          fill = "#293D22",
          colour = "#DEC6A6") +
  geom_sf(data = gondwana,
          colour = "#EA765D",
          fill = "#F3B3A5") +
  coord_sf(ylim = c(-43, -10), xlim = c(139, 162)) + 
  geom_curve( 
    data = arrow, 
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")),
    linewidth = 1.5,
    color = "#EA765D",
    curvature = -0.5
  ) +
  annotate("text", x = 157, y = -27.1, label = "Gondwana\nRainforests", size = 5.2, colour = "#DEC6A6", family = "roboto", hjust = 0.5) +
  theme_void() +
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill="#475842", color="#475842"),
    legend.position = "none"
  )
  
# Inset map
inset <- map + 
  coord_sf(
    xlim = c(151.25 , 153.4),
    ylim = c(-32.3, -27.85),
    expand = FALSE
  ) + 
  theme_void() +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "white", fill = NA, linewidth = 0.3))

# Add bbox to main map
main_bbox <- map + 
  geom_rect(aes(xmin = 151.25, xmax = 153.4,
                ymin = -32.3, ymax = -27.85),
            colour = "white",
            fill = NA, 
            lwd = 0.2) 

# inset + main
combined_map <- ggdraw(main_bbox) +
  draw_plot(inset, 
            x = 0.42, y = 0.53, 
            width =0.65, height = 0.45)

combined_map




# Complete viz ----------------------------

# various plot text
subtitle <- 'A World Heritage Area encompassing the largest subtropical and cool temperate rainforests in the world. Below is a glimpse of the area\'s 
     biodiversity, showing images of the most-recorded birds, mammals, invertebrates, plants and fungi, using data from the Atlas of Living Australia.' |>
  str_wrap(146)

image_credits <- top_taxa |>
  glue::glue_data("{common_name} by {photographer} ({license})") |>
  glue::glue_collapse(sep = " | ")

caption <- glue::glue(
  "Dataviz by Dax Kellie | Made in R | Image credits: {image_credits}"
  ) |>
  str_wrap(270)

# Merged plot (using patchwork)
(combined_map + p_bubble) + 
  plot_layout(widths = c(30, 60)) +
  plot_annotation(
    title = 'A snapshot of Gondwana Rainforests\' biodiversity',
    subtitle = subtitle,
    caption = caption
  ) & 
  theme(
    plot.title = element_text(size = 30, colour = "white", family = "roboto", margin = margin(l=30, b=10)),
    plot.subtitle = element_text(size = 14, colour = "white", family = "roboto", margin = margin(l=30)),
    plot.caption = element_text(colour = "#DEC6A6", family = "roboto", size = 5),
    plot.background = element_rect(fill = "#475842", colour = "#475842"),
    panel.background = element_blank(),
    plot.margin = unit(c(t = 1, b = 1, r = 1, l = 0),"cm"),
    )

# Save
showtext_opts(dpi = 350)
factor <- 19/14
ggsave(here::here("comms",
                  "2025-05-22_rainforests",
                  "plots",
                  "gondwana.png"), 
       height = 14/factor, 
       width = 14, 
       unit = "in", 
       dpi = 350)
