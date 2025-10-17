# ------------------------------------------------------#
# Title: Tawny Frogmouth Observations
# Author: Dax Kellie
# Date: 2025-10-17
# ------------------------------------------------------#


# packages
library(galah)
library(dplyr)
library(ggplot2)
library(purrr)
library(ggtext)
library(here)
library(ozmaps)
library(sf)
library(showtext)
library(terra)
library(tidyterra)
library(tictoc)
library(beepr)

# set email
galah_config(email = "dax.kellie@csiro.au")

# counts of tawny frogmouth and morphs
galah_call() |>
  identify("Podargus strigoides") |>
  group_by(scientificName) |>
  apply_profile(ALA) |>
  atlas_counts()

#...grouped by year
galah_call() |>
  identify("Podargus strigoides") |>
  group_by(year) |>
  apply_profile(ALA) |>
  atlas_counts()

# download occurrences
tawny <- galah_call() |>
  identify("Podargus strigoides") |>
  apply_profile(ALA) |>
  atlas_occurrences() |>
  tidyr::drop_na(decimalLongitude,decimalLatitude)



# --------- Bertin map

# get australia map
aus <- ozmap_states |>
  st_transform(crs = st_crs(4326))

# create grid
oz_grid <- st_make_grid(aus,
                        what = "polygons",
                        cellsize = 0.45,
                        square = TRUE,
                        flat_topped = TRUE)

# subset to grid cells that are within land
keep_hexes <- st_intersects(oz_grid, aus)
keep_hexes <- keep_hexes |> 
  as.data.frame() |> 
  pull(row.id)
oz_grid <- oz_grid[keep_hexes]

ggplot() +
  geom_sf(data = oz_grid)


# convert aus square grid to 
oz_grid_tibble <- oz_grid |> 
  as_tibble() |>
  mutate(id = row_number())
  # group_split(id)


# Download all observations in the ALA in 2023

# function to send a query for each polygon and return counts of frogs
get_counts <- function(polygon){
  
  # convert to wkt
  wkt_string <- st_as_text(oz_grid[[polygon]])
  
  # get counts
  result <- galah_call() |>
    galah_identify("Podargus strigoides") |>
    galah_geolocate(wkt_string) |>
    galah_filter(decimalLongitude > 110) |> # avoid some outliers
    atlas_counts(limit = NULL)
  
  # light formatting to catch errors
  if(is.null(result)){
    tibble(count = NA, id = polygon)
  }else{
    result$id <- polygon
    result
  }
}

# download number of species for each polygon
tic()
counts_list <- map(seq_along(oz_grid_tibble$geometry), get_counts)
toc(); beepr::beep(2)
# this took ~20 minutes, there are a lot of squares to query


# bind lists to data frame
counts_df <- counts_list |> bind_rows()

# merge polygons & counts
counts_joined <- oz_grid_tibble |>
  left_join(counts_df, join_by(id == id))




# Extract centre points from grid

# extract centre
counts_centre <- counts_joined |> 
  mutate(centre = st_centroid(geometry))

# get xy of centre
obs_coords <- counts_centre |>
  select(count, centre, id) |>
  mutate(longitude = st_coordinates(centre)[,1],
         latitude = st_coordinates(centre)[,2]) |>
  rename(geometry = centre)


ggplot(obs_coords)+
  geom_sf(aes(geometry = geometry)) 


# subset to grid cells that are within land
keep_hexes <- st_intersects(obs_coords$geometry, aus)
keep_hexes <- keep_hexes |> 
  as.data.frame() |> 
  pull(row.id)

point_grid <- obs_coords |>
  filter(id %in% keep_hexes)

point_grid |>
  arrange(desc(count))

# Edit points for legend
point_grid <- point_grid |>
  mutate(dens = case_when(
    count < 1 ~ "A",
    count >= 1 & count < 10 ~ "B",
    count >= 10 & count < 100 ~ "C",
    count >= 100 & count < 1000  ~ "D",
    count >= 1000 & count < 10000 ~ "E",
    count >= 10000 & count < 100000 ~ "F",
    # count >= 100000 ~ "G",
    TRUE ~ "H"
  ))

point_grid |>
  group_by(dens) |>
  count()

# palette
dot_palette <- colorRampPalette(c(
  "#8a715b",
  "#443a32"
))(7) 




## Map
library(showtext)
font_add_google("Roboto", "roboto")
showtext_auto()

library(marquee)

ggplot() +
  geom_sf(
    data = aus,
    colour = "transparent",
    fill = NA
  ) +
  geom_sf(
    data= point_grid,
    mapping= aes(size = dens,
                 # color = dens,
                 fill = dens,
                 geometry = geometry),
    shape = 21,
    color = "#ab927d" # #b7a69b
  ) +
  scale_size_manual(
    values = c(.01, 1, 2.5, 5, 6.5, 8),
    label = c("< 1 observation", "1 to 9", "10 to 99", 
              "100 to 999", "1,000 to 9,999", ">= 10,000")
  ) + 
  scale_fill_manual(
    values = dot_palette) +
  coord_sf(xlim = c(110, 155), 
           ylim = c(-45, -10)) +
  guides(size = guide_legend(title = "Observations"),
         color = guide_legend(title = "Observations"),
         fill = "none") +
  labs(
    title = "Tawny Frogmouth Observations",
    subtitle = glue::glue("Records of *Podargus strigoides* in the Atlas of Living Australia (*N* = {scales::comma(nrow(tawny))})"),
    caption = glue::glue("Dataviz by Dax Kellie<br>Data downloaded in R using the galah package")
  ) +
  theme_void() + 
  theme(
    legend.position = "right",
    text = element_text(family = "roboto"),
    plot.title = element_text(family = "roboto", size = 23, colour = "#a64f12", face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 20, colour = "#232323"),
    legend.title = element_text(size = 16, colour = "#232323"),
    legend.text = element_text(size = 14, colour = "#232323"),
    plot.background = element_rect(fill = "#f4f0f0", color = NA),
    panel.background = element_rect(fill = "#f4f0f0", color = NA),
    plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
    plot.caption = ggtext::element_textbox_simple(
      colour = "grey10",
      size = 8,
      halign = 1,
      lineheight = 0.25,
      margin = margin(t = 0.5,
                      r = -4, 
                      unit = "cm")
    )
  )

# save
ggsave(here::here("comms", "2025-10-16_tawny-frogmouth", "plots", "tawny-frogmouth2.svg"),
       height = 10, width = 10, unit = "in", dpi = 320)
