# ------------------------------------------------------------------------- #
# 2023 Atlas of Living Australia observations in dots (Jacques Bertin map)


# packages
library(galah)
library(tidyverse)
library(ggtext)
library(here)
library(ozmaps)
library(sf)
library(showtext)
library(terra)
library(tidyterra)
library(tictoc)
library(beepr)


# get australia map
aus <- ozmap_states |>
  st_transform(crs = st_crs(4326))

# create grid
oz_grid <- st_make_grid(aus,
                        what = "polygons",
                        cellsize = 0.5,
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



# Download all observations in the ALA in 2023

# set email
galah_config(email = "dax.kellie@csiro.au")

# function to send a query for each polygon and return counts of frogs
get_counts <- function(polygon){
  
  # convert to wkt
  wkt_string <- st_as_text(oz_grid[[polygon]])
  
  # get counts
  result <- galah_call() |>
    galah_geolocate(wkt_string) |>
    galah_filter(year == 2023,
                 decimalLongitude > 110) |>
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

# Edit points for legend
point_grid <- point_grid |>
  mutate(dens = case_when(
    count < 1 ~ "A",
    count >= 1 & count < 10 ~ "B",
    count >= 10 & count < 100 ~ "C",
    count >= 100 & count < 1000  ~ "D",
    count >= 1000 & count < 10000 ~ "E",
    count >= 10000 & count < 100000 ~ "F",
    count >= 100000 ~ "G",
    TRUE ~ "H"
  ))

point_grid |>
  group_by(dens) |>
  count()

# palette
dot_palette <- colorRampPalette(c(
  "#895C81",
  "#DE4BC3"
))(7) 




## Map

ggplot() +
  geom_sf(
    data = aus,
    colour = "grey80",
    fill = NA
  ) + 
  geom_sf(
    data= point_grid,
    mapping= aes(size = dens,
                 # color = dens,
                 fill = dens,
                 geometry = geometry),
    shape = 21,
    color = "grey90"
  ) +
  scale_size_manual(
    values = c(1, 2, 3, 3, 4, 5, 6),
    label = c("< 1 observation", "1 to 9", "10 to 99", 
              "100 to 999", "1,000 to 9,999", "10,000 to 999,000", 
              "≥ 100,000 observations")
  ) + 
  scale_fill_manual(
    values = dot_palette) +
  coord_sf(xlim = c(110, 155), 
           ylim = c(-45, -10)) +
  guides(size = guide_legend(title = "Observations"),
         color = guide_legend(title = "Observations")) +
  theme_void() + 
  theme(
    legend.position = "none"
  )

# save
ggsave(here::here("plots", "aus_2023_observations.svg"),
       height = 10, width = 10, unit = "in", dpi = 320)


# try an animation
library(gganimate)

animate_df <- point_grid |>
  filter(count > 0) |>
  arrange(dens) |>
  mutate(dens = as.ordered(dens))

levels_text <- levels(animate_df$dens)
animate_df2 <- lapply(seq_along(levels),
       function(a){
         result <- animate_df |>
           mutate(timestep = a,
                  size = as.integer(dens))
         result$size[result$size > a] <- a
         result
         }) |>
  bind_rows()

x <- ggplot(animate_df2, 
       aes(x = longitude, 
           y = latitude,
           color = size,
           size = size,
           group = id)) + 
  geom_point() + 
  scale_size(range = c(0.5, 4)) +
  scale_color_gradient(low = "#ffffff", high = "#E0AA51") +
  transition_states(timestep, wrap = FALSE) +
  enter_grow() +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#DE4BC3"))

animate(x, renderer = gifski_renderer('./gganimate/git_test.gif'))
