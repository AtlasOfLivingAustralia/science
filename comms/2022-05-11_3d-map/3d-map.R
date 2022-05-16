#------------------------------------#
# ALA records 3d map
# Author: Dax Kellie
# Date: 2022-05-16
#------------------------------------#

# packages

library(galah)
library(tidyverse)
library(here)
library(rayshader)
library(showtext)
library(sf)
library(ozmaps)
library(scales)
library(ggnewscale)
library(viridis)


# get a map and project to WGS84
oz_wgs84 <- ozmap_data(data = "country") |>
  st_transform(crs = st_crs("WGS84"))

## check map
ggplot(oz_wgs84) + geom_sf()

# create grid
oz_grid <- st_make_grid(oz_wgs84,
                        what = "polygons",
                        cellsize = 1.0,
                        square = FALSE,
                        flat_topped = TRUE)

# subset to grid cells that are within land
keep_hexes <- st_intersects(oz_grid, oz_wgs84)
keep_hexes <- as.data.frame(keep_hexes)$row.id
oz_grid <- oz_grid[keep_hexes]

## check
ggplot() +
  geom_sf(data = oz_wgs84) +
  geom_sf(data = oz_grid, fill = NA, color = "red")


# calculate counts for every hex
# | NOTE:
# | Uncomment the galah_identify() line for the query you require

counts_list <- lapply(
  seq_along(oz_grid), function(a){
    # convert to wkt
    wkt_string <- st_as_text(oz_grid[[a]]) %>%
      sub(")))", "))", .) %>%
      sub("POLYGON ", "POLYGON", .)
    # get counts
    result <- galah_call() |>
      galah_geolocate(wkt_string) |>
      galah_filter(profile = "ALA",
                   decimalLongitude > 110,
                   year >= 2020) |>
      # galah_identify("animalia") |>               # animals
      # galah_identify("plantae", "chlorophyta") |> # plants
      galah_identify("fungi") |>                  # fungi
      atlas_counts(type = "species", 
                   limit = NULL)
    # light formatting to catch errors
    if(is.null(result)){
      tibble(count = NA, id = a)
    }else{
      result$id <- a
      result
    }
  }
)


# bind lists to data frame
counts_df <- map_dfr(counts_list, rbind)


# convert to tibble, attach counts
oz_df <- st_as_sf(oz_grid)
oz_df$count <- NA
oz_df$count[counts_df$id] <- counts_df$count


# See top hexagons
counts_df %>%
  arrange(desc(count))



## -- Plot -- ##

fungi_hex_map <- ggplot() +
  geom_sf(
    data = oz_df,
    mapping = aes(fill = log10(count+1)),
    # mapping = aes(fill = count),
    alpha = 1,
    color = NA) +
  scale_fill_distiller(name = "Number of species \n(since 1 Jan, 2020)",
                       type = "seq",
                       direction = 1,
                       limits = c(0,4),
                       labels = c("10", "100", "1,000"),
                       # palette = "Oranges",             # animals
                       # palette = "Greens",              # plants
                       palette = "PuRd",                # fungi
                       # labels = c("10", "100", "1,000", "10,000", "100,000"), # all
                       guide = guide_colorsteps(direction = "horizontal",
                                                label.position = "top",
                                                title.position = "bottom",
                                                title.hjust = 0.5)
                       ) +
  geom_sf(data = oz_wgs84,
          color = NA,
          fill = NA)  +
  coord_sf(xlim = c(110, 155), 
           ylim = c(-45, -10)) + 
  pilot::annotate_pilot(x = 124, y = -40,
                        label = "Fungi",     # change annotation name
                        size = 8) +
  theme(text = element_text(family = "lato"),
        title = element_text(face = "bold"),
        legend.title = element_text(size = 19, family = "lato"),
        legend.position = "bottom",
        legend.key.width = unit(28, 'mm'),
        legend.text = element_text(size = 16, family = "lato"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.title = element_blank()
        )

fungi_hex_map


# Render 3d plot and save
plot_gg(fungi_hex_map, width = 9, height = 8, multicore = TRUE, scale = 300, raytrace = FALSE, windowsize = c(1200, 960),
        fov = 75, zoom = 0.37, theta = 320, phi = 33, max_error = 0.001, verbose = TRUE)
# (See note at bottom if you are getting an error related to the scales package)


# save
Sys.sleep(0.2)
render_snapshot(here("comms", "2022-05-11_3d-map", "fungi_3d.png"))


# save 2d plot
ggsave(here::here("comms", "2022-05-11_3d-map", "fungi_2d.png"),
       hex_map, height = 8, width = 9, units = "in", device='png')





# NOTE: 
# if there is a weird scales error, reinstalling all the "ray" functions fixes it:

# options(repos = c(
#   tylermorganwall = 'https://tylermorganwall.r-universe.dev',
#   CRAN = 'https://cloud.r-project.org'))
# install.packages('rayshader') # Install some packages

