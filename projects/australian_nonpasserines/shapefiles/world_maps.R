# this is an example script for plotting world maps
library(sf)
library(rnaturalearth)
library(ggplot2)

# download a file from rnaturalearth
filename <- ne_download(scale = 10, 
                        type = "countries",
                        returnclass = "sf",
                        destdir = "shapefiles",
                        load = FALSE)

# import downloaded shapefile
shapefile <- paste0("shapefiles/", filename, ".shp") |>
             st_read()

# plot whole world for example purposes
ggplot() +
  geom_sf(data = shapefile)

# plot a subset
ggplot() +
  geom_sf(data = shapefile, fill = "#98bf93") +
  lims(x = c(130, 155), y = c(-45, -30)) +
  theme_bw()

ggsave("example_plot.png")
