## ------------------------------------------------------#
## Title: Occurrence Counts by Country and Egrets Maps
## Author: Dax Kellie
## Date Created: 2021-05-4
## ------------------------------------------------------#

rm(list = ls())

# packages

# remotes::install_github("AtlasOfLivingAustralia/galah") # installs latest version
library(galah)
library(data.table)
library(ggplot2)
library(ozmaps)
library(sf)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(mapsf)

#-------------------------------------#
#           Country counts
#-------------------------------------#

# Get data

search_fields("countryCode") # id = "countryCode"
counts_by_country <- ala_counts(group_by = "countryCode", limit = 20) # top 20 of >300

#| so it looks like a lot of people use this as an open answer rather than a category
#| There is also a duplicate of "AU" & "Australia"


# Excluding the mammoth amount of AU data, the countries with the most records are:
ggplot(counts_by_country %>% filter(name != "AU")) + 
  geom_bar(aes(x = count, y = reorder(name, count)),
           stat = "identity", fill = "forestgreen") + 
  labs(y = "Country") + 
  theme_minimal() + theme(legend.position = "none")



#-------------------------------------#
#         Environment counts
#-------------------------------------#


# Differences in terrestrial vs marine counts

search_fields("marine") # id = "countryCode", id = "biome"
counts_by_environment <- ala_counts(group_by = "biome", limit = 100)

ggplot(counts_by_environment) + 
  geom_bar(aes(x = "", y = count, fill = name), stat = "identity") + 
  coord_polar("y") + 
  theme_void() + scale_fill_viridis_d(direction = -1)



#-------------------------------------#
#       Map with globe inset
#-------------------------------------#


# Might need to install:
# devtools::install_github('ropensci/rnaturalearthhires') 
# install.packages("rgeos")

aus_ne <- rnaturalearth::ne_states(country = "australia", returnclass = "sf")

mf_map(aus_ne)


# Get data

# Might need:
# ala_config(email = "dax.kellie@csiro.au")


# Get Intermediate Egret data as example data
#   (a bird that, compared to the great egret, was only determined to be intermediate...)

# Counts of Egrets by state
state_layers <- search_fields("australian states and territories") # id = "cl22"
egret_by_state <- ala_counts(taxa = select_taxa("Ardea intermedia"), group_by = "cl22")

# join with data from {ozmaps}
state_map <- merge(
  ozmaps::ozmap_states,
  egret_by_state,
  by.x = "NAME",
  by.y = "cl22"
)

state_map <- st_transform(state_map, crs = st_crs(3577))

?ozmaps
# Map using mf_map
mf_map(x = state_map, var = "count", type = "choro",
       pal = "Dark Mint")

# Start an inset map
mf_inset_on(x = "worldmap", pos = "topleft")
# Plot the position of the sample dataset on a worldmap
mf_worldmap(state_map, col = "#0E3F5C")
# Close the inset
mf_inset_off()


# source
# https://riatelab.github.io/mapsf/

 



#----------------------------------#
#           Spherical map
#----------------------------------#


# Get Intermediate Egret occurences data
# egrets <- ala_occurrences(taxa = select_taxa("Ardea intermedia"))

# Call saved local data file
path <- "C:/Users/KEL329/Documents/Projects/Data Holdings/data_ALA"
data_filepath <- file.path(path, "egrets.rds")
egrets <- readRDS(file=data_filepath)


dt_egrets <- setDT(egrets) # convert to data.table
dt_egrets <- egrets[1:1000,.(decimalLatitude, decimalLongitude)] # subset first 1000 values



# Below makes a spherical map focused on AU

library(rworldmap)
library(geosphere)
library(gpclib)
library(Directional) # for spherical density functions
library(rgdal) # for coordinate transforms
library(sp) # for plotting grid images
library(sf)
library(spData) # world map
library(raster)
library(lwgeom)
library(metR)

# World map
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

# World
worldmap <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(-20, 135, 0)) + 
  theme(legend.position = "none")

worldmap




# Plot Egret density distribution on world map

# Calculate densities
# find density grid
vmf_density_grid <- function(u, ngrid = 100) {
  # Translate to (0,180) and (0,360)
  u[,1] <- u[,1] + 90
  u[,2] <- u[,2] + 180
  res <- vmf.kerncontour(u, thumb = "none", den.ret = T, full = T,
                         ngrid = ngrid)
  
  # Translate back to (-90, 90) and (-180, 180) and create a grid of
  # coordinates
  ret <- expand.grid(Lat = res$lat - 90, Long = res$long - 180)
  ret$Density <- c(res$den)
  ret
}

grid.size <- 100
egret.densities <- vmf_density_grid(dt_egrets[1:1000,c("decimalLatitude",
                                                       "decimalLongitude")],
                                    ngrid = grid.size)

# Plot
dens_worldmap <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  geom_point(data = dt_egrets,
             mapping = aes(x = decimalLongitude, y = decimalLatitude),
             color = "red", alpha = .2, size = .5, stroke = 0) + 
  geom_contour(data = egret.densities, aes(x=Long, y=Lat, z=Density)) + # this works
  # geom_contour_fill(data = egret.densities, aes(x=Long, y=Lat, z=Density)) + # this doesn't work
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(-20, 135, 0)) + 
  theme_minimal() +
  theme(legend.position = "none")

dens_worldmap

# Note that we would prefer the contours to be filled but I am getting an error
# Something about vectors not liking what I have at the moment





# Testing way to get coloured density geom onto sf map
str(dt_egrets)
egrets <- as.data.frame(egrets)
egrets_sf <- st_as_sf(dt_egrets, coords = c("x", "y"), crs = 28992)

map <- ggplot () +
  geom_sf(
    data = state_map,
    mapping = aes(fill = sqrt(count)), color = "grey30") +
  scale_fill_gradient(low = "#dbdbdb", high = "#E06E53") +
  stat_density_2d(data = dt_egrets,
                  mapping = ggplot2::aes(x = purrr::map_dbl(geometry, ~.[1]),
                                         y = purrr::map_dbl(geometry, ~.[2]),
                                         fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 0.5) +
  theme_void() +
  theme(
    legend.position = "none") 


density <- ggplot(data = dt_egrets, 
                  aes(x = decimalLongitude, y = decimalLatitude), fill = ..level..) + 
  geom_sf(
    data = state_map,
    mapping = aes(fill = sqrt(count)), color = "grey30") +
  scale_fill_gradient(low = "#dbdbdb", high = "#E06E53") +
  theme_void() +
  theme(
    legend.position = "none") + 
  stat_density2d(aes(fill = ..level..), geom = "polygon")

density + map
ggplot() + geom_density2d(data = dt_egrets,
                     mapping = aes(x = decimalLongitude, y = decimalLatitude, fill=..level..),
                     geom='polygon')




#-------------------------------#
#     Testing other methods
#-------------------------------#


# We can also make this hardcore map (using some weird matrix magic)

density_matrix <- matrix(egret.densities$Density, nrow = grid.size)
density_matrix <- t(apply(density_matrix, 2, rev))
gridVals <- data.frame(att=as.vector(density_matrix))
gt <- GridTopology(cellcentre.offset = c(-180 + 180 / grid.size,
                                         -90 + 90 / grid.size),
                   cellsize = c( 360 / grid.size, 180 / grid.size),
                   cells.dim = c(grid.size, grid.size))
sGDF <- SpatialGridDataFrame(gt,
                             data = gridVals,
                             proj = "+proj=longlat +datum=WGS84 +no_defs")

plot(sGDF)
plot(gridlines(sGDF), add = TRUE, col = "grey30", alpha = .1)
plot(st_geometry(world), add = TRUE, col = NA, border = "grey")

# str(sGDF)



# source
# http://egallic.fr/en/maps-with-r/



# Other websites that show how to do spherical density maps
# https://micah.waldste.in/external/rsphericaldensity/posts/heatmap/
# https://micah.waldste.in/blog/2018/06/introduction-to-spherical-densities-in-r/
# https://randomeffect.net/post/2021/01/05/plotting-a-spherical-distribution-in-r/


# Map Australia + NZ

# Might need to install {rnaturalearthdata} package
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# extract locations
world_points<- st_centroid(world)
# extract labels
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
# generate annotated world map
ggplot(data = world) +
  geom_sf(fill= "gray90") +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(100.00, 180.00), ylim = c(-50.00, -10.00), expand = FALSE)






#----------------------------------------------#
#                   spline
#----------------------------------------------#


# set up lon + lat data frame
spatial_df <- egrets %>% 
  dplyr::select(decimalLatitude, decimalLongitude) %>% 
  rename(., Lat = decimalLatitude, Lon = decimalLongitude)

spatial_df <- spatial_df %>% filter(!is.na(spatial_df$Lat)) #remove NA

# count records
spatial_df$record_count <- unlist(lapply(
  spatial_df$Lon,
  function(a){
    return(length(which(egrets$decimalLongitude == a)))
  }))


# model using GAMs
# install.packages("mgcv")
library(mgcv)
spatial_df$Lon_scaled <- scale(spatial_df$Lon)
spatial_df$Lat_scaled <- scale(spatial_df$Lat)


# try a model with interacting date terms
model <- gam(record_count ~ s(Lon, by = Lat, k = 30),
             data = spatial_df,
             family = poisson(link = "log"))

# make predictions
# first of change over time
Lon_vector <- seq(
  min(spatial_df$Lon),
  max(spatial_df$Lon),
  length.out = 100)


Lat_vector <-  seq(
  min(spatial_df$Lat),
  max(spatial_df$Lat),
  length.out = 100)

prediction_surface <- expand.grid(
  Lon = Lon_vector,
  Lat = Lat_vector)

model_prediction <- predict(model, newdata = prediction_surface, se.fit = FALSE)
prediction_surface$fit <- as.numeric(model_prediction)
str(model_prediction)


contours <- model_prediction %>% 
  raster::rasterToContour(levels = c(0.5, 1, 1.5)) %>% 
  st_as_sf()
str(prediction_surface)

ggplot() + 
  geom_sf(data = state_map) +
  coord_sf(crs = st_crs(3577)) +
  # geom_sf(data = depth_contours) +
  geom_tile(data = prediction_surface, 
            mapping = aes(x = Lon, y = Lat, fill = fit)) + 
  scale_fill_viridis() +
  theme_bw()
# NOPE terrible idea

# OLD
model <- gam(record_count ~ s(Lon) + s(Lat),
             data = spatial_df,
             family = poisson(link = "log"))

prediction_1 <- data.frame(
  Lon = seq(
    min(spatial_df$Lon),
    max(spatial_df$Lon),
    length.out = 100),
  Lat = 0)
prediction_1$date_unscaled <- seq(min(date_df$date), max(date_df$date), length.out = 100) # readjusting scale for plotting

model_prediction <- predict(model, newdata = prediction_1, se.fit = TRUE)
prediction_1$fit <- exp(model_prediction$fit)
prediction_1$lci <- exp(model_prediction$fit - (2 * model_prediction$se.fit))
prediction_1$uci <- exp(model_prediction$fit + (2 * model_prediction$se.fit))

# then repeat for Julian Date
prediction_2 <- data.frame(
  julian_scaled = seq(
    min(date_df$julian_scaled),
    max(date_df$julian_scaled),
    length.out = 100),
  date_scaled = 0)
prediction_2$date_unscaled <- seq(min(date_df$date_julian), max(date_df$date_julian), length.out = 100)

model_prediction <- predict(model, newdata = prediction_2, se.fit = TRUE)
prediction_2$fit <- exp(model_prediction$fit)
prediction_2$lci <- exp(model_prediction$fit - (2 * model_prediction$se.fit))
prediction_2$uci <- exp(model_prediction$fit + (2 * model_prediction$se.fit))

# finally calculate residuals from the model
date_df$residuals <- resid(model)


# draw
library(ggplot2)
library(patchwork)

a <- ggplot(prediction_1, aes(x = date_unscaled, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "#ff6e6e") +
  geom_path() +
  theme_bw() +
  labs(x = "Year", y = "Number of Records")

b <- ggplot(prediction_2, aes(x = date_unscaled, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "#6e9eff") +
  geom_path() +
  theme_bw() + 
  labs(x = "Day of year", y = "Number of Records")

c <- ggplot(date_df, aes(x = date, y = residuals)) +
  geom_path() +
  # geom_point() +
  theme_bw()

a / b / c
