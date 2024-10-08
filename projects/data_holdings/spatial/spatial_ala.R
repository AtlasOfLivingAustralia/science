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
library(tidyverse)
library(ozmaps)
library(sf)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(mapsf)
library(viridis)
# devtools::install_github("ropensci/rnaturalearthhires")
# install.packages("rgeos")


#-------------------------------------#
#           Country counts
#-------------------------------------#

# Get data

search_fields("countryCode") # id = "countryCode"
counts_by_country <- ala_counts(group_by = "countryCode", limit = 20) # top 20 of >300

#| so it looks like a lot of people use this as an open answer rather than a category
#| There is also a duplicate of "AU" & "Australia"
str(counts_by_country)

# Excluding the mammoth amount of AU data, the countries with the most records are:
ggplot(counts_by_country %>% filter(countryCode != "AU")) + 
  geom_bar(aes(x = count, y = reorder(countryCode, count)),
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
  geom_bar(aes(x = "", y = count, fill = biome), stat = "identity") + 
  coord_polar("y") + 
  theme_void() + scale_fill_viridis_d(direction = -1)



#-------------------------------------#
#       Map with globe inset
#-------------------------------------#


# Might need:
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

par(mfrow=c(1,1))
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
path <- "C:/Users/KEL329/OneDrive - CSIRO/Documents/ALA/Projects/Data Holdings/data_ALA"
data_filepath <- file.path(path, "egrets.rds")
egrets <- readRDS(file=data_filepath)

egrets <- egrets %>% filter(!is.na(.))
dt_egrets <- setDT(egrets) # convert to data.table
dt_egrets <- egrets[1:10000,.(decimalLatitude, decimalLongitude)] # subset first 1000 values




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
egret.densities <- vmf_density_grid(dt_egrets[1:2000,c("decimalLatitude",
                                                       "decimalLongitude")],
                                    ngrid = grid.size)

# Plot
dens_worldmap <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  geom_point(data = dt_egrets,
             mapping = aes(x = decimalLongitude, y = decimalLatitude),
             color = "light blue", alpha = .2, size = .5, stroke = 0) + 
  geom_contour(data = egret.densities, aes(x=Long, y=Lat, z=Density), colour = "#DC5E41") + # this works
  # geom_contour_filled(data = egret.densities, aes(x=Long, y=Lat, z=Density)) + # this doesn't work
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(-20, 135, 0)) + 
  theme_minimal() +
  theme(legend.position = "none")

dens_worldmap

# Note that we would prefer the contours to be filled but I am getting an error
# Something about vectors not liking what I have at the moment

library(MASS)

#----------------------------------#
#           2d density map
#----------------------------------#



# 2 different ways to calculate densities

#------ first way

# Calculate density
dgrid <- dt_egrets %>% 
  with( 
    MASS::kde2d(decimalLongitude, decimalLatitude, n = 101,
                lims = c(
                  scales::expand_range(range(decimalLongitude), .20),
                  scales::expand_range(range(decimalLatitude), .20)
                )
    )
  )

# make into a data frame
edens_df <- dgrid %>% 
  .[c("x", "y")] %>% 
  cross_df() %>% 
  rename("lon" = "x", "lat" = "y") %>% 
  mutate(density = as.vector(dgrid$z)) 

# plot
plot_2d_dens <- ggplot() + 
  geom_sf(data = ozmap_states, fill = "grey90", color = "grey50") + 
  geom_point(data = dt_egrets,
             mapping = aes(x = decimalLongitude, y = decimalLatitude),
             color = "#E06E53", alpha = .3, size = .9, stroke = 0) +
  geom_contour_filled(data = edens_df,
              mapping = aes(x = lon, y = lat, z = density), alpha = .5, bins = 5) +
  scale_fill_manual(values = c(NA,"#ECA798","#E37B64","#DC5E41","#CF4526")) +
  theme_bw() + 
  theme(legend.position = "none")

plot_2d_dens


#------ second way

# plot
plot_2d_dens <- ggplot() + 
  geom_sf(data = ozmap_states, fill = "grey90", color = "grey50") + 
  geom_point(data = dt_egrets,
             mapping = aes(x = decimalLongitude, y = decimalLatitude),
             color = "#E06E53", alpha = .3, size = .9, stroke = 0) +
  stat_density2d_filled(data = dt_egrets, 
                        mapping = aes(x = decimalLongitude, y = decimalLatitude, fill = ..level..), 
                        alpha = .5, geom = "polygon", bins = 5) + 
  scale_fill_manual(values = c(NA,"#ECA798","#E37B64","#DC5E41","#CF4526")) +
  theme_bw() + 
  theme(legend.position = "none")

plot_2d_dens




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


ggplot(spatial_df) + geom_sf(data = ozmap_states) + geom_point(aes(x = Lon, y = Lat), colour = "red")

str(spatial_df)

#----------------------------------------------#
#                   spline                     #
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
    return(length(which(egrets$decimalLongitude == a | egrets$decimalLatitude == a)))
  }))


# model using GAMs
library(mgcv)
spatial_df$Lon_scaled <- scale(spatial_df$Lon)
spatial_df$Lat_scaled <- scale(spatial_df$Lat)



#----------------WARNING: This takes between 20 and 30 minutes to run------------------------#
# model of lat, long w/ covariance specified by m
# model <- gam(record_count ~ s(Lon, Lat, k = 50, m = 2), # k likely needs to be bigger
#              data = spatial_df,
#              family = poisson(link = "log"))
# saveRDS(model, "spatial_gam_ala.rds") # save model

# Load file from local computer
path <- "C:/Users/KEL329/OneDrive - CSIRO/Documents/ALA/Projects/Data Holdings/output"
data_filepath <- file.path(path, "spatial_gam_ala.rds")
model <- readRDS(file=data_filepath) # load model
#--------------------------------------------------------------------------------------------#


# make predictions
# first calculate grid for 2d interpolation
Lon_vector <- seq(
  min(spatial_df$Lon),
  max(spatial_df$Lon),
  by = 3) # resolution

Lat_vector <-  seq(
  min(spatial_df$Lat),
  max(spatial_df$Lat),
  by = 3) # resolution

prediction_surface <- expand.grid(
  Lon = Lon_vector,
  Lat = Lat_vector)

# Predict (interpolate values)
model_prediction <- predict(model, newdata = prediction_surface, se.fit = FALSE)
prediction_surface$fit <- as.numeric(model_prediction)



# Plot
ggplot() + 
  geom_sf(data = world, fill= "gray90") +
  coord_sf(xlim = c(100.00, 180.00), ylim = c(-50.00, -10.00), expand = FALSE) +
  geom_tile(data = prediction_surface, 
            mapping = aes(x = Lon, y = Lat, fill = fit), alpha = .4) + 
  
  stat_contour(aes(x = Lon, y = Lat, z = fit, fill = ..level..), data = prediction_surface, geom = 'polygon', alpha = .4) +
  geom_contour(aes(x = Lon, y = Lat, z = fit), data = prediction_surface, colour = 'white', alpha = .4) +
  scale_fill_viridis() +
  theme_bw()





#-----------------------------------------------------------------------------#
# From https://stackoverflow.com/questions/28059182/smoothing-out-ggplot2-map
#-----------------------------------------------------------------------------#
library(raster)
au <- st_as_sf(raster::getData('GADM', country='AUS', level=1))
head(au)



library(tmap)
map_aus <- tm_shape(au) + tm_polygons()


library(data.table)
library(ggplot2)
library(automap)
library(tidyverse)
library(akima)


# Data munging
oz_states_df <- fortify(ozmap_states)
box <- st_bbox(ozmap_states) # Get boundaries of map


# Do spline interpolation with the akima package
fld = with(edens_df, interp(x = lon, y = lat, z = density, duplicate="median",
                            xo=seq(box$xmin, box$xmax, length = 100),
                            yo=seq(box$ymin, box$ymax, length = 100),
                            # xo=seq(1.02*min(au_map$long), max(au_map$long), length = 100),
                            # yo=seq(0.96*min(au_map$lat), max(au_map$lat), length = 100),
                            extrap=TRUE, linear=FALSE))
melt_x = rep(fld$x, times=length(fld$y))
melt_y = rep(fld$y, each=length(fld$x))
melt_z = as.vector(fld$z)
level_data = data.frame(longitude=melt_x, latitude=melt_y, APPT=melt_z)
interp_data = na.omit(level_data)



ggplot() + geom_sf(data = ozmap_states) + 
  geom_tile(data = interp_data, 
            mapping = aes(x = longitude, y = latitude, fill = APPT), alpha = .6) + 
  stat_contour(data = interp_data, 
               mapping = aes(x = longitude, y = latitude, z = APPT)) + 
  scale_fill_viridis(option = "turbo")



str(interp_data)
