#azlin's code
library(tidygeocoder)
library(tidyverse)
data <- read.csv("eudyptula_minor_azlin_adele_complete.csv")

result1 <- geocode(data, state = "state", country = "country") #returns centre of state
result2 <- geocode(data, state = "state", country = "country", limit = 1) #limit should only record each location once?
result3 <- geocode(data, address = "location") #promising
result4 <- geocode(data, street = "location", state = "state", country = "country", limit = 1)
result5 <- geocode(data, street = "location", state = "state", country = "country", limit = 1, unique_only = TRUE) #each location appears only once 
result6 <- geocode(data, street = "location", state = "state", country = "country", unique_only = TRUE)
result7 <- geocode(data, city = "location", state = "state", country = "country", unique_only = TRUE)



result6 <- geocode(data, street = "location", state = "state", country = "country", unique_only = TRUE)
result7 <- geocode(data1, city = "location", state = "state", country = "country", unique_only = TRUE)
data1 <- na.omit(data)

library(ggplot2)
install.packages("maps") #required
library(maps)

??maps
help(package='maps')

ggplot(result7, aes(long, lat)) +
  borders("world") + geom_point() #plots points on world map - some are in North America


ggplot(result7, aes(long, lat)) +
  geom_point() +
  maps::map(regions = "Australia")
  
  
  maps::map(regions = "Australia")

map("world")
map(regions = "Australia") #maps Australia

#https://ggplot2-book.org/maps.html

install.packages("ozmaps")
library(ozmaps)
?ozmaps

oz_states <- ozmaps::ozmap_states #object for state borders

#map australia
ggplot(oz_states) + 
  geom_sf() + 
  coord_sf()


#map df object from "cacatua_sang.qmd"
ggplot() + 
  geom_sf(data = oz_states, colour = "black", fill = NA) + 
  geom_point(data = df, mapping = aes(x = lon, y = lat), colour = "red") + 
  coord_sf()


  ?geom_point
