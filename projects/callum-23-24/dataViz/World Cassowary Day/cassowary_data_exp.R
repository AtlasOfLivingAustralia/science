##### Exploring Cassowary data in the ALA #####

##### Libraries #####
{
  library(galah)
  library(hms)
  library(lubridate)
  library(ozmaps)  
  library(sf)
  library(sfheaders)
  library(tidyverse)
  library(mapdeck)
  library(ggrepel)
}

galah_config(email = "callumwaite2000@gmail.com", verbose = FALSE)
mapdeck_key <- "pk.eyJ1IjoiY2p3YWl0ZTIzIiwiYSI6ImNsbDRvOHF4YTA3ZWMzZXFqdDd5NHV6djMifQ.isjAdjBb5gp27lVHLXTZMw"

##### Coastal Waters shpfile #####
CWA <- st_read("C:/Users/WAI045/OneDrive - CSIRO/ALA/Biosecurity/shapefiles/CWA1980_zones/Coastal_Waters_AMB2020_Areas.shp")

coastal_waters_shp <- CWA |>
  rbind(
    CWA |>
      filter(COMMENT == "New South Wales") |>
      mutate(geometry = st_difference(sf_remove_holes(geometry),
                                      geometry),
             COMMENT = "Australian Capital Territory",
             NAME = "Coastal Waters (State Powers) Act 1980 - AMB2020 - Area - Australian Capital Territory",
             OBJNAM = NA,
             MRN = NA,
             Shape_Leng = NA, #doesn't work for now
             Shape_Area = NA) #different units to rest of the column
  ) |>
  dplyr::select(COMMENT, geometry) |>
  rename(state_long = COMMENT)

##### Data Exploration #####
cassowary <- galah_call() |>
  galah_identify("Casuarius casuarius") |>
  galah_select(group = c("basic", "media"), cl932, cl22, cl1048, cl21,
               el674,
               species, subspecies,
               year, month, day) |>
  atlas_occurrences()

cassowary_cleaned <- cassowary |>
  # Clean out incomplete data
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude) & !is.na(eventDate)) |>
  # keep records past 1950
  filter(eventDate > dmy("1/1/1950")) |>
  # Only keep records in QLD
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs(coastal_waters_shp),
           remove = FALSE) |>
  mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
           as.integer(),
         cw_state = if_else(is.na(intersection),
                            NA,
                            coastal_waters_shp$state_long[intersection])) |>
  select(-intersection) |>
  filter(cw_state == "Queensland") |>
  # only keep records above 20 degrees S
  filter(decimalLatitude > -20) |>
  # Create eventTime column
  mutate(eventDate = if_else(dataResourceName == "iNaturalist Australia",
                            with_tz(eventDate, "Australia/Brisbane"),
                            force_tz(eventDate, "Australia/Brisbane"))) |>
  mutate(eventTime = as_hms(eventDate)) |>
  # Create eventDay column
  mutate(eventDay = format(eventDate, "%d-%m")) |>
  # Split into northern and southern populations
  mutate(northern_pop = (decimalLatitude > -14.5))

north_qld_cities <- tibble(
  town = c("Townsville", "Cairns", "Cooktown"),
  decimalLatitude = c(-19.2591042, -16.9202208, -15.4757629),
  decimalLongitude = c(146.8168224, 145.7706891, 145.2470796)
)

ggplot(cassowary_cleaned) +
  geom_point(aes(x = eventDay, y = el674, col = eventTime)) +
  theme_bw()

##### Data viz #####
qld_1 <- st_read("C:/Users/WAI045/OneDrive - CSIRO/ALA/Shapefiles/QLD_STATE_POLYGON_shp/QLD_STATE_POLYGON_shp.shp")

mapdeck(token = mapdeck_key, style = mapdeck_style("outdoors")) %>%
  add_scatterplot(data = cassowary_cleaned,
                  lat = "decimalLatitude", lon = "decimalLongitude",
                  fill_colour = "el674",
                  radius = 100, fill_opacity = 100)
htmlwidgets::saveWidget(widget = occurrence_map, file = paste0(cache_path, "maps/", list_row$recordID, ".html"))

map1 <- ggplot() +
  theme_void() +
  # QLD outline
  geom_sf(data = qld_1, 
          col = "white", fill = "grey10") +
  # Cassowary points
  geom_point(data = cassowary_cleaned,
             aes(x = decimalLongitude, y = decimalLatitude, col = northern_pop), 
             alpha = 0.4, size = 4) +
  # Cities
  geom_text_repel(data = north_qld_cities,
                  aes(x = decimalLongitude, y = decimalLatitude, label = town),
                  col = "white", size = 4,
                  nudge_x = 0.7,
                  nudge_y = c(0.3, 0, 0),
                  segment.colour = NA) +
  scale_colour_manual(values = c("red", "blue")) +
  theme(panel.background = element_rect(fill='grey10'),
        legend.position = "none") +
  xlim(c(140, 148)) + ylim(c(-20, -9))

ggsave("C:/Users/WAI045/OneDrive - CSIRO/ALA/dataViz/World Cassowary Day/map1.svg",
       map1, height = 8, width = 5, units = "in")



##### Large fruit species #####
fruit_species <- c("Monoon michaelii", "Calophyllum costatum", "Corynocarpus cribbianus", 
                   "Elaeocarpus stellaris", "Beilschmiedia oligandra", "Beilschmiedia volckii", 
                   "Cryptocarya oblata", "Cryptocarya pleurosperma", "Endiandra montana", 
                   "Endiandra sankeyana", "Syzygium divaricatum", "Syzygium cormiflorum", 
                   "Castanospora alphandii", "Faradaya splendida", "Cerbera floribunda")
fruit_occurrences <- galah_call() |>
  galah_identify(fruit_species) |>
  galah_select(group = c("basic", "media"), cl932, cl22, cl1048, cl21,
               el674,
               species, subspecies, vernacularName,
               year, month, day) |>
  atlas_occurrences()

fruit_occurrences_cleaned <- fruit_occurrences |>
  # Clean out incomplete data
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude) & !is.na(eventDate)) |>
  # keep records past 1950
  filter(eventDate > dmy("1/1/1950")) |>
  # Only keep records in QLD
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs(coastal_waters_shp),
           remove = FALSE) |>
  mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
           as.integer(),
         cw_state = if_else(is.na(intersection),
                            NA,
                            coastal_waters_shp$state_long[intersection])) |>
  select(-intersection) |>
  filter(cw_state == "Queensland") |>
  # only keep records above 20 degrees S
  filter(decimalLatitude > -20) |>
  # Create eventTime column
  mutate(eventDate = if_else(dataResourceName == "iNaturalist Australia",
                             with_tz(eventDate, "Australia/Brisbane"),
                             force_tz(eventDate, "Australia/Brisbane"))) |>
  mutate(eventTime = as_hms(eventDate)) |>
  # Create eventDay column
  mutate(eventDay = format(eventDate, "%d-%m"))

map2 <- ggplot() +
  theme_void() +
  # QLD outline
  geom_sf(data = qld_1, 
          col = "white", fill = "grey10") +
  # Cassowary points
  geom_point(data = cassowary_cleaned,
             aes(x = decimalLongitude, y = decimalLatitude), 
             col = "red", alpha = 0.4, size = 4) +
  # Fruit points
  geom_point(data = fruit_occurrences_cleaned,
             aes(x = decimalLongitude, y = decimalLatitude, col = species),
             alpha = 0.4, size = 2) +
  # Cities
  geom_text_repel(data = north_qld_cities,
                  aes(x = decimalLongitude, y = decimalLatitude, label = town),
                  col = "white", size = 4,
                  nudge_x = 0.7,
                  nudge_y = c(0.3, 0, 0),
                  segment.colour = NA) +
  #scale_colour_manual(values = c("red", "blue")) +
  theme(panel.background = element_rect(fill='grey10')) +
  xlim(c(140, 148)) + ylim(c(-20, -9))

ggsave("C:/Users/WAI045/OneDrive - CSIRO/ALA/dataViz/World Cassowary Day/map2.pdf",
       map2, height = 8, width = 7, units = "in")


##### Shapefiles #####
aus <- ozmap_data(data = "states")
###### QLD STATE POLYGON ######
qld_1 <- st_read("C:/Users/WAI045/OneDrive - CSIRO/ALA/Shapefiles/QLD_STATE_POLYGON_shp/QLD_STATE_POLYGON_shp.shp")

qld_1_plot <- ggplot() +
  geom_sf(data = aus[3,], fill = "lightyellow", alpha = 0.3) +
  geom_sf(data = qld_1, fill = "steelblue3", alpha = 0.7) +
  theme_void()

ggsave(filename = "C:/Users/WAI045/OneDrive - CSIRO/ALA/Shapefiles/QLD_STATE_POLYGON_shp/QLD_STATE_POLYGON_shp.pdf",
       plot = qld_1_plot)

###### QLD STATE POLYGON ######
qld_2 <- st_read("C:/Users/WAI045/OneDrive - CSIRO/ALA/Shapefiles/QLD_STATE_POLYGON_shp_GDA2020/QLD_STATE_POLYGON_shp_GDA2020.shp")

qld_2_plot <- ggplot() +
  geom_sf(data = aus[3,], fill = "lightyellow", alpha = 0.3) +
  geom_sf(data = qld_2, fill = "steelblue3", alpha = 0.7) +
  theme_void()

ggsave(filename = "C:/Users/WAI045/OneDrive - CSIRO/ALA/Shapefiles/QLD_STATE_POLYGON_shp_GDA2020/QLD_STATE_POLYGON_shp_GDA2020.pdf",
       plot = qld_2_plot)
