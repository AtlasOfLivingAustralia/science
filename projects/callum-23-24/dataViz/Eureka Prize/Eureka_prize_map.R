# maps for Eureka Prize

library(galah)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(ggrepel)

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(sovereignt != "Antarctica")

atlases <- show_all_atlases()
atlases$region[6] <- "Denmark"

# Manually add GBIF Voting Participants and Associate country participants (nations):
# https://www.gbif.org/the-gbif-network
gbif_nodes <- c(
  "Andorra", "Angola", "Argentina",
  "Armenia", "Australia", "Belarus",
  "Belgium", "Benin", "Brazil",
  "Cambodia", "Cameroon", "Canada",
  "Central African Republic", "Chile", "Colombia",
  "Costa Rica", "Croatia", "Denmark", 
  "Ecuador", "Estonia", "Finland",
  "France", "Georgia", "Germany",
  "Guatemala", "Guinea", "Iceland",
  "Ireland", "Kenya", "South Korea",
  "Liberia", "Luxembourg", "Madagascar",
  "Malawi", "Mauritania", "Mexico",
  "Namibia", "Netherlands", "New Zealand",
  "Nigeria", "Norway", "Peru",
  "Poland", "Portugal", "Sierra Leone",
  "Slovakia", "Slovenia", "South Africa",
  "South Sudan", "Spain", "Suriname",
  "Sweden", "Switzerland", "Tajikistan",
  "Tanzania", "Timor-Leste", "Togo",
  "Tonga", "United Kingdom", "United States of America",
  "Uruguay", "Uzbekistan", "Zimbabwe")


world$group <- 1 # "other"
selected_nodes <- world$name_en %in% gbif_nodes
# gbif_nodes[!(gbif_nodes %in% selected_places)] # Timor-Leste missing
world$group[selected_nodes] <- 2 # "GBIF nodes"
world$group[world$name_en %in% atlases$region] <- 3 # "Supported by galah"
world$group <- factor(world$group, 
                      levels = seq_len(3),
                      labels = c("Not supported", "GBIF nodes", "Supported nodes"))

p <- ggplot(data = world) +
  geom_sf(
    mapping = aes(fill = group),
    color = NA
  ) +
  geom_text_repel(
    data = world[which(world$highlight), ],
    mapping = aes(x = label_x, y = label_y, label = name_en)
  ) +
  scale_fill_manual(values = c("grey80", "grey60", "#2b8f38")) +
  theme_void() +
  theme(legend.position = "none")
ggsave("atlas_map_v1.png", p, width = 8, height = 3.5, units = "in")

# OK Global isn't going to work. Cutouts?
# add colors for countries with atlases?
# add lines to GBIF?

# API calls for GBIF map data:
# https://api.gbif.org/v2/map/occurrence/density/0/0/0@2x.png?srs=EPSG:4326&style=purpleYellow.point
# https://api.gbif.org/v2/map/occurrence/density/0/1/0@2x.png?srs=EPSG:4326&style=purpleYellow.point