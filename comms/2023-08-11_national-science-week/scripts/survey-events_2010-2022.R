library(tidyverse)
library(arrow)
library(sf)
library(rmapshaper)
library(ozmaps)
library(showtext)
library(magick)
library(here)

ibra_sf <- readRDS(here("comms", 
                        "2023-08-11_national-science-week", 
                        "data", 
                        "ibra_sf.rds"))

# NOTE: This is the monitoring dataset from EcoAssets
# Download the dataset here: 
# https://ecoassets.org.au/data/aggregated-data-environmental-monitoring-and-observations-effort/
events <- open_dataset(here("comms", 
                            "2023-08-11_national-science-week", 
                            "large-data", 
                            "monitoring.csv"), format = "csv")

plot_choropleth <- function(df) {
  
  each_year <- unique(df$year)
  
  ggplot() +
    geom_sf(data = ibra_sf, colour = "#232F0B", fill = NA) +
    geom_sf(data = df,
            aes(fill = cutCounts),
            colour = NA) +
    scale_fill_manual(name = NULL, 
                      drop = FALSE,
                      labels = c("0.2", "0.4", "0.6", "0.8"),
                      # 5-class YlOrBr
                      values = c("#B9CA98", "#92AC5D", "#6B8E23", "#4E6819", "#314210"),
                      na.value = "#efefef",
                      guide = guide_colorsteps(
                        direction = "horizontal",
                        label.position = "bottom",
                        title.position = "left",
                        title.vjust = 0.8, 
                        title.hjust = 0.8)) +
    annotate("text", 
             x = 133,
             y = -45.5,
             label = str_glue("Biological monitoring events: {each_year}"),
             size = 8, family = "poppins", colour = "#273b50") +
    coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.key.width = unit(25, 'mm'),
          legend.text = element_text(size = 6),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA)) 
}

ibra_year <- ibra_sf |> 
  as_tibble() |> 
  expand(ibraRegion, 2010:2022) |> 
  rename(year = `2010:2022`)

ibra_biol_class  <- events |> 
  filter(featureFacet1 == "Biological Classification", 
         ibraRegion != "NA") |> 
  count(year, ibraRegion) |> 
  collect() |> 
  full_join(ibra_sf, by = join_by(ibraRegion)) |> 
  rowwise() |> 
  mutate(propCounts = log10(`n`/area),
         cutCounts = cut(propCounts, 
                         breaks = c(-6, -4, -3, -2, -1, 0),
                         labels = c(0.2, 0.4, 0.6, 0.8, 1.0),
                         include.lowest = TRUE)) |> 
  right_join(ibra_year, by = join_by(year, ibraRegion)) |> 
  st_as_sf(sf_column_name = "geometry") |>  
  group_by(year) |> 
  group_split(year) %>% 
  set_names(map(., ~.x$year[1]))

all_plots <- map(ibra_biol_class, plot_choropleth)

plotnames <- map(names(all_plots), ~ paste0("images/monitoring_", ., ".png")) 

walk2(plotnames, all_plots, ~ggsave(filename = .x, 
                                    plot = .y, 
                                    height = 10, 
                                    width = 10, 
                                    units = "in"))

list.files(path = "./images", pattern = "^monitoring_.*png$", full.names = TRUE) |>
  map(image_read) |>  # reads each path file
  image_join() |>  # joins image
  image_animate(delay = 100, optimize = TRUE) |>  # animates, can opt for number of loops
  image_write("./images/monitoring.gif")

