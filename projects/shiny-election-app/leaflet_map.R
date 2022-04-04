
library(leaflet)
map_data <- readRDS("2022_electorate_map/spatial_data.rds")

leaflet_map <- leaflet() |> 
  fitBounds(112, -45, 154, -10) |>
  addProviderTiles(providers$CartoDB.Positron) |>  
  addPolygons(
    data = map_data,
    layerId = map_data$Elect_div,
    fillColor = "white",
    fillOpacity = .2, 
    color = "#111111", 
    weight = 1, 
    stroke = TRUE,
    highlightOptions = highlightOptions(
      color = "#C44D34", 
      fillColor = "#C44D34", 
      fillOpacity = 0.6,
      weight = 3,
      bringToFront = TRUE),
    label = map_data$Elect_div,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", 
                   padding = "3px 5px"),
      textsize = "12px",
      direction = "auto")
  )

saveRDS(leaflet_map, "2022_electorate_map/leaflet_map.rds")
