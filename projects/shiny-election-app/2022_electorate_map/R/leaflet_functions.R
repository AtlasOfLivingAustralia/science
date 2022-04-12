# functions: park in .R folder later on
leaflet_update <- function(df){
  leafletProxy("mymap") |>
    clearShapes() |>
    addPolygons(
      data = df,
      layerId = df$Elect_div,
      color = "#B7CD96",
      fillColor = "#B7CD96",
      fillOpacity = 0.6,
      weight = 3,
      label = lapply(df$label, HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", 
                     padding = "3px 5px"),
        textsize = "12px",
        direction = "auto")
    )
}

leaflet_reset <- function(){
  leafletProxy("mymap") |>
    clearShapes() |>
    addPolygons(
      data = x,
      layerId = x$Elect_div,
      fillColor = "white",
      fillOpacity = .2, 
      color = "#111111", 
      weight = 1, 
      stroke = TRUE,
      highlightOptions = highlightOptions(
        color = "#B7CD96", 
        fillColor = "#B7CD96", 
        fillOpacity = 0.6,
        weight = 3,
        bringToFront = TRUE),
      label = lapply(x$label, HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", 
                     padding = "3px 5px"),
        textsize = "12px",
        direction = "auto")
    )
}

