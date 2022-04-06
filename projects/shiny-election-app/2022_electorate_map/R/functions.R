# functions: park in .R folder later on
leaflet_update <- function(df){
  leafletProxy("mymap") |>
    clearShapes() |>
    addPolygons(
      data = df,
      layerId = df$Elect_div,
      color = "#C44D34",
      fillColor = "#C44D34",
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
      data = spatial_data,
      layerId = spatial_data$Elect_div,
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
      label = lapply(spatial_data$label, HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", 
                     padding = "3px 5px"),
        textsize = "12px",
        direction = "auto")
    )
}

circle_icon <- function(id, image_path){
  tags$a(
    id = id,
    href = "#",
    class = "action-button shiny-bound-input",
    tags$div(class = "clear_circle",
      tags$image(class = "taxon-outline", src = image_path)
    )
  )
}