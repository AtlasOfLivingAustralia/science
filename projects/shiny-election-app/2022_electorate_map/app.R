# shiny app for electorate mapping
library(shiny)
library(htmltools)
library(leaflet)
library(dplyr)

# data for plotting
species_data <<- readRDS("common_names.rds")
spatial_data <<- readRDS("simple_spatial_data.rds")

ui <- fluidPage(
  # import css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  fluidRow(
    # main section: leaflet map
    column(
      width = 8,
      div(class = "mapdiv", leafletOutput("mymap", width = "100%", height = "100%"))
    ),
    column(
      width = 4,
      # upper-right section: explanatory text
      div(class = "textdiv",
        tableOutput("text"),
      ),
      # lower-right section: taxonomic groups
      # currently vertebrates only
      circle_icon("taxon_clear", "outlines/Australia.svg"),
      circle_icon("taxon_birds", "outlines/PhyloPic-aves.svg"),
      circle_icon("taxon_mammals", "outlines/PhyloPic-mammalia.png"),
      circle_icon("taxon_reptiles", "outlines/PhyloPic-reptilia.png"),
      circle_icon("taxon_fish", "outlines/PhyloPic-fish.png"),
      circle_icon("taxon_frogs", "outlines/PhyloPic-frog.png")
    )
  )
)

server <- function(input, output){
  
  user_data <- reactiveValues(
    click = c(),
    type = "default"
  )
  
  # render leaflet map
  output$mymap <- renderLeaflet({
    leaflet() |> 
      fitBounds(112, -45, 154, -10) |>
      addProviderTiles(providers$CartoDB.Positron) |>  
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
  })

  # capture click events on map
  observeEvent(input$mymap_shape_click$id, { # previously `mouseover`
    user_data$type <- "species"
    user_data$click <- species_data |> 
      filter(electorate == input$mymap_shape_click$id)
    # any way to 'click off' map to show intro text?
  })

  
  # capture click events on taxa
  observeEvent(input$taxon_clear, {
    user_data$type <- "default"
    user_data$click <- c()
    leaflet_reset()
  })
  
  observeEvent(input$taxon_birds, {
    user_data$type <- "default"
    user_data$click <- species_data |> filter(class == "Aves")
    map_tr <- spatial_data[
      spatial_data$Elect_div %in% user_data$click$electorate, ]
    leaflet_update(map_tr)
  })
  
  observeEvent(input$taxon_mammals, {
    user_data$type <- "default"
    user_data$click <- species_data |> filter(class == "Mammalia")
    map_tr <- spatial_data[
      spatial_data$Elect_div %in% user_data$click$electorate, ]
    leaflet_update(map_tr)
  })
  
  observeEvent(input$taxon_reptiles, {
    user_data$type <- "default"
    user_data$click <- species_data |> filter(class == "Reptilia")
     map_tr <- spatial_data[
      spatial_data$Elect_div %in% user_data$click$electorate, ]
    leaflet_update(map_tr)
  })
  
  observeEvent(input$taxon_frogs, {
    user_data$type <- "default"
    user_data$click <- species_data |> filter(class == "Amphibia")
    map_tr <- spatial_data[
      spatial_data$Elect_div %in% user_data$click$electorate, ]
    leaflet_update(map_tr)
  })
  
  observeEvent(input$taxon_fish, {
    user_data$type <- "default"
    user_data$click <- species_data |> filter(class == "Actinopterygii" | class == "Chondrichthyes")
    map_tr <- spatial_data[
      spatial_data$Elect_div %in% user_data$click$electorate, ]
    leaflet_update(map_tr)
  })

  
  # render text
  output$text <- renderText({
    if(user_data$type == "default"){
      paste0(
        "<h1>", "Federal Electorates of Australia", "</h1>",
        "2021 boundaries showing representative species<br>",
        "data from ala.org.au"
      )
    }else{
      paste0(
        "<h1>", 
          user_data$click$electorate, ", ", user_data$click$state, 
        " </h1>",
        "<h1>", user_data$click$vernacular_name, "</h1>",
        "<h3><i>", user_data$click$species, "</i></h3>",
        "<i>",
          user_data$click$phylum, " > ",
          user_data$click$class, " > ",
          user_data$click$order, " > ",
          user_data$click$family,
        "</i>"
      )
    }
  })
}

shinyApp(ui, server)