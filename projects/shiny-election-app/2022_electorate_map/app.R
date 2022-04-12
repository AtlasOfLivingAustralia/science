# shiny app for electorate mapping
library(shiny)
library(htmltools)
library(leaflet)
library(dplyr)

## TODO:
  # add bar next to profile pics with ?, ALA, photo credit
    # ? should link to modal with full explanation of methods
  # add share bar
  # try using urls for species images
  
# DONE:
  # link species name to ALA page
  

# data for plotting
species_data <<- readRDS("common_names.rds")
australia_count <<- sum(species_data$electorate_records)
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
      div(class = "mapdiv", 
        leafletOutput("mymap", width = "100%", height = "100%")
      )
    ),
    column(
      width = 4,
      uiOutput("topright"),
      uiOutput("middleright"),
      tags$div(
        class = "textdiv",
        tags$table(class = "image_table",
          tags$tr(
            tags$td(
              circle_icon("taxon_clear", 
                "outlines/Australia.svg", 
                tip = "Reset Map")),
            tags$td(
              circle_icon("taxon_birds", 
                "outlines/PhyloPic-aves.svg",
                tip = "Birds")),
            tags$td(
              circle_icon("taxon_mammals", 
                "outlines/PhyloPic-mammalia.png", 
                tip = "Mammals")),
          ),
          tags$tr(
            tags$td(
              circle_icon("taxon_reptiles", 
                "outlines/PhyloPic-reptilia.png",
                tip = "Reptiles")),
            tags$td(
              circle_icon("taxon_fish", "outlines/PhyloPic-fish.png",
                tip = "Fish")),
            tags$td(
              circle_icon("taxon_frogs", "outlines/PhyloPic-frog.png",
                tip = "Frogs"))
          )
        )
      )
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
    leaflet(
      options = leafletOptions(minZoom = 4)
    ) |> 
      fitBounds(112, -45, 154, -10) |>
      setMaxBounds(115, -50, 155, -5) |>
      addProviderTiles(providers$CartoDB.Positron) |>  
      # default_polygons(x = spatial_data)
      addPolygons(
        data = spatial_data,
        layerId = spatial_data$Elect_div,
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

  # capture events on species
  observeEvent(input$spp_1, {    
    map_tr <- spatial_data[spatial_data$species == "Acanthiza lineata", ]
    leaflet_update(map_tr)
  })
 
  # click on help
  observeEvent(input$launch_help, {
    help_modal()
  })
   
   
  # capture click events on taxa
  observeEvent(input$taxon_birds, {    
    map_tr <- spatial_data[spatial_data$class == "Aves", ]
    leaflet_update(map_tr)
  })
  observeEvent(input$taxon_mammals, {
    map_tr <- spatial_data[spatial_data$class == "Mammalia", ]
    leaflet_update(map_tr)
  })
  observeEvent(input$taxon_reptiles, {
    map_tr <- spatial_data[spatial_data$class == "Reptilia", ]
    leaflet_update(map_tr)
  })
  observeEvent(input$taxon_frogs, {
    map_tr <- spatial_data[spatial_data$class == "Amphibia", ]
    leaflet_update(map_tr)
  })
  observeEvent(input$taxon_fish, {
    map_tr <- spatial_data[
      (spatial_data$class == "Actinopterygii" | 
      spatial_data$class == "Chondrichthyes"), ]
    leaflet_update(map_tr)
  })
  
  # clear click events on taxa
  observeEvent(input$taxon_clear, {
    user_data$type <- "default"
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
          color = "#B7CD96", 
          fillColor = "#B7CD96", 
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
  
  # RENDER SPECIES PROFILE AREA
  output$topright <- renderUI({
    if(user_data$type == "default"){
      block_images()
    }else{  
      species_profile(user_data$click)
    } 
  })

  # RENDER ELECTORAL DIVISION TEXT BOX
  output$middleright <- renderUI({
    if(user_data$type == "default"){  
      division_default()
    }else{
      division_selected(user_data$click)
    }
  })
  
  
}

shinyApp(ui, server)