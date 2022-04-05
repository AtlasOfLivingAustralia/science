# shiny app for electorate mapping
library(shiny)
library(htmltools)
library(leaflet)
library(dplyr)

# data for plotting
species_data <- readRDS("common_names.rds")
spatial_data <- readRDS("simpl_spatial_data.rds")

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
      tags$a(
        id = "taxon_birds",
        href = "#",
        class = "action-button shiny-bound-input",
        tags$image(
          class = "profile",
          src="images/swift_parrot.jpeg"
        )
      ),
      tags$a(
        id = "taxon_mammals",
        href = "#",
        class = "action-button shiny-bound-input",
        tags$image(
          class = "profile",
          src="images/swift_parrot.jpeg"
        )
      ),
      tags$a(
        id = "taxon_reptiles",
        href = "#",
        class = "action-button shiny-bound-input",
        tags$image(
          class = "profile",
          src="images/swift_parrot.jpeg"
        )
      ),
      tags$a(
        id = "taxon_fish",
        href = "#",
        class = "action-button shiny-bound-input",
        tags$image(
          class = "profile",
          src="images/swift_parrot.jpeg"
        )
      ),
      tags$a(
        id = "taxon_frogs",
        href = "#",
        class = "action-button shiny-bound-input",
        tags$image(
          class = "profile",
          src="images/swift_parrot.jpeg"
        )
      ),
      tags$a(
        id = "taxon_clear",
        href = "#",
        class = "action-button shiny-bound-input",
        tags$div(class = "clear_circle", "show all")
      )
    )
  )
)

server <- function(input, output){
  
  user_data <- reactiveValues(
    click = c(),
    type = "none",
    taxon_name = c(),
    taxon_classification = c()
    # group = "none"
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
    user_data$type <- "map"
    user_data$click <- species_data |> 
      filter(electorate == input$mymap_shape_click$id)
    # any way to 'click off' map to show intro text?
  })

  
  # capture click events on taxa
  observeEvent(input$taxon_birds, {
    user_data$type <- "taxon"
    user_data$click <- species_data |> filter(class == "Aves")
    user_data$taxon_name <- "Birds"
    user_data$taxon_classification <- "Chordata > Aves"
    map_tr <- spatial_data[
      spatial_data$Elect_div %in% user_data$click$electorate, ]
    leaflet_update(map_tr)
  })
  
  observeEvent(input$taxon_mammals, {
    user_data$type <- "taxon"
    user_data$click <- species_data |> filter(class == "Mammalia")
    user_data$taxon_name <- "Mammals"
    user_data$taxon_classification <- "Chordata > Mammalia"
    map_tr <- spatial_data[
      spatial_data$Elect_div %in% user_data$click$electorate, ]
    leaflet_update(map_tr)
  })
  
  observeEvent(input$taxon_reptiles, {
    user_data$type <- "taxon"
    user_data$click <- species_data |> filter(class == "Reptilia")
    user_data$taxon_name <- "Reptiles"
    user_data$taxon_classification <- "Chordata > Reptilia"
    map_tr <- spatial_data[
      spatial_data$Elect_div %in% user_data$click$electorate, ]
    leaflet_update(map_tr)
  })
  
  observeEvent(input$taxon_frogs, {
    user_data$type <- "taxon"
    user_data$click <- species_data |> filter(class == "Amphibia")
    user_data$taxon_name <- "Frogs"
    user_data$taxon_classification <- "Chordata > Amphibia > Anura"
    map_tr <- spatial_data[
      spatial_data$Elect_div %in% user_data$click$electorate, ]
    leaflet_update(map_tr)
  })
  
  observeEvent(input$taxon_fish, {
    user_data$type <- "taxon"
    user_data$click <- species_data |> filter(class == "Actinopterygii" | class == "Chondrichthyes")
    user_data$taxon_name <- "Fish"
    user_data$taxon_classification <- "Classes Actinopterygii & Chondrichthyes"
    map_tr <- spatial_data[
      spatial_data$Elect_div %in% user_data$click$electorate, ]
    leaflet_update(map_tr)
  })
  
  observeEvent(input$taxon_clear, {
    user_data$type <- "none"
    user_data$click <- c()
    user_data$taxon_name <- c()
    user_data$taxon_classification <- c()
    leaflet_reset()
  })
  
  # render text
  output$text <- renderText({
    if(is.null(user_data$click)){
      paste0(
        "<h1>", "Federal Electorates of Australia", "</h1>",
        "2021 boundaries showing representative species<br>",
        "data from ala.org.au"
      )
    }else{
      if(user_data$type == "taxon"){
        bird_subset <- user_data$click[
          order(user_data$click$z_sum, decreasing = TRUE), ]
        paste0(
          "<h1>", user_data$taxon_name, "</h1>",
          "<i>", user_data$taxon_classification, "</i>",
          "<h2>Most representative electorates</h2>",
          paste(
            bird_subset$electorate[1:5], 
            bird_subset$vernacular_name[1:5],
            sep = " | ",
            collapse = "<br>")
        )
      }
      else if(user_data$type == "map"){
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
    }
  })
}

shinyApp(ui, server)