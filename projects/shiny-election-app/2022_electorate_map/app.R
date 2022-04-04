# shiny app for electorate mapping
library(shiny)
library(htmltools)
library(leaflet)
library(dplyr)

map_data <- readRDS("simpl_spatial_data.rds")
map_tibble <- as_tibble(map_data)
leaflet_map <- readRDS("leaflet_map.rds")

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
      tags$div(class = "textdiv",
        # code to place an image of a bird as a link
        tags$div(class = "profile_div",
          tags$a(
            id = "taxon_birds",
            href = "#",
            class = "action-button shiny-bound-input",
            tags$image(
              class = "profile",
              src="images/swift_parrot.jpeg"
            )
          ),
          tags$p(class = "profile_text", "Birds")
        )
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
  )
  
  # render leaflet map
  output$mymap <- renderLeaflet(leaflet_map)

  # capture click events on map
  observeEvent(input$mymap_shape_click$id, { # previously `mouseover`
    user_data$type <- "map"
    user_data$click <- map_tibble |> 
      filter(Elect_div == input$mymap_shape_click$id)
    # any way to 'click off' map to show intro text?
  })
  
  # capture click events on taxa
  observeEvent(input$taxon_birds, {
    user_data$type <- "taxon"
    user_data$click <- map_tibble |> filter(class == "Aves")
    user_data$taxon_name <- "Birds"
    user_data$taxon_classification <- "Chordata > Aves"
    
    # NEXT: use leafletProxy() to highligh all electorates with class == "Aves"
    # https://rstudio.github.io/leaflet/shiny.html
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
          order(user_data$click$z_squared, decreasing = TRUE), ]
        paste0(
          "<h1>", user_data$taxon_name, "</h1>",
          "<i>", user_data$taxon_classification, "</i>",
          "<h2>Most representative electorates</h2>",
          paste(
            bird_subset$Elect_div[1:5], 
            bird_subset$vernacular_name[1:5],
            sep = " | ",
            collapse = "<br>")
        )
      }
      else if(user_data$type == "map"){
        paste0(
          "<h1>", 
            user_data$click$Elect_div, ", ", user_data$click$state, 
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