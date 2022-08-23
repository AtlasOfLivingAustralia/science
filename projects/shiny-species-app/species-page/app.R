library(shiny)
library(bslib)
library(galah)
library(glue)
library(dplyr)
library(waiter)
library(ozmaps)
library(sf)
library(leaflet)
library(tidyr)
library(glue)
library(plotly)

spatial_data <- ozmap_data()

ui <- fluidPage(
  useWaiter(),
  # banner ribbon
  fluidRow(
    style = "background-color:#B7CD96;",
    column(8,
      img(
        src = "ALA_Logo_Inline_REV.png", 
        style = "width: 300px; padding: 15px;"
      ),
      "species viewer (demo)"
    ),
    column(width = 3,
      br(),
      textInput(
        inputId = "search_text", 
        label = NULL, 
        placeholder = "Search",
        width = "100%"
      )
    ),
    column(width = 1,
      br(),
      actionButton(
        inputId = "search_go",
        label = "go"
      )
    )
  ),
  # main body
  fluidRow(
    # left-hand column
    column(6,
      tableOutput("species_name")
      # textOutput("species_name") # for testing
    ),
    # right-hand column
    column(6,
      div(
        style = "margin-top: 20px; margin-right: 20px;",
        leafletOutput("map", width = "100%", height = "320px")
      ),
      div(
        style = "margin-top: 20px; margin-right: 20px;",
        plotlyOutput("timeseries", width = "100%", height = "200px")
      )
    )
  ),
  # fluidRow(
  #   # table of scientific articles, rendered with datatable
  #   # note: this can't be achived yet as we don't have a place to store this info
  # ),
  theme = bs_theme(bootswatch = "minty")
)

server <- function(input, output, session) {
  
  # set up object storage
  x <- reactiveValues(
    search_term = NULL,
    images = NULL,
    map = spatial_data,
    timeseries = NULL,
    data_resource = NULL)
  
  # render leaflet map
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(minZoom = 3)
    ) |> 
      fitBounds(112, -45, 154, -10) |>
      setMaxBounds(115, -50, 155, -5) |>
      addProviderTiles(providers$CartoDB.Positron) |>  
      addPolygons(
        data = x$map,
        fillColor = "white",
        fillOpacity = 0.6, 
        color = "#111111", 
        weight = 1, 
        stroke = TRUE,
        label = ~NAME,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", 
                       padding = "3px 5px"),
          textsize = "12px",
          direction = "auto")
      )
  })
  
  observeEvent(input$search_go, {
    waiter_show(html = spin_dual_circle(), color = transparent(0.5))
    sp_result <- galah::search_taxa(input$search_text)
    if(nrow(sp_result) > 0 & ncol(sp_result) > 1){
      
      # clear cache of old images
      unlink("./www/cache", recursive = TRUE)
      dir.create("./www/cache")
      
      # preserve species information
      x$search_term <- sp_result
      
      # get images from iNat for that taxon
      x$images <- galah_call() |>
        galah_filter(
          basisOfRecord == c("HUMAN_OBSERVATION", "OBSERVATION"),
          # dataResourceName == "iNaturalist Australia",
          images != ""
        ) |>
        galah_identify(sp_result, search = FALSE) |>
        galah_group_by(images) |>
        atlas_counts(limit = 6) |>
        show_all_media() |>
        # filter(width > height) |>
        mutate(largest_file = order(size_in_bytes, decreasing = FALSE)) |>
        collect_media(path = "www/cache/", type = "thumbnail") |>
        mutate(download_path = sub("www/cache//", "cache/", download_path))
      
      # # test getting lat/lons
      # galah_config(email = "ala4r@ala.org.au")
      # ala_occurrences <- galah_call() |>
      #   galah_identify(sp_result, search = FALSE) |>
      #   galah_data_profile(ALA) |> 
      #   galah_select(decimalLatitude, decimalLongitude) |>
      #   atlas_occurrences() 
      ## requires online downloads to be performant   
      
      # add single galah_group_by call with expand = FALSE for:
        # decade (linechart)
        # provider (treemap)
        # ibra regions (map)
      # then plot three diagrams from one data source
      ala_data <- galah_call() |>
        galah_identify(sp_result, search = FALSE) |>
        galah_data_profile(ALA) |>
        galah_group_by(cl22, dataResourceName, decade, expand = FALSE) |>
        atlas_counts(limit = 50)
      
      ala_list <- list(
        state = ala_data |> 
          filter(!is.na(cl22)) |> 
          select(cl22, count),
        data_resource = ala_data |> 
          filter(!is.na(dataResourceName)) |> 
          select(dataResourceName, count),
        decade = ala_data |> 
          filter(!is.na(occurrenceYear)) |> 
          select(occurrenceYear, count) 
      )
      ala_list$decade$decade <- substr(ala_list$decade$occurrenceYear, 2, 5)
      
      # take info above and store in reactive object
      x$map <- spatial_data |> 
        left_join(ala_list$state, by = c("NAME" = "cl22")) |>
        replace_na(list(count = 0)) |>
        mutate(
          label = glue("{NAME}<br>{count} records for <i>{sp_result$scientific_name}</i>")
        )        
      x$timeseries <- ala_list$decade
      x$data_resource <- ala_list$data_resource
      
      # redraw map
      palette <- colorNumeric("YlGn", domain = log10(x$map$count + 1))
      leafletProxy("map") |>
        clearShapes() |>
        addPolygons(
          data = x$map,
          # layerId = spatial_data$Elect_div,
          fillColor = ~palette(log10(count + 1)),
          fillOpacity = 0.6, 
          color = "#111111", 
          weight = 1, 
          stroke = TRUE,
          label = ~lapply(x$map$label, HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", 
                         padding = "3px 5px"),
            textsize = "12px",
            direction = "auto")
        )
      
      waiter_hide()
      
    }else{
      x$search_term <- NULL
      x$images <- NULL
      x$data <- NULL
      waiter_hide()
    }
  })
  
  # render species text from search_taxa
  output$species_name <- renderPrint({
    if(!is.null(x$search_term)){
      paste0(
        paste(
          glue_data(x$images,
            "<img src='{download_path}',
              style = 
                'width: 30%;
                 height: 150px;
                 object-fit: cover;
                 margin-top: 20px;
                 margin-left: 20px;
                 margin-bottom: 0px;
                 margin-right: 0px;
                 border-radius: 10px;
              '>"
            ),
         collapse = ""
       ),
      glue_data(x$search_term,
          "<h2><i>{scientific_name}</i></h2>"
      )) |>
      HTML()
    }
  })
  
  # draw plotly barplot
  output$timeseries <- renderPlotly({
    if(!is.null(x$timeseries)){
      p <- ggplot(x$timeseries, aes(x = decade, y = count)) +
        geom_bar(stat = "identity", fill = "#B7CD96") +
        theme_bw()
      ggplotly(p)
    }
  })
  
}

shinyApp(ui, server)
