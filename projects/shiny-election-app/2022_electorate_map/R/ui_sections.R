# ui sections (rendered in server)


# opening block of images
block_images <- function(){
  div(class = "textdiv",
    tags$table(class = "image_table",
      tags$tr(
        tags$td(
          species_icon(id = "spp_1", "taxa/Acanthizalineata.jpeg")),
          # img(class = "taxon", src = "taxa/Acanthizalineata.jpeg")),
        tags$td(
          img(class = "taxon", src = "taxa/Ardeamodesta.jpeg")),
        tags$td(
          img(class = "taxon", src = "taxa/Cacatuagalerita.jpeg")),
      ),
      tags$tr(
        tags$td(
          img(class = "taxon", src = "taxa/Christinusmarmoratus.jpeg")),
        tags$td(
          img(class = "taxon", src = "taxa/Coturnixypsilophora.jpeg")),
        tags$td(
          img(class = "taxon", src = "taxa/Cygnusatratus.jpeg")),
      ),
      tags$tr(
        tags$td(
          img(class = "taxon", src = "taxa/Haliaeetusleucogaster.jpeg")),
        tags$td(
          img(class = "taxon", src = "taxa/Litoriaperonii.jpeg")),
        tags$td(
          img(class = "taxon", src = "taxa/Menetiagreyii.jpeg")),
      )
    )
  )
}

species_icon <- function(id, image_path){
  tags$a(
    id = id,
    href = "#",
    class = "action-button shiny-bound-input",
    tags$image(class = "taxon", src = image_path)
  )
}



# section for showing information for a single species
species_profile <- function(df){
  div(class = "textdiv",
    tags$img(
      class = "mainpic",
      src = "taxa/Litoriaperonii.jpeg"
    ),
    tags$a(
      href = paste0("https://bie.ala.org.au/species/",
        df$taxonConceptID),
      target = "_blank",
      h1(df$vernacular_name)
    ),
    tags$p(
      tags$i(df$species),
      paste0("| ", 
        format(df$count, big.mark = ","), 
        " records", 
        " | z score = ",
        format(df$z_score, digits = 2)
        )
    ),
    help_button()
    # tags$p(paste0(
    #   user_data$click$phylum, " > ",
    #   user_data$click$class, " > ",
    #   user_data$click$order, " > ",
    #   user_data$click$family
    # ))
  )
}


# electoral division sections
division_default <- function(){
  div(class = "electoratediv",
    div(class = "paddeddiv1", 
      h2("Biodiversity of Australia"),
      # tags$p(class = "electorate_name", tags$b("Biodiversity of Australia")),
      tags$a(
        id = "launch_help",
        href = "#",
        class = "action-button shiny-bound-input",
        "Representative species for each electoral division"
      ),
      tags$img(src='images/ALA_Logo_Inline_RGB.png', width = '50%')
    )
  )
}

division_selected <- function(df){
  div(class = "electoratediv",
    div(class = "paddeddiv2",
      tags$p(toupper("electoral division of")),
      tags$p(class = "electorate_name", tags$b(df$electorate)),
      # tags$p(paste0("electoral division of", df$electorate)),
      tags$p(toupper(switch(df$state,
        "ACT" = "Australian Capital Territory",
        "NSW" = "New South Wales",
        "NT" = "Northern Territory",
        "QLD" = "Queensland",
        "SA" = "South Australia",
        "TAS" = "Tasmania",
        "VIC" = "Victoria",
        "WA" = "Western Australia"
      ))),
      tags$p(
        paste0(
          format(df$electorate_records, big.mark = ","),
          " records since 2019"
        )
      )
    )
  )
}

# circle icons for groups of taxa
# TODO: add hover text
circle_icon <- function(id, image_path, tip = "none"){
  tags$a(
    id = id,
    href = "#",
    class = "action-button shiny-bound-input",
    tags$div(
      class = "clear_circle",
      # tags$span(
      #   class = "tooltiptext",
      #   tip
      # ),
      # title = title,
      tags$image(class = "taxon-outline", src = image_path)
    )
  )
}

# shiny element for launching a modal
help_button <- function(){
  tags$a(
    id = "launch_help",
    href = "#",
    class = "action-button shiny-bound-input",
    icon("question")
  )
}

