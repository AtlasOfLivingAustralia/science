
library(showtext)
library(ggplot2)

font_add_google("Lato", "lato")
showtext_auto()

theme_plant <- function() {
  
  theme_minimal() %+replace%
    
    theme(
      text = element_text(family = "lato"),
      axis.title.x = element_text(vjust = 1, size = rel(0.9), face = "bold", colour = "#444444"), 
      axis.title.y = element_text(angle = 90, vjust = 1, size = rel(0.9), face = "bold", colour = "#444444"), 
      plot.background = element_rect(fill = 'white', colour = 'white'),
      panel.background = element_rect(fill = 'white', colour = 'white'),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = 0.2)

    )
}