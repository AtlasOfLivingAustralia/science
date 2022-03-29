
library(showtext)
library(ggplot2)

font_add_google("Lato", "lato")
showtext_auto()

theme_ala101 <- function() {
  
  theme_minimal() %+replace%
    
    theme(
      text = element_text(family = "lato"),
      axis.title.x = element_text(vjust = 1, size = rel(0.9), face = "bold", colour = "#444444"), 
      axis.title.y = element_text(angle = 90, vjust = 0.8, size = rel(2), face = "bold", colour = "#444444"), 
      axis.text = element_text(size = 16),
      plot.background = element_rect(fill = 'white', colour = 'white'),
      panel.background = element_rect(fill = 'white', colour = 'white'),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = 0.2)
    )
}