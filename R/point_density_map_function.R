#---
# title: Function to make a point density map of occurrence records from galah 
# author: Olivia Torresan, Dax Kellie
# date: 14 August, 2024
#---


make_pdensity_map <- function(taxon, 
                         point_colour_1,
                         point_colour_2,
                         legend_text_colour) {
#create aus map
  aus <- ozmap_data(data = "states")
  
#plot 
  p <- ggplot() +
    geom_sf(data = aus, fill = NA, colour = NA) +
    geom_pointdensity(data = taxon,
                      mapping = aes(
                        x = decimalLongitude,
                        y = decimalLatitude, 
                        alpha = 0)) +
    scale_color_gradientn(colours = c("#C3EDEF", "#076164"), 
                          na.value = "white", 
                          labels = scales::comma_format(),
                          n.breaks = 4,
                          guide = guide_colourbar(title = "Observations", title.position = "top")) +
    coord_sf(
      ylim = c(-43, -10),
      xlim = c(114, 153)) +
    guides(alpha = "none",
           colour = guide_colorbar(title = "Number of 
overlapping points")) +
    theme_void() +
    theme(legend.title = element_text(hjust = 0.5,
                                      family = "Roboto",
                                      size = 12),
          legend.position = "bottom",  # Center horizontally and position vertically
          legend.justification = c("center", "bottom"),
          legend.text = element_text(size = 10,
                                     family = "Roboto",
                                     colour = legend_text_colour),
          legend.text.align = 0.5)
 
   return(p)
    
  
}