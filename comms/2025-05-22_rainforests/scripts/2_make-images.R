# -----------------------------------------------------------------#
# Title: Gondwana rainforests - make circle images
# Author: Dax Kellie
# -----------------------------------------------------------------#

# Code from here: https://tanyaviz.com/blog/image-circular-text/
# also from here: https://github.com/tashapiro/tanya-data-viz/blob/main/spotify-artists/scripts/plot-spotify-artists.R
# Tanya Shapiro is an absolute legend
library(tidyverse)
library(geomtextpath)
library(ggimage)
library(cropcircles)
library(magick)
library(glue)
library(showtext)
font_add_google("Roboto", "roboto")
showtext_auto()

top_taxa <- read_csv(here::here("comms",
                                "2025-05-22_rainforests",
                                "data",
                                "top-taxa-images.csv"),
                     )


#custom function to apply border to circle image with magick
border <- function(im) {
  ii <- magick::image_info(im)
  ii_min <- min(ii$width, ii$height)
  
  img <- image_blank(width = ii_min, height = ii_min, color = "none")
  drawing <- image_draw(img)
  symbols(ii_min/2, ii_min/2, circles = ii_min/2, bg = '#2A3C24', inches = FALSE, add = TRUE)
  dev.off()
  
  x = image_composite(image_scale(drawing, "x430"), image_scale(im, "x400"), offset = "+15+15")
  
  x
}

plot_image_label<-function(image,
                           label,
                           font_color="black", 
                           position="bottom",
                           hjust=0.2){
  
  #crop the image into a circle shape
  cropped_image = cropcircles::circle_crop(image)
  
  t = seq(0, 1, length.out = 100) * pi
  
  #set up params based on top or bottom
  if(position=="top"){
    data = data.frame(x = cos(t),y = sin(t))
    vjust=1.1
    ymax=1.2
    ymin=-0.9}
  
  else if(position=="bottom"){
    data=data.frame(x = cos(t),y = sin(t)*-1)
    vjust=-0.1
    ymax=1.0
    ymin=-1.4
    }
  
  #plot
  ggplot() +
    geom_image(aes(x=0, y=0, image = cropped_image), asp=2.4/2.1, size= .67, image_fun = border) +
    scale_x_continuous(limits = c(-1.5, 1.5))+
    scale_y_continuous(limits=c(ymin, ymax))+
    geom_textpath(data = data, aes(x , y, label = label), linecolor = NA, color = font_color,
                  family = "roboto",
                  size = 11, vjust = vjust, hjust = hjust)+
    coord_equal()+
    theme_void()
}


#test function
# plot_image_label(image=top_taxa$image_url[2],
#                  label= top_taxa$common_name[2],
#                  font_color="#dac17a")

# new file path for images
top_taxa <- top_taxa |> 
  mutate(
    new_image_path = paste0(tolower(str_replace_all(stringr::word(scientificName, 1)," ","_")),".png")
    )

# list of hjust values by .11
list_hjust <- seq(0,1, by=0.1)


# image save function
plot_and_save <- function(df) {
  #create loop to generate and save all images with labels
    hjust <- sample(list_hjust,1)
    path <- df$new_image_path
    plot <- plot_image_label(image = df$image_url,
                            label = df$common_name,
                            font_color = "#F4ECE1",
                            # position = position, 
                            hjust = hjust)
    ggsave(filename=glue("comms/2025-05-22_rainforests/images/{path}"), 
           plot = plot, 
           height = 3.95, 
           width = 4.5)
}

# loop to save photos
showtext_opts(dpi = 350)

top_taxa |>
  # slice(25) |>
  group_split(row_number()) |>
  map(\(df)
      plot_and_save(df))


# IMPORTANT: 
# Restart R and R Studio after running this code 
# (ie before attempting to save all images again or run any other script)
#
# For some reason incorrect images are saved if attempting to run multiple times.
# I think the image/plot device stays on after this loop, which means other plots 
# made afterwards can accidentally overwrite saved images.



