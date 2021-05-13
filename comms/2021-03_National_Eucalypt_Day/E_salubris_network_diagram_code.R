# do a plot of E. salubris for National Eucalypt Day 2021

# Note: This script requires an image to build the colour palette from.
# I used a downscaled version of the image at this link:
# https://twitter.com/DeanNicolle1/status/1374112431782301698


# First, get observations of the Eucalypt of the Year 2021 from ALA
# remotes::install_github("AtlasOfLivingAustralia/galah")
library(galah)
# Note: config required at this point using format: ala_config(email = "myemail@email.com")
counts <- select_taxa("Eucalyptus salubris", include_counts = TRUE)
occurrences <- ala_occurrences(counts)
occurrences$date <- lubridate::ymd(occurrences$eventDate)
occurrences$year <- lubridate::year(occurrences$date)


# Then get a color scheme from images of the species in question
# remotes::install_github("AndreaCirilloAC/paletter")
library(paletter)

# get a colour palette
image_pal <- create_palette(
  image_path = "./data/Dean_Nicolle_Esalubris_image_small.jpeg",
  type_of_variable = "categorical",
  number_of_colors = 15)
image_pal <- image_pal[image_pal != "#527FB9"] # remove blue from the palette

# create a vector to index colours
colour_index <- rep(seq_along(image_pal),
  each = floor(nrow(occurrences) / length(image_pal)))
# correct rounding errors
colour_index <- c(colour_index,
  rep(length(image_pal), nrow(occurrences) - length(colour_index)))
# add an index of colours to occurrences
occurrences$colour_index <- as.factor(colour_index)


# make an interesting layout by interpreting colours as a network
library(igraph)

graph_list <- lapply(c(1:14), function(a){
  lookup <- which(colour_index == a)
  return(data.frame(
    from = lookup[c(1:(length(lookup)-1))],
    to = lookup[c(2:length(lookup))]))
  })
graph_df <- as.matrix(do.call(rbind, graph_list))
colour_graph <- graph_from_edgelist(graph_df)

# convert to a set of point locations
test_layout <- as.data.frame(layout_nicely(colour_graph))
colnames(test_layout) <- c("x", "y")
test_layout$colour_index <- factor(colour_index)


# draw with ggplot
library(ggplot2)

p <- ggplot(test_layout, aes(x = x, y = y, colour = colour_index)) +
  geom_point(size = 3.5, alpha = 1) +
  scale_color_manual(values = image_pal) +
  coord_fixed() +
  lims(x = c(-20, 37), y = c(-20, 37)) + # these are fairly arbitrary
  theme_void() +
  theme(legend.position = "none")

# save
ggsave("plot_image.pdf", p)
