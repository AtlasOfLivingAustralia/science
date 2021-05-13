# Functions used in data_provider_count_script.R

# required packages
library(packcircles)
library(jsonlite)
library(crul)
library(ggplot2)

# functions for creating circular polygons of specified size
make_circle <- function(
  n = 100, # number of points, equally spaced around the edge of a circle
  k,  # scaling value (radius) - larger for bigger circles. defaults to 1
  offset = c(0, 0)
){
  if(missing(k)){k <- 1}
  # get coordinates
  theta <- (2 * (pi/(n-1)) * seq_len((n-1)) ) # -alpha
  values <- data.frame(
  	x = k * cos(theta),
  	y = k * sin(theta))
  rownames(values) <- NULL
  values$x <- values$x + offset[[1]]
  values$y <- values$y + offset[[2]]
  # return(values)
  return(values[c(seq_len(n-1), 1), ])
}
# note the result has nrow == n, but n-1 unique rows

# run make circle in a loop
make_circles <- function(
  df, # should be returned by packcircles::circleProgressiveLayout
  gap = 0
){
  df_list <- split(df, seq_len(nrow(df)))
  result <- as.data.frame(do.call(rbind,
    lapply(df_list, function(a){
      make_circle(k = a$radius - gap, offset = c(a$x, a$y))
    })
  ))
  result$group <- rep(seq_along(df_list), each = 100)
  return(result)
}

# function to build a circle-packing plot
# this is a bit over-engineered for this purpose but works ok
taxon_plot_data <- function(
  labels, # vector of taxon labels
  counts, # vector of counts
  color_scale = "viridis",
  log_scale = TRUE, # logical - should the result be log + 1 -scaled?
  gap = 0 # how much should the radius be reduced to decrease circle overlap?
){
  if(log_scale){
    taxon_counts <- log(counts + 1)
  }else{
    taxon_counts <- counts
  }
  circle_df <- packcircles::circleProgressiveLayout(taxon_counts)
  circle_plot_df <- make_circles(circle_df, gap = gap)
  circle_plot_df$label <- factor(circle_plot_df$group,
    levels = unique(circle_plot_df$group),
    labels = labels)
  circle_plot_df$count <- rep(counts, each = 100)
  circle_plot_df$plotly_text <- paste0(
    "<b>",
    circle_plot_df$label,
    "</b><br>n = ",
    formatC(circle_plot_df$count, big.mark = ","))
  circle_plot_df$color <- rep(
    do.call(color_scale, args = list(
      n = length(labels),
      direction = -1)),
    each = 100)
  return(circle_plot_df)
}

# query the data provider API
lookup_resource_uid <- function(id){
  url <- paste0("https://collections.ala.org.au/ws/dataResource/", id)
  cli <- HttpClient$new(url)
  cli <- cli$get()
  text <- cli$parse("UTF-8")
  if(nchar(text) < 10000){
    result <- fromJSON(text, flatten = TRUE)
    out <- data.frame(
      uid = {if(is.null(result$uid)){NA}else{result$uid}},
      name = {if(is.null(result$name)){NA}else{result$name}},
      provider = {if(is.null(result$provider$name)){NA}else{result$provider$name}},
      institution = {if(is.null(result$institution$name)){NA}else{result$institution$name}}
    )
  }else{
    out <- data.frame(uid = NA, name = NA, provider = NA, institution = NA)
  }
  return(out)
}

# plot code
ALA_colors <- list(
  flamingo = "#E06E53", # primary
  rust = "#B8573E",
  grey = "#667073",
  concrete = "#EEECEA", # secondary (monochrome)
  silver = "#9E9E9F",
  charcoal = "#222322",
  honey = "#FFC557", # extended
  pale_moss = "#B7CD96",
  seafoam = "#6BDAD5",
  ocean = "#003A70",
  # lavender = "#A19B2",
  plum = "#691C32"
)
scales::show_col(unlist(ALA_colors))

plot_providers <- function(data_df, labels_df, label_split = 2000, color = "#E06E53"){
  ggplot() +
    geom_polygon(
      data = data_df,
      mapping = aes(x = x, y = y, group = group, alpha = -group),
      fill = color
    ) +
    # add centred text for large circles
    geom_text(
      data = labels_df[labels_df$count >= label_split, ],
      mapping = aes(x = x, y = y, label = label),
      hjust = 0.5,
      color = "white",
      size = 3
    ) +
    # add offset labels for small circles
    geom_text(
      data = labels_df[labels_df$count < label_split, ],
      mapping = aes(x = x_max, y = y, label = label),
      color = "#667073",
      hjust = 0,
      nudge_x = 50,
      size = 3
    ) +
    # optional code for adding counts
    # geom_text(
    #   data = labels_df[labels_df$x > x_split, ],
    #   mapping = aes(x = x_max, y = y, label = count),
    #   color = "#667073",
    #   hjust = 0,
    #   nudge_x = 50,
    #   nudge_y = y_offset,
    #   size = 2
    # ) +
    # geom_text(
    #   data = labels_df[labels_df$x <= x_split, ],
    #   mapping = aes(x = x, y = y, label = count),
    #   color = "white",
    #   hjust = 0.5,
    #   nudge_y = y_offset,
    #   size = 2
    # ) +
    coord_fixed() +
    # optional content for changing the color scheme
    # scale_fill_viridis(option = "magma", begin = 0.1, end = 0.9, direction = -1) +
    # scale_fill_gradient(low = "#B8573E", high = "#E06E53") +
    scale_alpha(range = c(0.5, 1)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
}
