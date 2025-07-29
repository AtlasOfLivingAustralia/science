# Only the first function is necessary if plotting single figure; most of these
# steps are just general data wrangling. The functions are only because I'm
# making 4 figures and so it makes it easier to map.

# makes an irregular circle 
# adapted from dataviz for ICCB 2025 title slide
add_irregular_edges <- function(centre = c(0, 0),
                                radius,
                                n_points = 200,
                                noise_amplitude = 0.2,
                                frequency = 1,
                                seed = NULL) {
  
  if(!is.null(seed)) set.seed(seed)
  
  theta <- seq(0, 2 * pi, length.out = n_points + 1)[-1]
  
  # circular coordinates to sample 2D Perlin noise
  x_noise <- cos(theta) * frequency
  y_noise <- sin(theta) * frequency
  r_noise <- gen_perlin(x = x_noise, y = y_noise)
  
  # modulate radius with periodic noise
  r <- radius + noise_amplitude * r_noise
  
  tibble(x = centre[1] + r * cos(theta),
         y = centre[2] + r * sin(theta)) %>% 
    bind_rows(slice(., 1)) 
  
}

# get a dataframe with radius, amplitude, frequency based on count
calculate_radius <- function(df) {
  
  df |> 
    arrange(year) |> 
    mutate(radius_increment = scales::rescale(count, to = c(1, 10)),
           radius = accumulate(radius_increment, `+`),
           amplitude = scales::rescale(count, to = c(0.2, 0.9))) |> 
    rowwise() |> 
    mutate(frequency = sample(2:8, 1)) |> 
    select(-radius_increment) |> 
    pmap_dfr(function(species, year, count, radius, amplitude, frequency) {
      add_irregular_edges(radius = radius,
                          noise_amplitude = amplitude,
                          frequency = frequency) |>
        mutate(species = species, year = year, count = count)}) |> 
    # makes innermost circle a bit larger
    filter(year != 1975) |> 
    # to plot from newest (biggest polygon) to oldest (smallest polygon)
    mutate(year = factor(year, levels = 2025:1976)) 
  
}

# plotting functions

get_name_coords <- function(rings_df, spp_df) {
  
  max_radius <- rings_df |> 
    slice_max(x) |> 
    pull(x)
  
  title <- tibble(x = max_radius * cos(seq(0.2*pi, 1.2*pi, length.out = 50)),
                  y = max_radius * sin(seq(0.2*pi, 1.2*pi, length.out = 50)),
                  text = paste(spp_df$vernacular, "•", spp_df$species)) 
  
}

plot_rings <- function(rings_df, year_labs, name_labs, bg_pal) {
  
  min_label <- min(rings_df$count)
  max_label <- max(rings_df$count)
  breaks <- round(seq(min_label, max_label, length.out = 4), 0)
  
  ggplot() + 
    geom_polygon(data = rings_df, 
                 aes(x, y, group = year, fill = count)) +
    scale_fill_gradientn(name = "Records added to the ALA",
                         colors = rings_pal, 
                         breaks = breaks) +
    geom_text(data = year_labs,
              aes(x, y, label = year),
              hjust = "left",
              size = 6,
              colour = text_col) +  
    geom_textpath(data = name_labs,
                  aes(x, y, label = text),
                  text_only = TRUE,
                  size = 10,
                  family = "open_sans",
                  spacing = -180,
                  vjust = 0.9,
                  colour = rings_pal[1]) +
    coord_equal() + 
    theme_void() +
    theme(plot.background = element_rect(fill = bg_pal, colour = NA),
          legend.position = "bottom",
          legend.ticks = element_blank(),
          legend.key.width  = unit(2.5, "lines"),
          legend.key.height = unit(0.8, "lines"),
          legend.title.position = "top",
          legend.title = element_text(hjust = 0.5, size = 25, colour = rings_pal[1]),
          legend.text = element_text(size = 22, vjust = 1.2, colour = rings_pal[1]),
          text = element_text(family = "open_sans"),
          plot.margin = margin(t = 15,
                               r = 30,
                               b = 20,
                               l = 30))
  
}
