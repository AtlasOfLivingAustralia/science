#---
# title: Function to make a hex map of occurrence records from galah 
# author: Olivia Torresan, Dax Kellie
# date: 11 September, 2024
#---

make_stacked_bar_plot <- function(taxon, 
                                  bar_colour_1,
                                  bar_colour_2,
                         legend_text_colour,
                         highlight) {
# cumulative sum column 
  taxon <- taxon %>%
    arrange(-desc(year)) %>%
    mutate(
      total_count = cumsum(count)
    ) %>%
    arrange(desc(year))
  
  # determine the minimum and maximum of 'year' for dataset
  min_year <- min(taxon$year)
  max_year <- max(taxon$year)
  
  # set the x labels to start at a set point past the first listed year 
  start_value <- as.numeric(min_year) + 2
  
  # plot 
  p <- taxon |>
    ggplot() + 
    geom_bar(aes(x = year, y = total_count),
             stat = "identity",
             linewidth = 1,
             fill = bar_colour_1,
             width = 0.98) +
    geom_bar(aes(x = year, y = count),
             stat = "identity",
             linewidth = 0.5,
             fill = bar_colour_2,
             width = 0.98) +
    geom_text_pilot(data = taxon |> filter(year == max_year),
                    mapping = aes(x = year,
                                  y = count,
                                  label = number(count,
                                                 accuracy = 0.1,
                                                 scale_cut = cut_short_scale())),
                    hjust = "center",
                    nudge_y = -900000,
                    size = 2.75) +
    geom_text_pilot(data = taxon |> filter(year == max_year),
                    mapping = aes(x = year,
                                  y = total_count,
                                  label = number(total_count,
                                                 accuracy = 0.1,
                                                 scale_cut = cut_short_scale())),
                    hjust = "center",
                    nudge_y = -900000,
                    size = 2.75) +
    scale_y_continuous(labels = comma_format(),
                       expand = c(0,0)) +
    scale_x_discrete(breaks = seq(start_value, max_year, by = 2),
                     expand = c(0,0)) +
    labs(
      x = "Year", y = "Number of Observations"
    ) +
    theme_pilot(grid = "h",
                axes = "b") +
    theme(
      axis.line.x.bottom = element_line(linewidth = 1.2),
      text = element_text(family = "roboto"),
      title = element_text(size = 30),
      axis.text = element_text(size = 18),
      axis.title = element_text(size = 20),
      axis.text.x = element_text(vjust = -2),
      axis.title.x = element_text(vjust = -1.1),
      # NOTE: adding the coloured title in a function causes a graphics error in ggplot.
      #       might need to add this part outside of the function (if at all)
      # plot.title = ggtext::element_markdown(lineheight = 1.1),
      # legend.text = ggtext::element_markdown(size = 11)
      )

  return(p) 
  
}
