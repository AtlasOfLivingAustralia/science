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
#cumulative sum column 
  taxon <- taxon %>%
    arrange(-desc(year)) %>%
    mutate(
      total_count = cumsum(count)
    ) %>%
    arrange(desc(year))
  
  colour_1 <- paste0(bar_colour_1)
  colour_2 <- paste0(bar_colour_2)
  
  p <- taxon |>
    ggplot() + 
    geom_bar(aes(x = year, y = total_count),
             stat = "identity",
             fill = colour_1,
             size = 1,
             width = 0.98) +
    geom_bar(aes(x = year, y = count),
             stat = "identity",
             fill = colour_2,
             size = 0.5,
             width = 0.98) +
    geom_text_pilot(data = taxon |> filter(year == highlight),
                    mapping = aes(x = year,
                                  y = count,
                                  label = number(count,
                                                 accuracy = 0.1,
                                                 scale_cut = cut_short_scale())),
                    hjust = "center",
                    nudge_y = -900000,
                    size = 2.75) +
    geom_text_pilot(data = taxon |> filter(year == highlight),
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
    scale_x_discrete(breaks = c(2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021),
                     expand = c(0,0)) +
    labs(
      title = custom_title,
      x = "Year", y = "Number of Observations"
    ) +
    theme_pilot(grid = "h",
                axes = "b") +
    theme(
      axis.line.x.bottom = element_line(size = 1.2),
      text = element_text(family = "roboto"),
      title = element_text(size = 30),
      axis.text = element_text(size = 18),
      axis.title = element_text(size = 20),
      axis.text.x = element_text(vjust = -2),
      axis.title.x = element_text(vjust = -1.1),
      plot.title = element_markdown(lineheight = 1.1),
      legend.text = element_markdown(size = 11))

  return(p) 
  
}