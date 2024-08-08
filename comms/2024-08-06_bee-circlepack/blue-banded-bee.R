# ---------------------------------------------------------------------------- #
# title: Daily observations of blue-banded bees
# author: Dax Kellie
# date: 7 August, 2024
# ---------------------------------------------------------------------------- #

# Thank you William R Chase for writing this blog...
# https://www.williamrchase.com/writing/2019-01-31-tessellated-menagerie-12-months-of-art-january

# ...which led me to discover chisato/chisatini/chichacha's work. Their website is down and are no longer posting on socials though!
# Whoever you are chichacha, thanks so much for your code!! I would have never figured this out on my own
# https://github.com/chichacha/Imperfect/blob/master/content/post/2018-12-21-bubble-packed-chart-with-r-using-packcircles-package.Rmd


# packages
library(galah)
library(imager) ## to create data frame from image
library(scales) ## rescale function is so handy!
library(packcircles) ## making circle packing easy! 
library(tidyverse)
library(showtext)
library(glue)

# get bee data
galah_config(email = "dax.kellie@csiro.au")

bee_counts <- galah_call() |>
  identify("Amegilla") |> # blue-banded bees
  filter(year > 2000) |>
  group_by(eventDate) |>
  atlas_counts()

# convert to date format
bee_counts_edited <- bee_counts |>
  mutate(eventDate = ymd(as_date(eventDate))) |>
  arrange(eventDate) |>
  mutate(id = row_number())


# load image
bee_image <- load.image(here::here("comms", "2024-08-06_bee-circlepack", "blue-bee.jpg"))
plot(bee_image) # look at it


# convert image to dataframe
bee_image_df_colour <- bee_image |>
  as.data.frame(wide = "c") |> ## so that rgb value is in separate column.
  as_tibble() |>
  rename(im_x = x,
         im_y = y) |>
  mutate(hex = rgb(c.1,c.2,c.3))

# Use circleProgressiveLayout function.
# generate circle packing layout using rbeta distribution as size of circles
pack_layout <- circleProgressiveLayout(bee_counts_edited$count, 
                                       sizetype = 'area') |> 
  ## Figure out what colour to use, so we need to scale the layout & image df to pick one colour at each circle location. 
  mutate(im_x = floor(rescale(x, to = range(bee_image_df_colour$im_x))),  
         im_y = floor(rescale(y, to = range(bee_image_df_colour$im_y))),
         ## also generate id, so we can join the data frame easily later!
         id = row_number()) |> 
  inner_join(bee_image_df_colour |> 
               select(im_x, im_y, hex), 
             by = c("im_x","im_y")
             ) |>
  left_join(bee_counts_edited,
            join_by(id))

# Create labels
# Find top 2 days with most counts, construct a label for plot 
top_day_count <- bee_counts_edited |>
  arrange(desc(count)) |> 
  slice_max(n = 2, order_by = count) |>
  mutate(day = day(eventDate),
         month = month(eventDate, abbr = TRUE, label = TRUE),
         year = year(eventDate),
         label = paste0(day, " ", month, ", ", year, "\n", count, " observations")) |>
  select(id, label)

# find suitable "1 obs" candidate
# this will act like a legend, so people can interpret all of the different sized dots
# (it needs to be in a corner spot to make it easier to read/find)
# (I just chose the top option, but you could pick whatever looks best)
pack_layout |>
  filter(im_x > 460) |>
  arrange((im_y))

one_day_count <- bee_counts_edited |>
  slice(5743) |>
  mutate(label = paste0(count, " observation")) |>
  select(id, label)

# combine so we can find the components that make up the circle (in pack layout)
day_counts <- rbind(top_day_count, one_day_count)

# attach label to all points that construct circle
days_circles <- pack_layout |>
  left_join(day_counts, join_by(id))

## Using the layout above create data frame using circleLayoutVertices function so that you can plot circle using ggplot2
data_gg <- circleLayoutVertices(days_circles) %>% 
  inner_join(days_circles %>% select(id, hex, label), by = c("id"))


# attach labels to an individual point
# (otherwise, each point of the circle will get its own label)
labels_top <- data_gg |>
  filter(!is.na(label),
         id != 5743) |>
  group_by(id) |>
  filter(row_number()==14) # choose part of circle that the line will extend from

labels_one <- data_gg |>
  filter(!is.na(label),
         id == 5743) |>
  group_by(id) |>
  filter(row_number()==1) # choose part of circle that the line will extend from


# caption
image_credits <- 
  glue("
       **Dataviz by Dax Kellie** <br><br>
       Data consists of observations of genus *Amegilla* <br>
       within the family *Apidae* (aka bees) <br><br>
       Data downloaded using the galah package <br>
       
       ") |> paste0()


# add font
font_add_google("Roboto", "roboto")
showtext_auto()

## PLOT
# Basic bee plot
p <- data_gg %>% 
  ggplot(aes(x = x,
             y = y,
             group = id)) +
  geom_polygon(aes(fill = hex)) + 
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void()


# Add labels
p + 
  ggnewscale::new_scale_colour() +
  ggnewscale::new_scale_fill() +
  # circles
  geom_polygon(data = data_gg |> filter(!is.na(label)),
               aes(x = x,
                   y = y,
                   colour = hex,
                   colour = after_scale(colorspace::darken(colour, .6))
               ),
               fill = NA,
               linewidth = 1) +
  # top count labels
  ggrepel::geom_text_repel(data = labels_top,
                           aes(label = label,
                               colour = hex,
                               colour = after_scale(colorspace::darken(colour, .6))),
                           nudge_x = c(-40, -35),
                           nudge_y = c(-50, 40),
                           segment.size = 1.05,
                           segment.curvature = c(0.02, -0.2),
                           family = "roboto") +
  # "1 observation" label
  ggrepel::geom_text_repel(data = labels_one,
                           aes(label = label,
                               colour = hex,
                               colour = after_scale(colorspace::darken(colour, .6))),
                           nudge_x = 17,
                           nudge_y = -1,
                           segment.size = 1.05,
                           segment.curvature = 0.1,
                           size = 3.6,
                           family = "roboto") +
  labs(
    title = "Daily observations of blue-banded bees",
    subtitle = "Each point represents the total occurrence records per day\nsince 1 Jan, 2001 in the Atlas of Living Australia",
    caption = image_credits
    ) +
  scale_colour_identity() +
  theme(
    legend.position = "none",
    plot.title = element_text(family = "roboto", hjust = 0.5, colour = "black", size = 19),
    plot.subtitle = element_text(family = "roboto", hjust = 0.5, colour = "grey20", size = 12, margin=margin(5,0,5,0)),
    plot.margin = unit(c(2,6,1,6),"cm"),
    panel.background = element_rect(fill = "#EB9D07", color = NA),
    plot.background = element_rect(fill = "#EB9D07", colour = NA),
    plot.caption = ggtext::element_textbox_simple(
      colour = "grey10",
      size = 8,
      halign = 1,
      lineheight = 0.25,
      margin = margin(t = 0.5,
                      r = -0.25, 
                      unit = "cm")
      )
    )


# save
showtext_opts(dpi = 320)
ggsave(here::here("comms", "2024-08-06_bee-circlepack", "plots", "blue-banded-bee.png"),
       height = 8, width = 10.5, unit = "in", dpi = 320)



