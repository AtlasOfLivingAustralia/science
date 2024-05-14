##### Myrtle Rust map #####

{
  library(galah)
  library(tidyverse)
  library(lubridate)
  library(ozmaps)
  library(sf)
  library(viridis)
  library(RColorBrewer)
  library(pdftools)
  library(showtext)
  library(extrafont)
}

setwd("C:/Users/WAI045/OneDrive - CSIRO/ALA/dataViz/Myrtle Rust")
loadfonts(device = "win")

galah_config(
  email = "callumwaite2000@gmail.com", # Add email here
  run_checks = FALSE,
  verbose = TRUE)

myrtle_rust <- galah_call() |>
  galah_apply_profile(ALA) |>
  galah_identify(c("Uredo rangelii", "Austropuciinia psidii")) |>
  galah_select(group = c("basic"), country, stateProvince) |>
  galah_filter(firstLoadedDate <= "2024-03-16T00:00:00Z") |>
  atlas_occurrences()

myrtle_rust_clean <- myrtle_rust |>
  filter(country == "Australia") |>
  filter(!is.na(decimalLatitude),
         !is.na(decimalLongitude),
         !is.na(eventDate)) |>
  mutate(year = year(eventDate)) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs(ozmap_states),
           remove = FALSE) |>
  arrange(year) |>
  filter(year > 2009) |>
  mutate(year = factor(year, levels = 2010:2024, ordered = TRUE))

aus <- ozmap_data(data = "states")

font_add_google("Arial")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

myrtle_rust_plot <- ggplot() +
  geom_sf(data = aus, col = "grey5", fill = "white", alpha = 1) +
  geom_sf(data = myrtle_rust_clean, aes(col = year), 
          fill = NA, size = 3, alpha = 0.75, show.legend = TRUE) +
  # scale_colour_viridis(option = "C",
  #                      breaks = 2010:2024,
  #                      guide = guide_coloursteps(
  #                        title = "Year"
  #                      )) +
  # scale_colour_gradientn(colours = brewer.pal(9, "YlOrBr")[7:3]) +
  scale_colour_manual(values = plasma(n = 15),
                      guide = guide_legend(
                        title = "Year",
                        override.aes = list(shape = 15, size = 6, alpha = 1)
                      ),
                      drop = FALSE) +
  # binned_scale(aesthetics = "color",
  #              drop = FALSE,
  #              palette = function(x) plasma(n = 15),
  #              breaks = 2009:2025,
  #              guide = guide_coloursteps(
  #                title = "Year",
  #                show.limits = TRUE
  #              )) +
  xlim(114, 155) +
  theme_void() +
  theme(
    legend.key.height = unit(c(0.1), "inches"),
    legend.key.width = unit(c(0.1), "inches"),
    legend.key.spacing.y = unit(-0.07, "inches"),
    legend.position = "right",
    legend.justification = "centre",
    text = element_text(family = "Roboto"),
    legend.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", linewidth = NA),
  )
myrtle_rust_plot

ggsave("myrtle_rust_map2.pdf", plot= myrtle_rust_plot, device = cairo_pdf,
       width = 8, height = 6, units = "in")
pdf_convert("myrtle_rust_map2.pdf",
            format = "png", dpi = 1500,
            filenames = "myrtle_rust_map2.png")
