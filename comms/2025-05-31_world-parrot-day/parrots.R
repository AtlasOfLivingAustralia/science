library(galah)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(monochromeR)
library(grid)
library(ggridges)
library(patchwork)

galah_config(email = Sys.getenv("ALA_EMAIL"))
source("clifford.R")

# birds in the sky --------
# 1. get status of parrots
# epbc list downloaded from the ALA 
# https://lists.ala.org.au/speciesListItem/list/dr656
epbc <- read_csv("epbc_20250405.csv") |> 
  filter(family == "Cacatuidae" | family == "Psittacidae" | family == "Nestoridae") |> 
  select(family, scientificName, status)

parrot_counts <- galah_call() |> 
  identify("psittaciformes") |> 
  apply_profile(ALA) |> 
  group_by(family, species) |> 
  atlas_counts() 

parrot_status <- parrot_counts |> 
  ungroup() |> 
  left_join(epbc, by = join_by(family, species == scientificName)) |> 
  mutate(status = replace_na(status, "Unlisted")) |> 
  group_by(status) |> 
  summarise(counts = sum(count), .groups = "drop") |> 
  filter(status != "Extinct") |> 
  mutate(approx_points = round(counts / 10000))

point_count <- sum(parrot_status$approx_points)

# 2. position parrots in the sky
df <- clifford(n = point_count, a = -0.74, b = -0.87, c = 2.46, d = -1.5)
nrow(distinct(df)) == point_count # check no points are overlapping

# mountains (extinct parrots) ------
extinct_parrots <- epbc |> 
  filter(status == "Extinct") |> 
  pull(scientificName)

extinct <- galah_call() |> 
  identify(extinct_parrots) |>
  select(year, species) |> 
  atlas_occurrences() |> 
  filter(!is.na(year))

# plot everything -------
# colours for plotting
olive_pal <- generate_palette("olivedrab", 
                              modification = "go_both_ways", 
                              n_colours = 9, 
                              view_palette = TRUE, 
                              view_labels = FALSE)

purple_pal <- generate_palette("#810F7C", 
                              modification = "go_both_ways", 
                              n_colours = 9, 
                              view_palette = TRUE, 
                              view_labels = FALSE)

pink <- "#f8dfde"

blue <- "#008294"

# 1. plot parrots in the sky
birds_plot <- df |> 
  mutate(status = case_when(
    y > 0 & x < -0.06 ~ "critically endangered",
    y > 0 & x < 0.031 ~ "vulnerable",
    y > 0 & x < 0.45 ~ "endangered",
    .default = "unlisted")) |> 
  ggplot() +
  geom_jitter(aes(x = x, y = y, colour = status),
              height = 0.1,
              width = 0.1,
              size = 0.5) +
  scale_color_manual(values = purple_pal[c(1, 3, 5, 2)]) +
  theme_void() +
  theme(legend.position = "none")

# 2. plot gradient for the sky (sunset)
bg <- linearGradient(c(pink, blue),
                     x1 = 0.4, y1 = 0, x2 = 0.2, y2 = 1,
                     default.units = "snpc")

# 3. plot mountains 
# Note that this does not check the validity of records. It looks like a bunch
# of records of extinct parrots from QLD and NSW were wrongly dated and to keep
# the viz strictly accurate, records from after 1930 should probably be removed.
# Noting this here because I realised it after creating the viz
extinct_plot <- ggplot(extinct,
                       aes(x = year, y = species, fill = species)) +
  geom_density_ridges(scale = 10, 
                      bandwidth = 24,
                      linewidth = 0.5, 
                      colour = pink, 
                      rel_min_height = 0.01) +
  scale_fill_manual(values = olive_pal[7:5]) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(legend.position = "none")

# 4. put it all together
birds_plot / extinct_plot + 
  plot_layout(heights = c(1.3, 0.1)) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = bg, colour = "NA"),
                                panel.background = element_blank()))

ggsave("parrot_dot.png", width = 8, height = 8, units = "in")
