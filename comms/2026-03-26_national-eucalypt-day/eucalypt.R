# Shandiya Balasubramaniam 
# 23 Macrh 2026
# Data visualisation for National Eucalypt Day 2026
# 100 years (1926-2025) of eucalypt data

library(dplyr)
library(ggplot2)
library(ggridges)
library(ggforce)
library(patchwork)
library(grid)
library(galah)
galah_config(email = Sys.getenv("ALA_EMAIL"))

# colours
sky_dark <- "#0b255d"  #"#050505"
sky_light <- "#85a7bb" #"#112642"
star_col <- "#ead9b8"
water_dark <- "#163249"
water_light <- "#2a475d" 
mountains_pal <- c("#220901", "#621708", "#941b0c", "#bc3908", "#ffb627", "#e2711d", "#ffc971" )

sky <- linearGradient(c(sky_dark, sky_light),
                      x1 = 0.5, y1 = 1, x2 = 0.5, y2 = 0.6,
                      default.units = "snpc")

water <- linearGradient(c(water_dark, water_light),
                        x1 = 0.5, y1 = 0, x2 = 0.5, y2 = 1,
                        default.units = "snpc")

# define eucalypts
euc_taxonomy <- tibble(kingdom = "Plantae",
                       genus = c("Eucalyptus",
                                 "Corymbia",
                                 "Angophora"))

# datasets for plotting
euc_counts <- galah_call() |> 
  filter(taxonConceptID %in% search_taxa(euc_taxonomy)$taxon_concept_id,
         year >= 1926,
         year <= 2025,
         !is.na(species)) |> 
  group_by(year, species) |> 
  atlas_counts() 

dominant_spp <- euc_counts |> 
  group_by(year) |> 
  slice_max(count, n = 1, with_ties = TRUE) |> 
  ungroup()

all_eucs <- galah_call() |> 
  filter(taxonConceptID %in% search_taxa(euc_taxonomy)$taxon_concept_id) |> 
  atlas_counts() 

prop_eucs <- all_eucs |> 
  rename(euc_count = count) |> 
  mutate(plant_count = galah_call() |> 
           identify("plantae") |> 
           atlas_counts() |> 
           pull(count)) |> 
  mutate(prop_euc = euc_count/plant_count)

# water ----- 
water_plot <- ggplot(dominant_spp) +
  geom_point(aes(x = as.numeric(year), y = species),
             shape = "~",
             size = 4,
             colour = "#58788f") +
  scale_x_continuous(limits = c(1925.5, 2025.5)) +
  theme_void() +
  theme(plot.background = element_rect(fill = water, colour = NA),
        panel.background = element_blank())

# stars ----------
stars_plot <- ggplot() +
  geom_point(data = dominant_spp, 
             aes(x = as.numeric(year), y = log(count)),
             shape = 8, 
             colour = star_col, 
             size = 0.8) +
  scale_x_continuous(limits = c(1925.5, 2025.5)) + 
  scale_y_continuous(limits = c(2.6, 8.7)) +
  theme_void()

# moon --------
# # moon is a 5% crescent because 5% of all plants are eucs 
# formula based on this: 
# https://math.stackexchange.com/questions/791975/distance-between-2-circles-with-same-radius-to-overlap-a-desired-percentage
# distance between centres of two circles is approx. 0.0786*r for 95% overlap

# offset the overlap a bit
circles_df <- tibble(x0 = c(0, 0.07), 
                     y0 = c(0, 0.04), 
                     r = 1, 
                     group = letters[1:2])

moon_plot <- ggplot(circles_df) +
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = group),
              colour = NA) +
  scale_fill_manual(values = list(
    "a" = linearGradient(
      c(star_col, star_col),
      x1 = 0.5, y1 = 0, x2 = 0.5, y2 = 1, 
      group = FALSE),
    "b" = linearGradient(
      c("#1e3263", "#2e4370", "#3a507a"),
      x1 = 0.5, y1 = 1, x2 = 0.5, y2 = 0,
      group = FALSE))) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")

# mountains 
winning_spp <- c("Corymbia ficifolia",
                 "Angophora costata",
                 "Eucalyptus regnans",
                 "Eucalyptus salubris",
                 "Eucalyptus erythrocorys",
                 "Eucalyptus pauciflora",
                 "Eucalyptus camaldulensis")

winning_spp_tcid <- search_taxa(winning_spp) |> 
  pull(taxon_concept_id)

winning_spp_counts <- galah_call() |> 
  filter(taxonConceptID %in% winning_spp_tcid,
         year >= 1926,
         year <= 2025,
         !is.na(species)) |> 
  group_by(year, species) |> 
  atlas_counts() 

mountains_plot <- ggplot() +
  geom_density_ridges(data = winning_spp_counts, 
                      aes(x = log(count), 
                          y = forcats::fct_relevel(species, "Eucalyptus regnans", after = 4), 
                          fill = species),
                      colour = NA,
                      scale = 12, alpha = 0.8) +
  scale_fill_manual(values = mountains_pal) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(legend.position = "none")

# plots assemble! --------
# inset moon
moon_and_stars_plot <- stars_plot + inset_element(moon_plot, 
                                                  left = 0.02, 
                                                  bottom = 0.6, 
                                                  right = 0.32, 
                                                  top = 0.9)

zero_margin_theme <- theme(plot.margin = margin(0, 0, 0, 0, "cm"),
                           panel.spacing = unit(0, "cm")) 

(moon_and_stars_plot / mountains_plot / water_plot) + 
  plot_layout(heights = c(2, 0.8, 2.5)) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = sky, colour = "NA"),
                                panel.background = element_blank())) & zero_margin_theme

ggsave("eucalypt.png", height = 8, width = 8, units = "in")
