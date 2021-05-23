## ---------------------------
## Title: Map of estimated species richness across Australia
## Author: Martin Westgate
## Date Created: 2021-05-20
## ---------------------------

# libraries
library(raster)
library(terra)
library(galah)
# library(rasterVis)
library(pbapply)

library(ggplot2)
library(viridis)
library(ggtext)

# get worldclim maps
worldclim <- rast(
  getData("worldclim", var = "bio", res = 10, path = "data"))

# subset to Australia
bbox <- ext(c(110, 155, -45, -10))
aus <- crop(worldclim, bbox)
# plot(aus)

# create grid lookup for ala records
grid <- expand.grid(
  xmin = c(100:154),
  ymin = c(-45:-9))
grid$xmax <- grid$xmin + 1
grid$ymax <- grid$ymin + 1

# subset to those over the australian land mass
point_values <- extract(aus, grid[, c("xmin", "ymin")] + 0.5)
keep_rows <-
  !apply(point_values[, -1], 1, function(a){all(is.na(a))}) &
  !(grid$ymin >= -11 & grid$xmin != 142) & # excludes offshore stuff
  !(grid$ymin == -12 & grid$xmin > 150)
grid <- grid[keep_rows, ]
point_values <- point_values[keep_rows, ]

# ggplot(grid, aes(x = xmin, y = ymin)) + geom_point() # works

# get richness calcs from ALA
wkt_list <- lapply(
  split(grid, seq_len(nrow(grid))),
  function(a){
    wkt <- paste0("POLYGON((",
      paste(
        paste(
          c(a$xmin, a$xmax, a$xmax, a$xmin, a$xmin),
          c(a$ymin, a$ymin, a$ymax, a$ymax, a$ymin),
          sep = " "),
        collapse = ","),
    "))")
  })


point_values$richness <- unlist(pblapply(wkt_list,
  function(a){
    ala_counts(location = select_locations(a), type = "species")
  }))

point_values$records <- unlist(pblapply(wkt_list,
  function(a){
    ala_counts(location = select_locations(a), type = "record")
  }))

# save data
point_values <- cbind(point_values, grid)
# saveRDS(point_values, "./data/point_values.rds")
# point_values <- readRDS("./data/point_values.rds")

# point_values <- point_values[
#   !(point_values$ymin >= -11 & point_values$xmin != 142) & # excludes offshore stuff
#   !(point_values$ymin == -12 & point_values$xmin > 150), ]
# ggplot(point_values, aes(x = xmin, y = ymin)) + geom_point() # works

# now we can build a model that includes covariates,
# but also accounts for effort
# ggplot(point_values, aes(x = log(records), y = log(richness))) +
#   geom_point() +
#   geom_smooth(method = "lm", color = "red") +
#   geom_smooth(method = "gam", color = "blue")

# try an actual model using lm
initial_formula <- formula(paste0(
  "richness ~ scale(log(records)) + ",
  paste(paste0("bio", seq_len(19)), collapse = " + ")
))

model <- step(glm(initial_formula,
  family = poisson(link = "log"),
  data = point_values))

# summary(model)
model_coefs <- as.list(coef(model))
# point_values$predicted <- predict(model)

# multiply out
model_list <- rast(lapply(
  names(model_coefs)[-c(1, 2)], function(a){
    aus[[a]] * model_coefs[[a]]
  }))


aus_surface <- sum(model_list) + model_coefs[[1]]
# plot(aus_surface)

# plot with rasterVis
# gplot(aus_surface) + geom_tile(aes(fill = value)) +
#   coord_equal()
# not a nice effect

# try to plot natively with ggplot2
raster_df <- as.data.frame(coords(aus_surface))
raster_df$prediction <- as.data.frame(aus_surface)$sum
raster_df$prediction_exp <- exp(raster_df$prediction)
# saveRDS(raster_df, "./data/raster_df.rds")
# raster_df <- readRDS("./data/raster_df.rds")

p <- ggplot(raster_df, aes(x = x, y = y, fill = prediction_exp)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis(
    trans = "log",
    breaks = seq_len(4) * 1000,
    labels = paste0(seq_len(4), "k"),
    option = "magma") +
  geom_text(
    x = 113, y = -43,
    label = "Data sources: Worldclim (worldclim.org) &\nAtlas of Living Australia (ala.org.au)",
    size = 2,
    color = "#919191",
    hjust = 0
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Number of\nSpecies",
    title = "Biodiversity in Australia<br>
      <span style='font-size:8pt;'>Estimated number of species, controlling for differences in survey effort</span>"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),
    plot.title =  element_textbox_simple(
      size = 14, lineheight = 1, padding = margin(0, 0, 5, 0)),
    axis.title = element_blank(), # element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 8)# , color = "#919191")
  )

ggsave("./plots/species_richness_plot_final.png", p,
  width = 14, height = 11, units = "cm")
