# code for basic species accumulation curves

# get a map
library(galah)
library(ozmaps)
library(sf)

aus <- ozmap_data()[1:8, ]

# create hexagonal grid
aus_grid <- sf::st_make_grid(aus,
  what = "polygons", cellsize = 0.5,
  square = FALSE, flat_topped = TRUE)

plot(aus_grid) # works
aus_grid <- st_as_sf(aus_grid)

# now try and query a single cell
grid_list <- split(aus_grid, seq_len(nrow(aus_grid)))


# pause to get observations per year
year_counts <- lapply(c(2000:2020), function(a){
  result <- ala_counts(
    taxa = select_taxa("aves"),
    filters = select_filters(
      year = a,
      cl22 = "Tasmania"
    ),
    # locations = select_locations(a),
    group_by = "species_guid",
    limit = NULL
  )
  result$year <- a
  return(result)
})

year_df <- do.call(rbind, year_counts)
all_taxa <- data.frame(species_guid = sort(unique(year_df$species_guid)))

row_list <- lapply(year_counts, function(a){
  x <- merge(all_taxa, a, all = TRUE)
  x$count[is.na(x$count)] <- 0
  return(x$count)
})

result_matrix <- do.call(rbind, row_list)


# test code for deriving species accumulation curves
library(vegan)
test <- specaccum(result_matrix)
test_fit <- fitspecaccum(test, "gleason")
# ?predict.nls

newdata <- data.frame(sites = seq_len(40))
newdata$fit <- predict(test_fit, newdata = newdata)

plot(test, xlim = c(0, 40), ylim = c(0, 350))
lines(x = newdata$sites, y = newdata$fit, col = "red")
