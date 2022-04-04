# script for processing ALA election app 2022

# GALAH NOTES
library(sf)
library(wk)
library(dplyr)
library(galah)
galah_config(email = "martinjwestgate@gmail.com")
library(ggplot2)
library(readr)

# search_fields("elect") # most recent electorates are from 2021, not 2018 as in ALA

# read boundaries shapefile
boundaries_raw <- read_sf("2021-Cwlth_electoral_boundaries_ESRI/2021_ELB_region.shp")
# str(boundaries_raw)

# add states
electorate_by_state <- read_csv("electorate_by_state.csv")
# boundaries_raw <- cbind(boundaries_raw, electorate_by_state[, 2])
boundaries_raw <- left_join(boundaries_raw, electorate_by_state, by = c("Elect_div" = "electorate"))

# convert to WGS84
boundaries_wgs84 <- boundaries_raw |> st_transform(crs = st_crs("WGS84"))


in_list <- split(boundaries_wgs84, seq_len(nrow(boundaries_wgs84)))
species_list <- vector(mode = "list", length = length(in_list))

# sf::sf_use_s2(TRUE) # fix for bugs in the underlying spatial data

for(i in seq_along(in_list)){ # for each row
  
  # extract geographic info
  # remainder of pipe fixes spherical geometry failures
  a <- in_list[[i]]
  electorate <- a$geometry |> st_as_s2(rebuild = TRUE) |> st_as_sf()
  
  # get a bounding box for this polygon, convert to wkt
  wkt <- electorate |> st_bbox() |> st_as_sfc() |> st_as_text() 
  wkt <- gsub(")))", "))", wkt)
  wkt <- gsub("POLYGON ", "POLYGON", wkt)

  # download records within the bounding box
  records <- galah_call() |>
    galah_identify("animalia") |>
    galah_geolocate(wkt) |>
    galah_filter(year >= 2019, profile = "ALA") |>
    # galah_group_by("species") |>
    # atlas_counts(limit = NULL)
    atlas_occurrences()

  # convert occurrences to an sf object
  point_locs <- st_as_sf(
    records, 
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = st_crs("WGS84"))

  # spatial intersect between points and polygon
  points_check <- st_within(point_locs, electorate, prepared = TRUE)
  point_locs$within <- FALSE
  point_locs$within[as.data.frame(points_check)$row.id] <- TRUE

  # # plot to check spatial intersection is working
  # library(ggplot2)
  # ggplot() +
  #   geom_sf(data = in_list[[i]]) +
  #   geom_sf(data = point_locs, mapping = aes(color = within))
  # # looks ok

  # get counts by taxonConceptID
  result <- point_locs |>
    filter(within == TRUE) |>
    as_tibble() |>
    group_by(taxonConceptID) |>
    summarize(count = n()) |>
    mutate(electorate = a$Elect_div[1])

  # return(result)
  species_list[[i]] <- result
}
# )

# # save/load
# saveRDS(species_list, "species_list.rds")
# species_list <- readRDS("species_list.rds")

# join into a single df
species_df <- do.call(rbind, species_list)

# split by taxonConceptID and keep species that:
  # occur in >1 electorates and
  # have been observed twice or more in total and
  # have been observed at least 10 times in at least one electorate
  # have a range > 0
species_split <- split(species_df, species_df$taxonConceptID)
species_split <- species_split[unlist(lapply(species_split, function(a){
  nrow(a) > 1 & 
  sum(a$count) > 2 & 
  max(a$count) > 10 & 
  (max(a$count) - min(a$count)) > 0
}))]
species_df <- do.call(rbind, species_split)

# for each electorate, calculate the proportion of observations that each species occupies
electorate_sums <- species_df |> 
  group_by(electorate) |> 
  summarize(electorate_sum = sum(count))
species_df <- left_join(species_df, electorate_sums, by = "electorate")
species_df$prop_by_electorate <- species_df$count / species_df$electorate_sum

# test that these proportions sum to one per electorate
# species_df |> group_by(electorate) |> summarize(test = sum(prop_by_electorate))

# calculate z score on electorate-level proportions
species_df <- do.call(rbind, lapply(
  split(species_df, species_df$taxonConceptID),
  function(a){
    x <- a$prop_by_electorate
    a$z_score_proportions <- (x - mean(x)) / sd(x)
    y <- log(a$count)
    a$z_score_counts <-  (y - mean(y)) / sd(y)
    a$z_squared <- a$z_score_proportions * a$z_score_counts
    a
  }))
  
  ## OLD code  
  # total <- sum(a$count)
  # a$prop_species_obs <- (a$count / total) 
  # a$prop_obs <- (a$prop_species_obs - min(a$prop_species_obs)) * total
  # - 1/sum(a$count) ? would down-weight rare taxa


# # investigate
# ggplot(filter(species_df, electorate == "Lyons"),
#   aes(x = log10(count), y = z_score_proportions)) +
#   geom_point()


# import GRISS species to remove them from consideration
griis <- read_csv("GRIIS_Introduced_Species.csv")
species_df <- species_df[!(species_df$taxonConceptID %in% griis$guid), ]


# function to get taxon names
join_spp <- function(df){
  df <- df[df$count > 10, ]
  x <- search_identifiers(df$taxonConceptID[1:20]) |> as_tibble()
  if(!any(x$rank == "species")){
    x <- search_identifiers(df$taxonConceptID[1:50]) |> as_tibble()
  }
  if(!any(colnames(x) == "vernacular_name")){
    x$vernacular_name <- NA
  }
  x <- cbind(
    df[seq_len(nrow(x)), ], 
    x |> select(rank, phylum, class, order, family, species, vernacular_name)) |> filter(rank == "species")
  x
}

# get list of top species per electorate with added taxonomic information
electorate_list <- lapply(
  split(species_df, species_df$electorate),
  function(a){
    a <- a[order(a$z_squared, decreasing = TRUE), ] 
    join_spp(a)
  })

# electorate_list[["Lingiari"]]

# choose top-ranked species per electorate
electorate_df <- do.call(rbind, lapply(electorate_list, function(a){a[1, ]})) 

# electorate_df[, -1]


# # Note that there is a duplicate species here: (now obsolete)
# any(electorate_df$species == "Gallirallus philippensis")
# electorate_df$species[
#   electorate_df$species == "Gallirallus philippensis"] <- "Hypotaenidia philippensis"

# screen out duplicates
# if there are any ties, assign species to the electorate with the highest count
# then choose next on list for second-placed electorate
xt <- xtabs(~electorate_df$species)
duplicated_names <- names(xt[xt > 1])
for(i in seq_along(duplicated_names)){
   duplicated_rows <- which(
     electorate_df$species == duplicated_names[i])
   x <- electorate_df[duplicated_rows, ]
   replace_row <- which.min(x$z_squared)
   electorate_tr <- x$electorate[replace_row]
   electorate_options <- electorate_list[[electorate_tr]]
   available_rows <- which(
     !(electorate_options$species[-1] %in% electorate_df$species))
   chosen_row <- available_rows[1] + 1 
   electorate_df[duplicated_rows[replace_row], ] <- electorate_options[chosen_row, ]
}

## check any duplicates remaining
# any(xtabs(~electorate_df$species) > 1)

## look at groupings
# str(electorate_df)
# electorate_df[which(electorate_df$class == "Amphibia"), ]
# sort(xtabs(~class, data = electorate_df))

# add in states
electorate_df <- left_join(electorate_df, electorate_by_state, by = "electorate")

write.csv(electorate_df, "species_by_electorate.csv", row.names = FALSE)
saveRDS(electorate_df, "species.rds")


## MAPPING

# reimport common names
species_common <- read.csv("species_by_electorate_commonNames_31-03.csv")
species_common <- species_common[, colnames(species_common)!="state"]

# convert to spdf object
elect <- boundaries_raw |>
  left_join(species_common, by = c("Elect_div" = "electorate"))
  
spdf_elect <- elect |> 
  st_zm() |> 
  as_Spatial()

saveRDS(spdf_elect, "./2022_electorate_map/spatial_data.rds")

# try simplifying
# doesn't seem to noticeably reduce loading time - simplify further?
spdf_elect_simpl <- rmapshaper::ms_simplify(spdf_elect, keep = 0.01)
saveRDS(spdf_elect_simpl, "./2022_electorate_map/simpl_spatial_data.rds")
  
# run app
shiny::runApp("2022_electorate_map")
