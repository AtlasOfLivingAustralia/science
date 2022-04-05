# script for processing ALA election app 2022

# GALAH NOTES
library(sf)
library(wk)
library(dplyr)
library(galah)
galah_config(email = "martinjwestgate@gmail.com")
library(ggplot2)
library(readr)
library(leaflet)
library(htmltools)

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

# calculate number of species per electorate
electorate_df <- species_df |> 
  group_by(electorate) |> 
  summarize(electorate_spp = n_distinct(taxonConceptID))


## RESTRICT TAXONOMIC SCOPE
# restrict to taxa recognised by AFD
species_df <- species_df[
  grepl("^urn:lsid.biodiversity.org.au", species_df$taxonConceptID), ]
  
# import GRISS species to remove them from consideration
griis <- read_csv("GRIIS_Introduced_Species.csv")
# and selected other introduced species  
other_introduced <- c(
  house_mouse = "urn:lsid:biodiversity.org.au:afd.taxon:107696b5-063c-4c09-a015-6edfdb6f4d52",
  cottoon_bollworm = "urn:lsid:biodiversity.org.au:afd.taxon:643c69f3-d060-4f65-a48a-e8d8466aa60b",
  woodlouse = "urn:lsid:biodiversity.org.au:afd.taxon:4f5be6a5-41c0-4b49-ba7f-e4d693bd01a5",
  cupboard_spider = "urn:lsid:biodiversity.org.au:afd.taxon:2b7b9500-61e1-43de-8064-3a609d0bf67f",
  three_lined_potato_beetle = "urn:lsid:biodiversity.org.au:afd.taxon:a9970e29-da21-4ac7-b9ac-dbc92b35cce0" 
)
all_intro <- c(griis$guid[!is.na(griis$guid)], other_introduced)
species_df <- species_df[!(species_df$taxonConceptID %in% all_intro), ]


## REMOVE ULTRA-RARE SPECIES
# split by taxonConceptID and keep species that:
  # occur in >1 electorates and
  # have been observed twice or more in total and
  # have been observed at least 10 times in at least one electorate
  # have a range > 0
species_split <- split(species_df, species_df$taxonConceptID)
species_split <- species_split[unlist(lapply(species_split, function(a){
  nrow(a) > 2 & 
  sum(a$count) > 2 & 
  max(a$count) > 10 & 
  (max(a$count) - min(a$count)) > 0
}))]
species_df <- do.call(rbind, species_split)


## CALCULATE WEIGHTS
# for each electorate, calculate the proportion of observations that each species occupies
electorate_sums <- species_df |> 
  group_by(electorate) |> 
  summarize(electorate_sum = sum(count))
electorate_df$electorate_sum <- electorate_sums$electorate_sum
species_df <- left_join(species_df, electorate_df, by = "electorate")


# test that these proportions sum to one per electorate
# species_df |> group_by(electorate) |> summarize(test = sum(prop_by_electorate))

# calculate z score on electorate-level proportions
z_score <- function(x){((x - mean(x)) / sd(x))}

species_df$z_species <- unlist(lapply(
  split(species_df, species_df$taxonConceptID),
  function(a){z_score(log(a$count))}))

species_df$z_electorate <- unlist(lapply(
  split(species_df, species_df$electorate),
  function(a){z_score(log(a$count))}))

names(species_df$z_species) <- NULL
names(species_df$z_electorate) <- NULL

species_df$z_sum <- species_df$z_species + species_df$z_electorate 

# function to get taxon names
join_spp <- function(df){
  df <- df[df$count > 10, ]
  nrow_vec <- if(nrow(df) < 100){seq_len(nrow(df))}else{seq_len(100)}
  x <- search_identifiers(df$taxonConceptID[nrow_vec]) |> as_tibble()
  # if(!any(x$rank == "species" & x$phylum == "Chordata")){
  #   x <- search_identifiers(df$taxonConceptID[1:100]) |> as_tibble()
  # }
  if(!any(colnames(x) == "vernacular_name")){
    x$vernacular_name <- NA
  }
  x <- cbind(
    df[seq_len(nrow(x)), ], 
    x |> select(rank, phylum, class, order, family, species, vernacular_name)) |> filter(rank == "species" & phylum == "Chordata")
  x
}

# get list of top species per electorate with added taxonomic information
electorate_list <- lapply(
  split(species_df, species_df$electorate),
  function(a){
    a <- a[order(a$z_sum, decreasing = TRUE), ] 
    join_spp(a)
  })

# electorate_list[["Lingiari"]]

# choose top-ranked species per electorate
electorate_df <- do.call(rbind, lapply(electorate_list, function(a){a[1, ]})) 




# # Note that there is a duplicate species here: (now obsolete)
# any(electorate_df$species == "Gallirallus philippensis")
# electorate_df$species[
#   electorate_df$species == "Gallirallus philippensis"] <- "Hypotaenidia philippensis"

# screen out duplicates
# if there are any ties, assign species to the electorate with the highest count
# then choose next on list for second-placed electorate
xt <- xtabs(~electorate_df$species)
duplicated_names <- names(xt[xt > 1])
electorate_df <- electorate_df[order(electorate_df$z_sum, decreasing = TRUE), ]
for(i in seq_along(duplicated_names)){
   duplicated_rows <- which(
     electorate_df$species == duplicated_names[i])
   x <- electorate_df[duplicated_rows, ]
   replace_row <- which.min(x$z_sum)
   electorate_tr <- x$electorate[replace_row]
   electorate_options <- electorate_list[[electorate_tr]]
   available_rows <- which(
     !(electorate_options$species[-1] %in% electorate_df$species)) # note: 
      # a better option might be to replace present species if the 
      # replacement in quesiton has a higher z_sum than that already in the dataset
   chosen_row <- available_rows[1] + 1 
   electorate_df[duplicated_rows[replace_row], ] <- electorate_options[chosen_row, ]
}

## check any duplicates remaining
# any(xtabs(~electorate_df$species) > 1)
# electorate_df[, -1]

## look at groupings
# str(electorate_df)
# electorate_df[which(electorate_df$class == "Amphibia"), ]
# sort(xtabs(~class, data = electorate_df))

# add in states
electorate_df <- left_join(electorate_df, electorate_by_state, by = "electorate")
electorate_df <- electorate_df[order(electorate_df$electorate), ]
write.csv(electorate_df, "species_by_electorate.csv", row.names = FALSE)
saveRDS(electorate_df, "species.rds")


## MAPPING

# reimport common names
# species_common <- read.csv("species_by_electorate_commonNames_31-03.csv")
species_common <- electorate_df # temporary solution until images available

# convert to spdf object
elect <- boundaries_raw |>
  left_join(
    species_common[, c("electorate", "species", "vernacular_name")], 
    by = c("Elect_div" = "electorate"))

elect$label <-  paste0(
  elect$Elect_div, ", ", elect$state, "<br>",
  elect$vernacular_name
)
  
spdf_elect <- elect |> 
  st_zm() |> 
  as_Spatial()

saveRDS(spdf_elect, "./2022_electorate_map/spatial_data.rds")


leaflet_map <- leaflet() |> 
  fitBounds(112, -45, 154, -10) |>
  addProviderTiles(providers$CartoDB.Positron) |>  
  addPolygons(
    data = spdf_elect,
    layerId = spdf_elect$Elect_div,
    fillColor = "white",
    fillOpacity = .2, 
    color = "#111111", 
    weight = 1, 
    stroke = TRUE,
    highlightOptions = highlightOptions(
      color = "#C44D34", 
      fillColor = "#C44D34", 
      fillOpacity = 0.6,
      weight = 3,
      bringToFront = TRUE),
    label = lapply(elect$label, HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", 
                   padding = "3px 5px"),
      textsize = "12px",
      direction = "auto")
  )

# saveRDS(leaflet_map, "2022_electorate_map/leaflet_map.rds")
saveRDS(as_tibble(species_common), "2022_electorate_map/common_names.rds")


# # try simplifying
# # doesn't seem to noticeably reduce loading time - simplify further?
spdf_elect_simpl <- rmapshaper::ms_simplify(spdf_elect, keep = 0.01)
saveRDS(spdf_elect_simpl, "./2022_electorate_map/simpl_spatial_data.rds")
  
# run app
shiny::runApp("2022_electorate_map")
