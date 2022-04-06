# script for processing ALA election app 2022

# GALAH NOTES
library(sf)
library(wk)
library(dplyr)
# remotes::install_github("AtlasOfLivingAustralia/galah@development")
# necessary for galah_select, galah_profile
library(galah)
galah_config(email = "martinjwestgate@gmail.com", run_checks = FALSE)
library(ggplot2)
library(readr)
library(leaflet)
library(htmltools)
library(tidyr)

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

# iterate over electorates to import all data
in_list <- split(boundaries_wgs84, seq_len(nrow(boundaries_wgs84)))
species_list <- vector(mode = "list", length = length(in_list))

# run a loop to get raw data for each electorate
for(i in seq_along(in_list)){ # for each row
  
  # extract geographic info
  # remainder of pipe fixes spherical geometry failures
  a <- in_list[[i]]
  cat(paste0("Running loop entry #", i, ", electorate = ", a$Elect_div, "\n"))
  electorate <- a$geometry |> st_as_s2(rebuild = TRUE) |> st_as_sf()
  
  # get a bounding box for this polygon, convert to wkt
  wkt <- electorate |> st_bbox() |> st_as_sfc() |> st_as_text() 
  wkt <- gsub(")))", "))", wkt)
  wkt <- gsub("POLYGON ", "POLYGON", wkt)

  # download records within the bounding box
  records <- galah_call() |>
    galah_filter(
      year >= 2019, 
      kingdom == "Animalia" | kingdom == "Plantae",
      # species != "", # doesn't work
      profile = "ALA") |>
    galah_select(
      "decimalLongitude", "decimalLatitude", 
      "kingdom", "phylum", "species", "taxonConceptID") |>
    galah_geolocate(wkt) |>
    # atlas_counts(limit = NULL)
    atlas_occurrences() |>
    filter(species != "", phylum != "")
    
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
    group_by(kingdom, phylum, taxonConceptID) |>
    summarize(count = n()) |>
    mutate(electorate = a$Elect_div[1]) |>
    filter(grepl("biodiversity.org.au", taxonConceptID)) # restrict to taxa recognised by AFD

  species_list[[i]] <- result
}


# # save/load
# saveRDS(species_list, "species_list.rds")
# species_list <- readRDS("species_list.rds")

# join into a single df
raw_df <- do.call(rbind, species_list)
raw_df$group <- "invertebrates"
raw_df$group[raw_df$phylum == "Chordata"] <- "vertebrates"
raw_df$group[raw_df$kingdom == "Plantae"] <- "plants"


## CREATE SUMMARY DF TO STORE LATER RESULTS
# calculate number of species per electorate and group
electorate_df <- raw_df |> 
  group_by(electorate, group) |> 
  summarize(
    electorate_sum_taxon = sum(count),
    electorate_spp_taxon = n_distinct(taxonConceptID))
# electorate_df <- electorate_by_group |> 
#   pivot_wider(names_from = group, 
#     values_from = c(electorate_sum, electorate_spp))
    
# add sums per electorate WITHOUT group
electorate_df <- raw_df |> 
  group_by(electorate) |> 
  summarize(
    electorate_sum_total = sum(count),
    electorate_spp_total = n_distinct(taxonConceptID)
  ) |>
  left_join(electorate_df, by = "electorate")


## RESTRICT TAXONOMIC SCOPE
# import GRISS species to remove them from consideration
griis <- read_csv("GRIIS_Introduced_Species.csv")
# and selected other introduced species  
other_introduced <- c(
  house_mouse = "urn:lsid:biodiversity.org.au:afd.taxon:107696b5-063c-4c09-a015-6edfdb6f4d52",
  cottoon_bollworm = "urn:lsid:biodiversity.org.au:afd.taxon:643c69f3-d060-4f65-a48a-e8d8466aa60b",
  woodlouse = "urn:lsid:biodiversity.org.au:afd.taxon:4f5be6a5-41c0-4b49-ba7f-e4d693bd01a5",
  cupboard_spider = "urn:lsid:biodiversity.org.au:afd.taxon:2b7b9500-61e1-43de-8064-3a609d0bf67f",
  three_lined_potato_beetle = "urn:lsid:biodiversity.org.au:afd.taxon:a9970e29-da21-4ac7-b9ac-dbc92b35cce0",
  ostrich = "urn:lsid:biodiversity.org.au:afd.taxon:44fbaf1a-a7ef-4780-8693-d342ee3aa88e"
)
all_intro <- c(griis$guid[!is.na(griis$guid)], other_introduced)
raw_df <- filter(raw_df, !(taxonConceptID %in% all_intro))


## REMOVE ULTRA-RARE SPECIES
# split by taxonConceptID and keep species that:
  # occur in >1 electorates and
  # have been observed twice or more in total and (redundant with point below)
  # have been observed at least 10 times in at least one electorate
  # have a range > 0
species_check <- raw_df |>
  group_by(taxonConceptID) |>
  summarize(keep = 
    n_distinct(electorate) > 2 &
    max(count) >= 10 &
    (max(count) - min(count)) > 0 
  )
raw_df <- filter(raw_df, 
  taxonConceptID %in% filter(species_check, keep == TRUE)$taxonConceptID)


## CALCULATE WEIGHTS

# get sum by electorate, calculate proportions
raw_df <- raw_df |>
  left_join(
    select(electorate_df, electorate, electorate_sum_total)) |>
  mutate(prop_obs = count / electorate_sum_total) 
  
# calculate z scores on proportion data
raw_df <- raw_df  |>
  group_by(taxonConceptID) |>
  summarize(mean = mean(prop_obs), sd = sd(prop_obs)) |> 
  left_join(raw_df, by = "taxonConceptID") |>
  mutate(z_score = (prop_obs - mean) / sd) |>
  select(-mean, -sd)

  
## OLD METHOD - CALCULATE SEPARATE Z SCORES FOR ELECTORATES AND SPECIES
   
# # calculate z scores by species
# raw_df <- raw_df |>
#   group_by(taxonConceptID) |>
#   summarize(mean = mean(log(count)), sd = sd(log(count))) |>
#   left_join(raw_df, by = "taxonConceptID") |>
#   mutate(z_species = (log(count) - mean) / sd) |>
#   select(-mean, -sd)
# 
# # ditto for electorates
# raw_df <- raw_df |>
#   group_by(electorate) |>
#   summarize(mean = mean(log(count)), sd = sd(log(count))) |>
#   left_join(raw_df, by = "electorate") |>
#   mutate(z_electorate = (log(count) - mean) / sd) |>
#   select(-mean, -sd)
# 
# # add a 'combined' score
# raw_df <- mutate(raw_df, z_sum = z_species + z_electorate)


## SELECT SPECIES FOR EACH ELECTORATE AND GROUP

## choose top-ranked species per electorate and group
species_df_ordered <- raw_df |>
  group_by(electorate, group) |>
  arrange(desc(z_score), .by_group = TRUE) 
  
top_species <- species_df_ordered |> 
  group_by(electorate, group) |>
  slice_head(n = 1)
  
 
# REMOVE DUPLICATES
calculate_xtabs <- function(){
  xt <- xtabs(~ taxonConceptID + group, data = top_species) |> 
    as.data.frame() |> 
    as_tibble() |>
    filter(Freq > 1)
  xt$taxonConceptID <- as.character(xt$taxonConceptID)
  xt$group <- as.character(xt$group)
  arrange(xt, desc(Freq))
}

# use while loop to iteratively reduce number of duplicates
xt <- calculate_xtabs()
run <- 1
while(nrow(xt) > 0){
  duplicated_rows <- which(top_species$taxonConceptID == xt$taxonConceptID[1])
  x <- top_species[duplicated_rows, ]
  x_order <- order(x$z_score, decreasing = TRUE)
  duplicated_rows <- duplicated_rows[x_order]
  x <- x[x_order, ]
  x <- x[2, ]
  next_rows <- which(
    species_df_ordered$electorate == x$electorate &
    species_df_ordered$group == x$group
  )
  y <- species_df_ordered[next_rows, ]
  next_rows <- next_rows[!(y$taxonConceptID %in% top_species$taxonConceptID)]
  y <- species_df_ordered[next_rows, ]
  top_species[duplicated_rows[2], ] <- species_df_ordered[next_rows[1], ]
  species_df_ordered <- species_df_ordered[-next_rows[1], ]
  # end matters
  xt <- calculate_xtabs()
  cat(paste0("run ", run, " completed, nrow(xt) = ", nrow(xt), "\n"))
  run <- run + 1
}

## checks
# print(top_species, n = 100)
# any(xtabs(~group + electorate, data = top_species) > 1) # FALSE


# JOIN SPECIES INFORMATION WITH ELECTORATE DATA
top_species_ids <- search_identifiers(top_species$taxonConceptID) # up to here

top_species <- bind_cols(top_species, 
  select(as_tibble(top_species_ids), 
    -taxon_concept_id, -kingdom, -phylum))

electorate_df <- left_join(
  electorate_df, 
  top_species,
  by = c("electorate", "group")
)


## MAPPING

# reimport common names
# species_common <- read.csv("species_by_electorate_commonNames_31-03.csv")
species_common <- filter(electorate_df, group == "vertebrates") # temporary solution until images available

write.csv(species_common, "species_by_electorate.csv")
  
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

saveRDS(species_common, "2022_electorate_map/common_names.rds")

# # try simplifying
# # doesn't seem to noticeably reduce loading time - simplify further?
spdf_elect_simple <- rmapshaper::ms_simplify(spdf_elect, keep = 0.01)
saveRDS(spdf_elect_simple, "./2022_electorate_map/simple_spatial_data.rds")
  
# run app
shiny::runApp("2022_electorate_map")

# TODO:
  # add images and common names
  # add 'click to share on social media'
  # better opening text 
  # add numbers of records per electorate and species
  # state data missing from species_common
  
  

## BELOW HERE IS OLD

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

## test pre-loading map
# leaflet_map <- leaflet() |> 
#   fitBounds(112, -45, 154, -10) |>
#   addProviderTiles(providers$CartoDB.Positron) |>  
#   addPolygons(
#     data = spdf_elect,
#     layerId = spdf_elect$Elect_div,
#     fillColor = "white",
#     fillOpacity = .2, 
#     color = "#111111", 
#     weight = 1, 
#     stroke = TRUE,
#     highlightOptions = highlightOptions(
#       color = "#C44D34", 
#       fillColor = "#C44D34", 
#       fillOpacity = 0.6,
#       weight = 3,
#       bringToFront = TRUE),
#     label = lapply(elect$label, HTML),
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", 
#                    padding = "3px 5px"),
#       textsize = "12px",
#       direction = "auto")
#   )
# saveRDS(leaflet_map, "2022_electorate_map/leaflet_map.rds")

saveRDS(as_tibble(species_common), "2022_electorate_map/common_names.rds")

# # try simplifying
# # doesn't seem to noticeably reduce loading time - simplify further?
spdf_elect_simple <- rmapshaper::ms_simplify(spdf_elect, keep = 0.01)
saveRDS(spdf_elect_simple, "./2022_electorate_map/simple_spatial_data.rds")
  
# run app
shiny::runApp("2022_electorate_map")


# phylopic images
  # parrot http://phylopic.org/image/34d9872c-b7d0-416f-8ac6-1f9f952982c8/
  # mammal http://phylopic.org/image/295cd9f7-eef2-441e-ba7e-40c772ca7611/
  # reptile http://phylopic.org/image/0e2a08ed-13a1-4b9e-a047-ef4045e7d88f/
  # frog http://phylopic.org/image/f3630498-3396-4ae2-8501-46209a766381/
  # fish
  # butterfly http://phylopic.org/image/4275ac3e-d74c-4ca3-9189-23f2d3a83def/
  # 