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
      phylum == "Chordata", 
      # kingdom == "Animalia" | kingdom == "Plantae",
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
    group_by(species) |>
    summarize(
      electorate = a$Elect_div[1],
      kingdom = kingdom[1],
      phlum = phylum[1],
      taxonConceptID  = taxonConceptID[1],
      count = n()) |> 
    filter(grepl("biodiversity.org.au", taxonConceptID)) 

  species_list[[i]] <- result
}


# # save/load
saveRDS(species_list, "species_list.rds")
# species_list <- readRDS("species_list.rds")

# join into a single df
raw_df <- do.call(rbind, species_list)
# raw_df$group <- "vertebrates"
## OLD
# raw_df$group <- "invertebrates"
# raw_df$group[raw_df$phylum == "Chordata"] <- "vertebrates"
# raw_df$group[raw_df$kingdom == "Plantae"] <- "plants"


## CREATE SUMMARY DF TO STORE LATER RESULTS
# calculate number of species per electorate and group
electorate_df <- raw_df |> 
  group_by(electorate) |> # group 
  summarize(
    electorate_records = sum(count),
    electorate_spp = n_distinct(species))
# electorate_df <- electorate_by_group |> 
#   pivot_wider(names_from = group, 
#     values_from = c(electorate_sum, electorate_spp))
    
# # add sums per electorate WITHOUT group
# electorate_df <- raw_df |> 
#   group_by(electorate) |> 
#   summarize(
#     electorate_sum_total = sum(count),
#     electorate_spp_total = n_distinct(taxonConceptID)
#   ) |>
#   left_join(electorate_df, by = "electorate")


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
  ostrich = "urn:lsid:biodiversity.org.au:afd.taxon:44fbaf1a-a7ef-4780-8693-d342ee3aa88e",
  red_deer = "urn:lsid:biodiversity.org.au:afd.taxon:70943adf-8589-40e9-8338-7ba862074b86",
  red_fox = "urn:lsid:biodiversity.org.au:afd.taxon:2869ce8a-8212-46c2-8327-dfb7fabb8296",
  goldfinch = "urn:lsid:biodiversity.org.au:afd.taxon:402d4b2b-cfc4-47a8-8a1d-d0de3dc6191c",
  mallard = "urn:lsid:biodiversity.org.au:afd.taxon:f75e95ed-a3ab-4c51-9727-4c6686c2fb12",
  european_carp = "urn:lsid:biodiversity.org.au:afd.taxon:16171fac-8d6c-4327-9fab-f2db864d71bf",
  cat = "urn:lsid:biodiversity.org.au:afd.taxon:7de6b16a-f854-4b4b-88cf-81868ce74ad8"
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
    select(
      electorate_df,
      # filter(electorate_df, group == "vertebrates"), # prevent multiplying nrows by 3
      electorate, electorate_records
    )) |>
  mutate(prop_obs = count / electorate_records) 
  
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
  group_by(electorate) |>
  arrange(desc(z_score), .by_group = TRUE) |>
  filter(count > 10) # top-ranked species must have n > 10
  
top_species <- species_df_ordered |> 
  group_by(electorate) |>
  slice_head(n = 1)
 
# REMOVE DUPLICATES BY TAXON CONCEPT ID
calculate_xtabs <- function(){
  xt <- xtabs(~ species, data = top_species) |> 
    as.data.frame() |> 
    as_tibble() |>
    filter(Freq > 1)
  xt$species <- as.character(xt$species)
  # xt$group <- as.character(xt$group)
  arrange(xt, desc(Freq)) 
  # filter(group == "vertebrates")
}

# use while loop to iteratively reduce number of duplicates
xt <- calculate_xtabs()
run <- 1
while(nrow(xt) > 0){
  duplicated_rows <- which(top_species$species == xt$species[1])
  x <- top_species[duplicated_rows, ]
  x_order <- order(x$z_score, decreasing = TRUE)
  duplicated_rows <- duplicated_rows[x_order]
  x <- top_species[duplicated_rows, ][2, ] # this is the row to replace
  # x <- x[2, ]
  next_rows <- which(
    species_df_ordered$electorate == x$electorate 
    # species_df_ordered$group == x$group
  )
  # y <- species_df_ordered[next_rows, ]
  next_rows <- next_rows[
    !(species_df_ordered$species[next_rows] %in% top_species$species)]
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

# top_species <- filter(top_species, group == "vertebrates")
# any(is.na(top_species$taxonConceptID))
# print(top_species, n = 151)

# JOIN SPECIES INFORMATION WITH ELECTORATE DATA
top_species_ids <- search_identifiers(top_species$taxonConceptID) # up to here

top_species <- bind_cols(top_species, 
  select(as_tibble(top_species_ids), 
    -taxon_concept_id, -kingdom, - species))

# check for duplicate species names
# xt <- xtabs(~species, data = top_species )
# xt[xt > 1]
# none

# filter(top_species, species == "Eolophus roseicapilla")

# merge with electorates
electorate_df <- left_join(
  electorate_df,
  # filter(electorate_df, group == "vertebrates"),
  top_species,
  by = "electorate" # group
)

electorate_df <- left_join(electorate_df, electorate_by_state, 
   by = "electorate")


## MAPPING

# reimport common names
species_common <- read_csv("species_by_electorate_wCommonNames_final.csv") |>
  select(-electorate_records.y)
colnames(species_common)[2] <- "electorate_records"
# write.csv(species_common, "species_by_electorate.csv", row.names = FALSE)
 
 
# convert to spdf object
elect <- left_join(
  boundaries_raw,
  select(species_common, electorate, class, species, vernacular_name),
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
  
# calculate statistics
# sum(species_common$electorate_sum_total.x)

# run app
shiny::runApp("electoral-divisions-2022")

# TODO:
  # add images and common names
  # add 'click to share on social media'
  # better opening text 
  # add numbers of records per electorate and species
  # state data missing from species_common
  