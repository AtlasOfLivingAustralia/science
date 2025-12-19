# -------------------------------------------------------- #
# Title: Download record counts of species on threatened species lists in Mareeba, QLD
# Date: 2025-12-15
# -------------------------------------------------------- #


library(galah)
library(dplyr)
library(purrr)

galah_config(email = "your-email-here") # add email

## EPBC list

search_all(lists, "EPBC Act Threatened") # EPBC list

epbc_matches <- galah_call() |>
  filter(species_list_uid == dr656) |>
  group_by(taxonConceptID) |>
  geolocate(lat = -16.99,
            lon = 145.42,
            radius = 20,
            type = "radius") |>
  atlas_counts()


epbc_matches




## Queensland conservation list

# search for list ID in ALA
search_all(lists, "Queensland threatened") # Queensland conversation list

# NOTE: For some reason, species_list_uid isn't matching names properly in a 
#       galah query.
#       As a work-around, we can extract names from the full list and use them 
#       to filter our query.
#       However, there is a limit to how long a query url can be on the ALA, so 
#       we have to run multiple queries to match all the names (in a loop), 
#       then join them together at the end!

# Download list
qld_threatened_list <- search_all(lists, "queensland threatened") |> show_values()

# split names evenly (50 names each)
qld_split <- qld_threatened_list |>
  group_split(grp = as.integer(gl(n(), 50, n())), .keep = FALSE)

# make a function to query the ALA using each group of names
get_matches <- function(df) {
  
  result <- galah_call() |>
    filter(taxonConceptID == df$lsid) |> # match to taxon ID
    group_by(scientificName) |>
    geolocate(lat = -16.99,
              lon = 145.42,
              radius = 20,
              type = "radius") |>
    atlas_counts()
  
  return(result)
}


# download occurrence records in a loop
qld_matches_split <- qld_split |>
  map(
    \(df)
    get_matches(df),
    .progress = TRUE
  ) 

# merge
qld_matches <- qld_matches_split |>
  bind_rows()


qld_matches



# Save lists using write.csv()
# write.csv(data-to-save, here::here("path", "to", "file.csv")) # edit

