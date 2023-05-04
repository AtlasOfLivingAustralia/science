library(galah)
library(dplyr)
library(arrow)

# download a bunch of data and save it as a dataset 
# (cant' run this until my 25 million records download stops clogging up the queue)
# 126163 records will be downloaded - not massive but just a proof of concept
galah_call() |> 
  galah_apply_profile(ALA) |> 
  galah_identify("petroica") |> 
  galah_filter(year > 2000) |> 
  galah_select(scientificName,
               cl22,
               year,
               decimalLatitude,
               decimalLongitude,
               eventDate,
               cl1048, 
               cl966) |> 
  atlas_occurrences() |> 
  write_dataset(path = "projects/arrow_workflow/data", format = "parquet")

# access the data without reading it into R i.e. no memory issues
ds <- open_dataset("projects/arrow_workflow/data", format = "parquet")

# check records are in ibra or imcra regions (proxy for Australian records only)
# gets counts of robin species in each ibra region
robins_in_ibra <- ds |> 
  filter(!is.na(cl966) | !is.na(cl1048)) |> 
  filter(!is.na(scientificName)) |> 
  select(scientificName, 
         ibraRegion = cl1048) |> 
  count(scientificName, ibraRegion) |>
  collect()
  
# lots of other tidyverse verbs play nicely with {arrow}
# e.g. we could do something with lubridate or mutate 