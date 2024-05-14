##### Libraries #####
{
  library(remotes)
  install_github("atlasoflivingaustralia/galah-R@dev")
  install_github("atlasoflivingaustralia/koel@galah-2.0-support")
  library(galah)
  library(koel)
  library(lubridate)
  library(purrr)
  library(readr)
  library(tidyverse)
  library(sf)
}

galah_config(email = Sys.getenv("ALA_EMAIL"), run_checks = FALSE, verbose = FALSE)

load("koel/data/coastal_waters_shp.rda")

combined_species_list <- readr::read_csv("biosecurity_alerts/data/combined_list.csv")

##### Simple collection of unique records #####

unique_records <- map(
  .x = list.files("ba_csvs/csv/", full.names = TRUE),
  .f = \(.x) read_csv(.x, show_col_types = FALSE) |>
    select(recordID, decimalLatitude, decimalLongitude, eventDate, dataResourceName, scientificName, cl22) |>
    mutate(date_sent = str_extract(.x,"\\d{4}-\\d{2}-\\d{2}"))) |>
  list_rbind() |>
  distinct()

write_csv(unique_records, file = "all_alerts_occurrences.csv")

##### Beginning to try and collate all records with all data #####

first_wed <- ymd("2022-11-16")

combined_species_list <-  read_csv("biosecurity_alerts/data/combined_list.csv")
correct_common_names <- assign_common_names(combined_species_list)

species_records <- search_occurrences(combined_species_list, correct_common_names, 
                                      event_date_start = format(first_wed - days(28), "%d/%m/%Y"), event_date_end = 0,
                                      upload_date_start = format(first_wed - days(7), "%d/%m/%Y"), upload_date_end = 0)
species_filtered <- filter_occurrences(species_records, "biosecurity_alerts/data/shapefiles/")
species_downloads <- download_occurrences(species_filtered, "all_ba_occurrences/image_cache/") |>
  mutate(firstLoadedDate = as.Date(firstLoadedDate),
         week_wed = firstLoadedDate + days((11 - wday(firstLoadedDate)) %% 7)) |>
  arrange(week_wed, eventDate)

all_ba_occurrences <- species_downloads

write_csv(all_ba_occurrences, file = "all_ba_occurrences/all_ba_occurrences.csv")

all_ba_occurrences |> 
  filter(recordID %in% unique_records$recordID) |>
  write_csv("all_ba_occurrences/all_ba_alerts_full.csv")



##### Collate all records with all columns ? #####
all_records <- map(
  .x = rev(list.files("ba_csvs/csv/", full.names = TRUE)),
  .f = function(x) {
    records <- read_csv(x, show_col_types = FALSE) |>
      mutate(across(everything(), as.character),
             date_sent = str_extract(x,"\\d{4}-\\d{2}-\\d{2}"))
  }) |>
  list_rbind() |>
  distinct() |>
  pivot_longer(cols = c("ACT_Weeds",  "AUS_ACPPO", "AUS_CEBO", "NSW_Marine", "NSW_TerrestrialPests", "NSW_Weeds", "QLD_InvasivePests&Animals", "QLD_MarinePests", "QLD_OtherAquaticPests", "QLD_PlantHealth", "VIC_PlantApiary", "VIC_PlantEntomology", "VIC_PlantPathology", "WA_Entomology", "Wingecarribee_Weed", "VIC_PlantPestDiseases_Apiary", "VIC_PlantPestDiseases_Entomology", "VIC_PlantPestDiseases_Pathology", "cebo", "acppo", "NSW_marine"),
               names_to = "list_name2", values_to = "inlist_name2") |>
  relocate(inlist_name2, .after = data_resource_uid) |>
  filter(inlist_name2 == TRUE | !is.na(list_name)) |>
  mutate(list_name = ifelse(is.na(inlist_name2), list_name, list_name2),
         mimetype = ifelse(is.na(mime_type), mimetype, mime_type),
         image_url = ifelse(is.na(url), image_url, url),
         state = ifelse(is.na(jurisdiction), state, jurisdiction)) |>
  select(-list_name2, -inlist_name2) |>
  distinct() |>
  mutate(list_name = case_when(
    list_name == "acppo" ~ "AUS_ACPPO",
    list_name == "cebo" ~ "AUS_CEBO",
    list_name == "NSW_marine" ~ "NSW_Marine",
    list_name == "VIC_PlantPestDiseases_Entomology" ~ "VIC_PlantEntomology",
    list_name == "VIC_PlantPestDiseases_Pathology" ~ "VIC_PlantPathology",
    .default = list_name
  )) |>
  distinct(recordID, list_name, .keep_all = TRUE) |>
  select(1:42)

all_records_new <- all_records |>
  split(~ recordID + list_name, drop = TRUE) |>
  map(.f = function(x) {
    na_cols <- which(is.na(x))
    na_cols_ala <- na_cols[!(na_cols %in% c(10, 24:42))]
    na_fields <- names(x)[na_cols_ala]
    ala_fields <- galah_call() |>
      galah_filter(id == x$recordID) |>
      galah_select(recordID, {{na_fields}}) |>
      atlas_occurrences() |>
      select(-recordID) |>
      mutate(across(everything(), as.character))
    if (nrow(ala_fields) == 1) {
      x[,na_cols_ala] <- ala_fields
    }
    return(x)
  }, .progress = TRUE) |>
  list_rbind() |>
  arrange(date_sent) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = st_crs(coastal_waters_shp),
           remove = FALSE) |>
  mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
           as.integer(),
         cw_state = if_else(is.na(intersection),
                            NA,
                            coastal_waters_shp$state_abbr[intersection]),
         flagged_state = str_detect(state, cw_state)) |>
  select(-intersection) |>
  st_drop_geometry()

write_csv(all_records_new, file = "all_ba_occurrences/all_alerts_complete.csv")