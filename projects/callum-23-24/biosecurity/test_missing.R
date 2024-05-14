################### NEW WORKFLOW ###########################
##### Collate lists() ######
path <- "dummy_biosecurity_lists/"
path <- "biosecurity_alerts/supplied_data/"
list_suffix <- "_list"

file_names <- list.files(path)
file_paths <- paste0(path, file_names)
labels <- gsub(paste0(list_suffix, ".csv"), "", file_names, ignore.case = T)
labels_lower <- tolower(labels)

alerts_lookup <- data.frame(label = labels,
                            source = labels_lower,
                            path = file_paths,
                            csv_names = file_names)

##### get_species_lists2() #####
lists_df <- alerts_lookup
synonym_delimiter <- ", "

combined_df <- lists_df |>
  pmap(.f = \(path, label, ...)
       read_csv(path, show_col_types = FALSE) |>
         mutate(list_name = label)) |>
  list_rbind() |>
  distinct() |>
  mutate(correct_name = gsub("\\s{2,}", " ", correct_name),   # successive spaces
         correct_name = gsub(",", "", correct_name)) |>       # commas
  mutate(jurisdiction = replace_na(jurisdiction, "AUS"))

combined_df_clean <- combined_df |>
  separate_longer_delim(synonyms, synonym_delimiter) |>
  mutate(correct_name2 = correct_name) |>
  pivot_longer(c(correct_name2, synonyms),
               names_to = "type_of",
               values_to = "search_term") |>
  select(-type_of) |>
  relocate(search_term, .after = provided_name) |>
  filter(!is.na(search_term)) |>
  distinct()

unique_species <- combined_df_clean |>
  select(correct_name, jurisdiction, list_name) |>
  distinct() |>
  mutate(dummy_values = TRUE) |>
  pivot_wider(id_cols = c(correct_name, jurisdiction),
              names_from = list_name,
              values_from = dummy_values,
              values_fill = FALSE)

combined_df_joined <- combined_df_clean |>
  left_join(unique_species, by = c("correct_name", "jurisdiction")) |>
  select(-list_name) |>
  mutate(common_name = toTitleCase(common_name)) |>
  distinct()

##### assign_common_names() #####
species_list2 <- combined_df_joined

common_names <- species_list2 |>
  select(correct_name, common_name) |>
  group_by(correct_name) |>
  # take the first common_name present for each correct species name
  summarise(common_name = na.omit(common_name)[1], .groups = "drop")

##### get_ala_records #####
###### lookup_species_count() ######
galah_config(
  email = Sys.getenv("ALA_EMAIL"),
  run_checks = FALSE,
  verbose = TRUE)

species_list2
max_counts <- 10000
start_date <- 7
end_date <- 0

# manipulate date objects to create correct window
if (is.numeric(start_date)) {
  start_date <- as.character(Sys.Date() - start_date) |>
    paste0("T00:00:00Z")
} else if (is.character(start_date)) {
  start_date <- dmy(start_date) |>
    paste0("T00:00:00Z")
}
if (is.numeric(end_date)) {
  end_date <- as.character(Sys.Date() - end_date + 1) |>
    paste0("T00:00:00Z")
} else if (is.character(end_date)) {
  end_date <- (dmy(end_date) + 1) |>
    paste0("T00:00:00Z")
}

# record counts
species_list2 <- species_list2 |>
  select(-common_name) |>
  mutate(search_term = gsub(" sp\\.", "", search_term),
         search_term = gsub(" spp\\.", "", search_term)) |>
  distinct()

tmp_select <- galah_select(raw_scientificName, scientificName, vernacularName,
                           genus, species, subspecies,
                           group = c("basic"))
divisions <- seq(1, length(unique(species_list2$search_term)), 100)

# iterate search of Atlas for each search term. Store counts
species_counts2 <- map(
  .x = 1:length(divisions),
  .f = function(num) {

    search_terms <- if (num != length(divisions)) {
      unique(species_list2$search_term)[divisions[num]:(divisions[num+1] - 1)]
    } else {
      unique(species_list2$search_term)[divisions[num]:length(unique(species_list2$search_term))]
    }

    sn_search <- galah_call() |>
      galah_filter(eventDate >= start_date, eventDate <= end_date,
                   scientificName == search_terms) |>
      atlas_occurrences(select = tmp_select) |>
      mutate(match = "scientificName")
    rsn_search <- galah_call() |>
      galah_filter(eventDate >= start_date, eventDate <= end_date,
                   raw_scientificName == search_terms) |>
      atlas_occurrences(select = tmp_select) |>
      mutate(match = "raw_scientificName")
    vn_search <- galah_call() |>
      galah_filter(eventDate >= start_date, eventDate <= end_date,
                   vernacularName == search_terms) |>
      atlas_occurrences(select = tmp_select) |>
      mutate(match = "vernacularName")
    g_search <- galah_call() |>
      galah_filter(eventDate >= start_date, eventDate <= end_date,
                   genus == search_terms) |>
      atlas_occurrences(select = tmp_select) |>
      mutate(match = "genus")
    s_search <- galah_call() |>
      galah_filter(eventDate >= start_date, eventDate <= end_date,
                   species == search_terms) |>
      atlas_occurrences(select = tmp_select) |>
      mutate(match = "species")
    ss_search <- galah_call() |>
      galah_filter(eventDate >= start_date, eventDate <= end_date,
                   subspecies == search_terms) |>
      atlas_occurrences(select = tmp_select)  |>
      mutate(match = "subspecies")

    ala_search <- rbind(sn_search, rsn_search, vn_search, g_search, s_search, ss_search)
    ala_search <- ala_search |>
      filter(!duplicated(ala_search |> dplyr::select(-match)))

    cat(paste0(num, ": ", nrow(ala_search), "\n"))
    return(ala_search)
  },
  .progress = TRUE) |>
  map(~mutate(., across(everything(), as.character))) |>
  list_rbind() |>
  distinct() |>
  filter(!duplicated(recordID))

species_present2 <- species_list2 |>
  left_join(species_counts2, by = "search_term") |>
  filter(counts > 0 & counts < max_counts)
























###### download_ala_records() ######
species_present2
common_names
start_date <- 30
end_date <- 0

cache_path <- paste0(withr::local_tempdir(), "/")
dir.create(paste0(cache_path, "species_images"))
dir.create(paste0(cache_path, "maps"))

if (is.numeric(start_date)) {
  start_date <- as.character(Sys.Date() - start_date) |>
    paste0("T00:00:00Z")
} else if (is.character(start_date)) {
  start_date <- dmy(start_date) |>
    paste0("T00:00:00Z")
}
if (is.numeric(end_date)) {
  end_date <- as.character(Sys.Date() - end_date + 1) |>
    paste0("T00:00:00Z")
} else if (is.character(end_date)) {
  end_date <- (dmy(end_date) + 1) |>
    paste0("T00:00:00Z")
}

# record downloads
if (nrow(species_present2) > 0) {
  cat(paste0("Downloading records for ", nrow(species_present2), " species\n"))

  galah_config(
    email = Sys.getenv("ALA_EMAIL"),
    run_checks = FALSE,
    verbose = TRUE)

  occ_list <- map(
    .x = species_present2$search_term,
    .f = function(search_term) {
      cat(search_term)
      galah_call() |>
        galah_identify(search_term) |>
        galah_filter(eventDate >= start_date,
                     eventDate <= end_date) |>
        # when galah is updated at OR condition for IBRA, IMCRA
        galah_select(raw_scientificName, scientificName,
                     genus, species, subspecies,
                     decimalLatitude, decimalLongitude,
                     cl22, cl1048, cl966,
                     basisOfRecord,
                     group = c("basic", "media")) |>
        atlas_occurrences() |>
        filter(scientificName == search_term |
                 verbatimScientificName == search_term |
                 genus == search_term |
                 species == search_term |
                 subspecies == search_term)
    }) |>
    list_rbind() |>
    search_media() |>
    distinct() |>
    filter(!duplicated(recordID),
           !is.na(cl966) | !is.na(cl1048)) |>
    left_join(species_present2,
              by = c("scientificName" = "search_term"),
              relationship = "many-to-many") |>
    left_join(common_names,
              by = c("correct_name")) |>
    mutate(common_name = replace_na(common_name, "[Common Name Unknown]")) |>
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
             crs = st_crs(coastal_waters_shp),
             remove = FALSE) |>
    mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
             as.integer(),
           cw_state = if_else(is.na(intersection),
                              NA,
                              coastal_waters_shp$state_abbr[intersection])
    ) |>
    select(-c(counts, intersection)) |>
    st_drop_geometry() |>
    mutate(flagged_state = str_detect(jurisdiction, cw_state)) |>
    filter(jurisdiction == "AUS" | flagged_state) |>
    select(-flagged_state) |>
    as_tibble()

  occ_media <- occ_list |>
    collect_media(path = paste0(cache_path, "species_images"),
                  type = "thumbnail") |>
    select(recordID, jurisdiction, url, download_path) |>
    right_join(occ_list, by = c("recordID", "jurisdiction"))

  write.csv(occ_media,
            file = paste0(cache_path, "alerts_data.csv"),
            row.names = F)
} else {
  write.csv(tibble(), file = paste0(cache_path, "alerts_data.csv"))
}







test_output <- galah_call() |>
  galah_identify(tibble(genus = unique(species_list2$search_term[500:600]),
                        scientificName = unique(species_list2$search_term[500:600]))) |>
  galah_select(raw_scientificName, scientificName, vernacularName,
               genus, species, subspecies,
               group = c("basic")) |>
  # when galah is updated at OR condition for IBRA, IMCRA
  atlas_occurrences() |>
  filter(eventDate >= start_date,
         eventDate <= end_date,
         scientificName %in% species_list2$search_term |
           verbatimScientificName %in% species_list2$search_term |
           vernacularName %in% species_list2$search_term |
           genus %in% species_list2$search_term |
           species %in% species_list2$search_term |
           subspecies %in% species_list2$search_term)




#### Code to check % of occurrences being found by galah ####
#################### CURRENT WORKFLOW ###################################
##### Collate lists() ######
path <- "dummy_biosecurity_lists/"
list_suffix <- "_list"

file_names <- list.files(path)
file_paths <- paste0(path, file_names)
labels <- gsub(paste0(list_suffix, ".csv"), "", file_names, ignore.case = T)
labels_lower <- tolower(labels)
alerts_lookup <- data.frame(label = labels,
                            source = labels_lower,
                            path = file_paths,
                            csv_names = file_names)

##### get_species_lists2() #####
lists_df <- alerts_lookup
synonym_delimiter <- ", "

combined_df <- lists_df |>
  pmap(.f = \(path, label, ...)
       read_csv(path, show_col_types = FALSE) |>
         mutate(list_name = label)) |>
  list_rbind() |>
  distinct() |>
  mutate(correct_name = gsub("\\s{2,}", " ", correct_name),   # successive spaces
         correct_name = gsub(",", "", correct_name)) |>       # commas
  mutate(jurisdiction = replace_na(jurisdiction, "AUS"))

combined_df_clean <- combined_df |>
  separate_longer_delim(synonyms, synonym_delimiter) |>
  mutate(correct_name2 = correct_name) |>
  pivot_longer(c(correct_name2, synonyms),
               names_to = "type_of",
               values_to = "search_term") |>
  select(-type_of) |>
  relocate(search_term, .after = provided_name) |>
  filter(!is.na(search_term)) |>
  distinct()

unique_species <- combined_df_clean |>
  select(correct_name, jurisdiction, list_name) |>
  distinct() |>
  mutate(dummy_values = TRUE) |>
  pivot_wider(id_cols = c(correct_name, jurisdiction),
              names_from = list_name,
              values_from = dummy_values,
              values_fill = FALSE)

combined_df_joined <- combined_df_clean |>
  left_join(unique_species, by = c("correct_name", "jurisdiction")) |>
  select(-list_name) |>
  mutate(common_name = toTitleCase(common_name)) |>
  distinct()

##### assign_common_names() #####
species_list <- combined_df_joined

common_names <- species_list |>
  select(correct_name, common_name) |>
  group_by(correct_name) |>
  # take the first common_name present for each correct species name
  summarise(common_name = na.omit(common_name)[1], .groups = "drop")

##### get_ala_records #####
###### lookup_species_count() ######

species_list
max_counts <- 10000
start_date <- 30
end_date <- 0

# manipulate date objects to create correct window
if (is.numeric(start_date)) {
  start_date <- as.character(Sys.Date() - start_date) |>
    paste0("T00:00:00Z")
} else if (is.character(start_date)) {
  start_date <- dmy(start_date) |>
    paste0("T00:00:00Z")
}
if (is.numeric(end_date)) {
  end_date <- as.character(Sys.Date() - end_date + 1) |>
    paste0("T00:00:00Z")
} else if (is.character(end_date)) {
  end_date <- (dmy(end_date) + 1) |>
    paste0("T00:00:00Z")
}

# record counts
species_list <- species_list |>
  select(-common_name) |>
  distinct()

# iterate search of Atlas for each search term. Store counts
species_counts <- map(
  .x = unique(species_list$search_term),
  .f = function(search_term) {
    cat(search_term)
    ala_search <- galah_call() |>
      galah_filter(eventDate >= start_date,
                   eventDate <= end_date,
                   scientificName == search_term) |>
      # when galah is updated at OR condition for IBRA, IMCRA
      atlas_counts()
    # Ask Martin about this if-else statement
    if (any(colnames(ala_search) == "count")) {
      number_out <- ala_search$count[1]
    } else {
      number_out <- 0
    }
    cat(paste0(": ", number_out, "\n"))
    return(data.frame(search_term = search_term, counts = number_out))
  },
  .progress = TRUE) |>
  list_rbind()

species_present <- species_list |>
  left_join(species_counts, by = "search_term") |>
  filter(counts > 0 & counts < max_counts)

###### download_ala_records() ######
species_present
common_names
start_date <- 30
end_date <- 0

cache_path <- paste0(withr::local_tempdir(), "/")
dir.create(paste0(cache_path, "species_images"))
dir.create(paste0(cache_path, "maps"))

if (is.numeric(start_date)) {
  start_date <- as.character(Sys.Date() - start_date) |>
    paste0("T00:00:00Z")
} else if (is.character(start_date)) {
  start_date <- dmy(start_date) |>
    paste0("T00:00:00Z")
}
if (is.numeric(end_date)) {
  end_date <- as.character(Sys.Date() - end_date + 1) |>
    paste0("T00:00:00Z")
} else if (is.character(end_date)) {
  end_date <- (dmy(end_date) + 1) |>
    paste0("T00:00:00Z")
}

# record downloads
if (nrow(species_present) > 0) {
  cat(paste0("Downloading records for ", nrow(species_present), " species\n"))

  galah_config(
    email = Sys.getenv("ALA_EMAIL"),
    run_checks = FALSE,
    verbose = TRUE)

  occ_list <- map(
    .x = species_present$search_term,
    .f = function(search_term) {
      cat(search_term)
      galah_call() |>
        galah_filter(eventDate >= start_date,
                     eventDate <= end_date,
                     scientificName == search_term) |>
        # when galah is updated at OR condition for IBRA, IMCRA
        galah_select(raw_scientificName,
                     decimalLatitude, decimalLongitude,
                     cl22, cl1048, cl966,
                     basisOfRecord,
                     group = c("basic", "media")) |>
        atlas_occurrences()
    }) |>
    list_rbind() |>
    search_media() |>
    distinct() |>
    filter(!duplicated(recordID),
           !is.na(cl966) | !is.na(cl1048)) |>
    left_join(species_present,
              by = c("scientificName" = "search_term"),
              relationship = "many-to-many") |>
    left_join(common_names,
              by = c("correct_name")) |>
    mutate(common_name = replace_na(common_name, "[Common Name Unknown]")) |>
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
             crs = st_crs(coastal_waters_shp),
             remove = FALSE) |>
    mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
             as.integer(),
           cw_state = if_else(is.na(intersection),
                              NA,
                              coastal_waters_shp$state_abbr[intersection])
    ) |>
    select(-c(counts, intersection)) |>
    st_drop_geometry() |>
    mutate(flagged_state = str_detect(jurisdiction, cw_state)) |>
    filter(jurisdiction == "AUS" | flagged_state) |>
    select(-flagged_state) |>
    as_tibble()

  occ_media <- occ_list |>
    collect_media(path = paste0(cache_path, "species_images"),
                  type = "thumbnail") |>
    select(recordID, jurisdiction, url, download_path) |>
    right_join(occ_list, by = c("recordID", "jurisdiction"))

  write.csv(occ_media,
            file = paste0(cache_path, "alerts_data.csv"),
            row.names = F)
} else {
  write.csv(tibble(), file = paste0(cache_path, "alerts_data.csv"))
}

























