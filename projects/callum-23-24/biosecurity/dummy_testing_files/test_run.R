# Test running koel changes
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

##### clean_names() #####
clean_names <- function(name) {
  cleaned_name <- name %>%
    gsub("\u00A0", " ", .) %>%      # remove non-ASCII whitespaces
    gsub("\n", " ", .) %>%          # replace line breaks with spaces
    gsub(";", ",", .) %>%           # replace semi-colons with commas
    gsub(" ,", ",", .) %>%          # remove spaces before commas
    gsub("\\s{2,}", " ", .) %>%     # remove multiple spaces
    gsub(",$", "", .) %>%           # remove trailing commas
    gsub(" +$", "", .) %>%          # remove trailing spaces
    gsub(",(\\w)", ", \\1", .) %>%  # add spaces between commas and text
    gsub(" sp.", "", .) %>%
    gsub(" spp.", "", .) %>%        # remove spp. and sp. abbreviations
    str_squish(.)

  return(cleaned_name)
}

##### get_species_lists2() #####
lists_df <- alerts_lookup
synonym_delimiter <- ","

combined_df <- lists_df |>
  pmap(.f = \(path, label, ...)
       read_csv(path, show_col_types = FALSE) |>
         mutate(list_name = label)) |>
  list_rbind() |>
  distinct() |>
  mutate(
    correct_name = clean_names(correct_name),
    synonyms = clean_names(synonyms)) |>       # commas
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
  mutate(search_term = clean_names(search_term)) |>
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


































##### generate_email #####
###### build_email() ######
alerts_data <- occ_media
email_list <- data.frame(email = "callum.waite@csiro.au", list = "testlist")
email_subject <- "[TEST] ALA Biosecurity Package"
email_send <- "callum.waite@csiro.au"
email_password <- "_____"
template_path <- ("C:\\Users\\WAI045\\OneDrive - CSIRO\\ALA\\Biosecurity\\biosecurity_alerts\\email_template.Rmd")
cache_path
output_path <- NULL
# set current date and time for inclusion in file names
date_time <- Sys.time() |>
  gsub("\\s", "_", x = _) |>
  gsub(":", "-", x = _)

# identify list names from alerts_data
list_names <- colnames(alerts_data)[(which(colnames(alerts_data) == "jurisdiction") + 1):
                                      (which(colnames(alerts_data) == "common_name") - 1)]
list_name <- list_names[1]

list_col <- alerts_data[[list_name]]
if (any(list_col)) {
  cat(paste0("Writing email for list: ", list_name, "\n"))
  #table_df <- build_gt_table(alerts_data |> filter(list_col), cache_path)

  df <- alerts_data |> filter(list_col)
  # get first image per record
  df <- df |>
    filter(!duplicated(recordID)) |>
    # format dates
    mutate(date_html = (format(eventDate, "%H:%M %d-%m-%Y")))

  build_map_thumbnail <- function(list_row) {
    location_data <- list_row |> st_as_sf(
      coords = c("decimalLongitude", "decimalLatitude"),
      crs = "WGS84")
    occurrence_map <- leaflet(options = leafletOptions(crs = leafletCRS(code = "WGS84"))) |>
      addTiles() |>
      #addProviderTiles(providers$Esri.WorldTopoMap) |>
      setView(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude, zoom = 14) |>
      addCircleMarkers(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude,
                       opacity = 0.75, color = "darkblue", radius = 25)
    mapshot(occurrence_map, file = paste0(cache_path, "maps/", list_row$recordID, ".png"))
  }

  # add maps
  df |>
    pmap(tibble) |>
    map(~{build_map_thumbnail(.x)}, .progress = TRUE)

  # build table info
  table_df <- df |>
    arrange(cl22, creator) |>
    mutate(
      path = here(),
      image_url = sub("thumbnail$", "original", url)
      ) |>
    mutate(
      # add common name
      species = map(
        glue(
        "<a href='https://biocache.ala.org.au/occurrences/{recordID}' target='_blank'><b><i>{correct_name}</i></b></a><br>
          Supplied as:<br><i>{provided_name}</i><br>
          Common name:<br>{common_name}
        "),
        gt::html
        ),
      observation = map(glue("
         <b>{creator}</b><br>
         {date_html}<br>
         {cw_state}<br>(
         <a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}' target='_blank'>{decimalLongitude}, {decimalLatitude}
         </a>)<br>
         <i>{dataResourceName}</i>
       "),
       gt::html
       ),
      location = map(
        glue("
          <a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}' target='_blank'>
            <img src='{cache_path}maps/{recordID}.png' width='200' height='150'
                 style='width:200px;max-width:200px;height:150px;max-height:150px;'/>
          </a>"),
        gt::html
       ),
      image = map(
        ifelse(is.na(url),
               glue("
                    <b>NO MEDIA AVAILABLE</b>"
                    ),
               glue("
                 <a href={image_url} target='_blank'>
                    <img src='{download_path}' height = '200'
                         style='max-width:267px;height:100%;max-height:200px;'>
                 </a>"
                 )),
        gt::html
        )
      ) |>
    select(species, observation, location, image)

  # render and save output
  output_file <- ifelse(
    is.null(output_path),
    paste0(cache_path, "email_", date_time, "_", list_name, ".html"),
    paste0(output_path, "html/email_", date_time, "_", list_name, ".html")
    )
  rmarkdown::render(template_path, output_file = output_file)

  recipients <- email_list |>
    filter(list == list_name | list == "universal") |>
    pull(email)
  send_email(recipients, output_file,
             email_send, email_password,
             subject = email_subject)
  } else {
    cat(paste0("No alert sent for list: ", list_name, "\n"))
    }

if (!is.null(output_path)) {
  # save out and clean up
  write_csv(alerts_data,
            file = paste0(output_path, "csv/alerts_data_", date_time, ".csv"))
  }








##### Proper test run #####
cache_path <- paste0(withr::local_tempdir(), "/")
dir.create(paste0(cache_path, "species_images"))
dir.create(paste0(cache_path, "maps"))

alerts_lookup <- collate_lists("dummy_biosecurity_lists/")
combined_species_list <- get_species_lists2(alerts_lookup)
correct_common_names <- assign_common_names(combined_species_list)
species_list <- lookup_species_count(combined_species_list, 50, 15, 0)
species_downloads <- download_records(species_list, correct_common_names, cache_path, 17, 0)
alerts_data <- species_downloads
email_list <- data.frame(email = c("callum.waite@csiro.au", "callum.waite@csiro.au"), #, "martin.westgate@csiro.au", "shandiya.balasubramaniam@csiro.au"),
                         list = c("testlist", "testlist2")) #, "universal", "universal"))
build_email(alerts_data, email_list, "[TEST - Sea birds] ALA Biosecurity Alert",
            email_send = NA, email_password = NA,
            "C:\\Users\\WAI045\\OneDrive - CSIRO\\ALA\\Biosecurity\\biosecurity_alerts\\email_template.Rmd",
            cache_path)






