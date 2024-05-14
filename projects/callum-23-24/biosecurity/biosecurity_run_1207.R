# Biosecurity run with biocache not working properly

# Test running koel changes
##### Libraries #####
{
  library(remotes)
  # install_github("atlasoflivingaustralia/galah") # main branch is v 1.5.2
  library(galah)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(purrr)
  library(glue)
  library(here)
  library(janitor)
  library(tools)
  library(sf)
  library(maptiles)
  library(leaflet)
  library(mapview)
  library(webshot)
  #webshot::install_phantomjs()
  library(gt) # tables
  library(rmarkdown)
  library(htmltools)
  library(emayili)
  library(xml2)
  library(rlang)
}


##### Collate lists() ######
lists_path <- "dummy_biosecurity_lists/"
lists_path <- "biosecurity_alerts/supplied_data/lists/"
list_suffix <- "_list"

file_names <- list.files(lists_path)
file_paths <- paste0(lists_path, file_names)
labels <- gsub(paste0(list_suffix, ".csv"), "", file_names, ignore.case = T)
labels_lower <- tolower(labels)

alerts_lookup <- tibble(label = labels,
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
    gsub(" sp\\.", "", .) %>%
    gsub(" spp\\.", "", .) %>%      # remove spp. and sp. abbreviations
    str_squish(.)

  return(cleaned_name)
}

##### get_species_lists2() #####
list_df <- alerts_lookup
synonym_delimiter <- ","

combined_df <- list_df |>
  pmap(.f = \(path, label, ...)
       read_csv(path, show_col_types = FALSE) |>
         mutate(list_name = label)) |>
  list_rbind() |>
  distinct() |>
  # to clean columns for searching
  mutate(
    correct_name = clean_names(correct_name),
    synonyms = clean_names(synonyms)) |>
  # add state and/or LGA columns if not present
  (\(.) if ("state" %in% names(.)) {.}
   else {. |> tibble::add_column(state = NA)})() |>
  (\(.) if ("lga" %in% names(.)) {.}
   else {. |> tibble::add_column(lga = NA)})() |>
  (\(.) if ("shape" %in% names(.)) {.}
   else {. |> tibble::add_column(shape = NA)})() |>
  relocate(c(state, lga, shape), .after = common_name) |>
  # empty state rows (with no provided LGA or shape) default to "AUS"
  mutate(state = ifelse(is.na(state) & is.na(lga) & is.na(shape), "AUS", state),
         across(1:7, as.character))

combined_df_clean <- combined_df |>
  # split multiple synonyms
  separate_longer_delim(synonyms, synonym_delimiter) |>
  mutate(correct_name2 = correct_name) |>
  # create column for search-terms
  pivot_longer(c(correct_name2, synonyms),
               names_to = "type_of",
               values_to = "search_term") |>
  select(-type_of) |>
  relocate(search_term, .after = provided_name) |>
  filter(!is.na(search_term)) |>
  mutate(search_term = clean_names(search_term)) |>
  distinct()

# group unique species+states together from multiple lists
unique_species <- combined_df_clean |>
  select(correct_name, state, lga, shape, list_name) |>
  distinct() |>
  mutate(dummy_values = TRUE) |>
  pivot_wider(id_cols = c(correct_name, state, lga, shape),
              names_from = list_name,
              values_from = dummy_values,
              values_fill = FALSE)

combined_df_joined <- combined_df_clean |>
  left_join(unique_species, by = c("correct_name", "state", "lga", "shape")) |>
  select(-list_name) |>
  mutate(common_name = toTitleCase(common_name),
         lga = toupper(lga)) |>
  distinct() |>
  mutate(lga = ifelse(lga == "WINGECARRIBEE SHIRE COUNCIL", "Wingecarribee (A)", lga))

##### assign_common_names() #####
species_names <- combined_df_joined

# take the list of all species, and keep only species names columns
common_names <- species_names |>
  select(correct_name, common_name) |>
  group_by(correct_name) |>
  # take the first common_name present for each correct species name
  summarise(common_name = na.omit(common_name)[1], .groups = "drop") |>
  mutate(common_name = replace_na(common_name, "[Common Name Unknown]"))
###### search_name_fields() ######
search_name_fields <- function(field,
                               upload_date_start, upload_date_end,
                               event_date_start, event_date_end,
                               search_terms) {
  # field_fixed <- ifelse(field == "raw_scientificName",
  #                       "verbatimScientificName",
  #                       field)
  field_fixed <- field
  occ_search <- galah_call() |>
    galah_filter(firstLoadedDate >= upload_date_start,
                 firstLoadedDate <= upload_date_end,
                 eventDate >= event_date_start,
                 eventDate <= event_date_end,
                 {{field}} == search_terms) |>
    galah_select(raw_scientificName, scientificName, vernacularName,
                 genus, species, subspecies,
                 decimalLatitude, decimalLongitude,
                 cl22, cl10923, cl1048, cl966, cl21,
                 firstLoadedDate, basisOfRecord,
                 group = c("basic", "media")) |>
    atlas_occurrences() |>
    mutate(match = field_fixed,
           search_term = .data[[field_fixed]],
           across(everything(), as.character))
  return(occ_search)
}

##### identify_state() ######
identify_state <- function(species_records) {
  species_records |>
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
             crs = st_crs(coastal_waters_shp),
             remove = FALSE) |>
    mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
             as.integer(),
           cw_state = if_else(is.na(intersection),
                              NA,
                              coastal_waters_shp$state_abbr[intersection])) |>
    select(-intersection) |>
    st_drop_geometry()
}

##### identify_lga() ######
identify_lga <- function(species_records) {
  species_records |>
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
             crs = st_crs(LGA_shp),
             remove = FALSE) |>
    mutate(intersection = st_intersects(geometry, LGA_shp) |>
             as.integer(),
           lga_2018 = if_else(is.na(intersection),
                              NA,
                              LGA_shp$LGA_NAME18[intersection])) |>
    select(-intersection) |>
    st_drop_geometry()
}

##### identify_shape() ######
identify_shape <- function(species_records, shapes_path = NULL) {
  # first need to download all relevant shape files
  shape_names <- unique(species_records$shape) |> na.omit()
  # only proceed if we have at least one shape named and a path provided
  if (length(shape_names) == 0 | is.null(shapes_path)) {
    sr_shapes <- species_records |>
      mutate(shape_feature = NA)
  } else {
    shapefiles <- map(
      .x = shape_names,
      .f = function(shape_name) {
        st_read(paste0(shapes_path, shape_name, "/", shape_name, ".shp")) |>
          (\(.) if (all(st_is_valid(.))) . else st_make_valid(.))()
      }
    ) |>
      setNames(shape_names)
    # then figure out species_records an occurrence is in its specified shpfile
    sr_shapes <- species_records |>
      # group occurrences by the shapefile they use (or by NA if none provided)
      split(species_records |>
              mutate(shape = replace_na(shape, "empty")) |>
              pull(shape)) |>
      # iterate over each identified shapefile (one df per shp)
      map(.f = function(sr_dfs) {
        # if its the NA df then just return a column of NAs
        if (all(is.na(sr_dfs$shape))) {
          sr_dfs |>
            mutate(shape_feature = NA)
        } else {
          # otherwise, check whether each occurrence lies in the shp file,
          # and return the feature name that it lies in (change to SHAPE_NAME)
          # but FIRST check whether there is a SHAPE_NAME column
          is_SHAPE_NAME <- "SHAPE_NAME" %in% names(shapefiles[[unique(sr_dfs$shape)]])
          sr_dfs |>
            st_as_sf(
              coords = c("decimalLongitude", "decimalLatitude"),
              crs = st_crs(shapefiles[[unique(sr_dfs$shape)]]),
              remove = FALSE) |>
            mutate(intersection = st_intersects(geometry,
                                                shapefiles[[unique(shape)]]) |>
                     as.integer(),
                   shape_feature = ifelse(
                     is.na(intersection),
                     NA,
                     shape),
                   shape_feature = ifelse(
                     !is.na(shape_feature) & is_SHAPE_NAME,
                     shapefiles[[unique(shape)]]$SHAPE_NAME[intersection],
                     shape_feature)) |>
            select(-intersection) |>
            st_drop_geometry()
        }
      }) |>
      list_rbind()
  }

  return(sr_shapes)
}


##### search_occurrences() #####
species_list <- combined_df_joined
common_names
event_date_start <- 28
event_date_end <- 0
upload_date_start <- 7
upload_date_end <- 0

galah_config(
  email = Sys.getenv("ALA_EMAIL"),
  run_checks = FALSE,
  verbose = TRUE)

# manipulate date objects to create correct window
event_date_start <- ifelse(
  is.numeric(event_date_start),
  paste0(Sys.Date() - event_date_start, "T00:00:00Z"),
  paste0(dmy(event_date_start), "T00:00:00Z")
)
event_date_end <- ifelse(
  is.numeric(event_date_end),
  paste0(Sys.Date() - event_date_end + 1, "T00:00:00Z"),
  paste0(dmy(event_date_end) + 1, "T00:00:00Z")
)

upload_date_start <- ifelse(
  is.numeric(upload_date_start),
  paste0(Sys.Date() - upload_date_start, "T00:00:00Z"),
  paste0(dmy(upload_date_start), "T00:00:00Z")
)
upload_date_end <- ifelse(
  is.numeric(upload_date_end),
  paste0(Sys.Date() - upload_date_end + 1, "T00:00:00Z"),
  paste0(dmy(upload_date_end) + 1, "T00:00:00Z")
)

# remove common name duplicates
species_df <- species_list |>
  select(-common_name) |>
  distinct()

# set up divisions of 100 to search with
divisions <- seq(1, length(unique(species_df$search_term)), 100)

# set up search fields
fields <- c("scientificName", "raw_scientificName", "species", "subspecies", "genus")

# iterate search of Atlas for each search term. Store counts
species_records <- map(
  .x = 1:length(divisions),
  .f = function(num) {
    # create set of 100 search terms
    search_terms <- if (num != length(divisions)) {
      unique(species_df$search_term)[divisions[num]:(divisions[num+1] - 1)]
    } else {
      unique(species_df$search_term)[divisions[num]:length(unique(species_df$search_term))]
    }
    # search through each potential name field
    ala_search <- fields |>
      map(search_name_fields,
          event_date_start = event_date_start,
          event_date_end = event_date_end,
          upload_date_start = upload_date_start,
          upload_date_end = upload_date_end,
          search_terms = search_terms) |>
      list_rbind()
    # informative output of no. of occurrences
    cat(paste0("Names ", divisions[num], "-",
               min(divisions[num] + 99, length(unique(species_df$search_term))), ": ",
               length(unique(ala_search$recordID)), " records", "\n"))
    return(ala_search)
  }, .progress = TRUE) |>
  # turn all columns into character columns in case dfs are empty
  map(~mutate(., across(everything(), as.character))) |>
  list_rbind() |>
  # remove duplicated records
  group_by(across(-c(match, search_term))) |>
  slice_head() |>
  ungroup() |>
  mutate(eventDate = as_datetime(eventDate),
         across(c(decimalLatitude, decimalLongitude), as.numeric)) |>
  # remove duplicates of the same record for the same search term
  distinct(recordID, search_term, .keep_all = TRUE) |>
  # add on list-specific data and common names
  left_join(species_df,
            by = "search_term",
            relationship = "many-to-many") |>
  left_join(common_names,
            by = "correct_name")

cat(paste0("Total: ", length(unique(species_records$recordID)),
           " records pre location filtering \n"))

##### filter_occurrences() #####
species_records
shapes_path <- "dummy_testing_files/test_shapefiles/"
shapes_path <- "biosecurity_alerts/supplied_data/shapefiles/"

occ_list <- species_records |>
  # state-based filtering
  identify_state() |>
  identify_lga() |>
  identify_shape(shapes_path = shapes_path) |>
  # do ID'd states + LGAs match provided ones
  mutate(flagged_state = str_detect(state, cw_state),
         flagged_lga = !is.na(lga_2018) &
           (lga_2018 %in% str_split(lga, ", ")[[1]]),
         flagged_shape = !is.na(shape_feature)) |>
  # filter out occurrences not in areas of interest
  filter(state == "AUS" |
           (!is.na(state) & flagged_state) |
           (!is.na(lga) & flagged_lga) |
           (!is.na(shape) & flagged_shape)) |>
  select(-flagged_state, -flagged_lga, -flagged_shape) |>
  # filter by IBRA and IMCRA regions - may shift this line around a bit
  #filter(!is.na(cl966) | !is.na(cl1048) | !is.na(cl21)) |>
  as_tibble()

cat(paste0("Total: ", length(unique(occ_list$recordID)),
           " records post location filtering\n"))

##### download_records() #####
occ_list
dir.create("biosecurity_alerts/cache")
dir.create("biosecurity_alerts//cache/maps")
dir.create("biosecurity_alerts/cache/species_images")
cache_path <- "biosecurity_alerts/cache/"

# download records and save temp files in cache_path if they exist
occ_media <- occ_list |>
  # introduce media data (if exists) for each occurrence (time sink)
  (\(.) if (any(!is.na(.$multimedia))) search_media(.) else mutate(., creator = NA))() |>
  # keep the first media item for each record
  distinct(recordID, correct_name, provided_name, state, lga, shape, .keep_all = TRUE)

# Note that collect_media() only retains records with media - need to
# right_join to the search_media() output
occ_full <- occ_media |>
  # only collect_media if we have images present
  (\(.) if (any(!is.na(.$multimedia))) {
    collect_media(.,
                  path = paste0(cache_path, "species_images"),
                  type = "thumbnail")
  } else {
    mutate(., url = NA, download_path = NA, creator = NA)
  })() |>
  select(recordID, state, lga, shape, url, download_path, creator) |>
  right_join(occ_media, by = c("recordID", "creator", "state", "lga", "shape")) |>
  relocate(c(state, lga, shape), .before = common_name) |>
  relocate(creator, .after = basisOfRecord)

write.csv(occ_full,
          file = paste0(cache_path, "alerts_data.csv"),
          row.names = FALSE)

##### build_email() #####
alerts_data <- occ_full
template_path <- "C://Users/WAI045/OneDrive - CSIRO/ALA/Biosecurity/biosecurity_alerts/email_template.Rmd"
cache_path
output_path <- "C://Users/WAI045/OneDrive - CSIRO/ALA/Biosecurity/biosecurity_alerts/outputs/"
email_list <- readr::read_csv("C://Users/WAI045/OneDrive - CSIRO/ALA/Biosecurity/biosecurity_alerts/email_list/email_list_test.csv", show_col_types = FALSE)[c(1,3), ]
email_list <- readr::read_csv("C://Users//WAI045//OneDrive - CSIRO//ALA//Biosecurity//email_list_long.csv", show_col_types = FALSE)
email_subject <- "ALA Biosecurity Alert"
email_send = "biosecurity@ala.org.au"
email_password = NA
email_host = "smtp-relay.gmail.com"
email_port = 587
test <- FALSE

# set current date and time for inclusion in file names
date_time <- Sys.time() |>
  gsub("\\s", "_", x = _) |>
  gsub(":", "-", x = _)

# identify list names from alerts_data
list_names <- colnames(alerts_data)[(which(colnames(alerts_data) == "provided_name") + 1):
                                      (which(colnames(alerts_data) == "state") - 1)]

map(.x = list_names,
    .f = function(list_name) {
      list_col <- alerts_data[[list_name]]
      if (any(list_col)) {
        cat(paste0("Writing email for list: ", list_name, "\n"))
        table_df <- build_gt_table(alerts_data |> filter(list_col), cache_path)
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
                   email_host = email_host, email_port = email_port,
                   email_subject = email_subject,
                   test = test)
      } else {
        cat(paste0("No alert sent for list: ", list_name, "\n"))
      }
    }
)

build_gt_table <- function(df, cache_path){

  ##### Function Implementation #####
  # get first image per record
  df2 <- df |>
    filter(!duplicated(recordID)) |>
    # format dates
    mutate(date_html = (format(eventDate, "%H:%M %d-%m-%Y")))

  # add maps
  invisible(
    df2 |>
      pmap(tibble) |>
      map(~{build_map_thumbnail(.x, cache_path)})
  )

  # build table info
  table_df <- df2 |>
    arrange(scientificName, eventDate, cw_state) |>
    mutate(
      path = here(),
      image_url = sub("thumbnail$", "original", url)
    ) |>
    rowwise() |>
    mutate(
      species_names = map(
        glue(
          "<a href='https://biocache.ala.org.au/occurrences/{recordID}' target='_blank'>
            <b><i>{scientificName}</i></b></a><br>",
          "Supplied as:<br><i>{provided_name}</i><br>",
          "Common name:<br>{common_name}"
        ),
        gt::html
      ),
      observation = map(
        glue(
          if_else(is.na(creator), "", "<b>{creator}</b><br>"),
          "{date_html}<br>",
          if_else(is.na(shape), "", "<font size='-1'>{shape_feature}</font><br>"),
          if_else(is.na(lga), "", "<font size='-1'>{lga_2018}</font><br>"),
          "{cw_state}<br>",
          "(<a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}'
            target='_blank'>{decimalLongitude}, {decimalLatitude}</a>)<br>",
          "<i>{dataResourceName}</i>"
        ),
        gt::html
      ),
      location = map(
        glue(
          "<a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}'
              target='_blank'>
            <img src='{cache_path}maps/{recordID}.png' width='200' height='150'
                 style='width:200px;max-width:200px;height:150px;max-height:150px;'/>
          </a>"
        ),
        gt::html
      ),
      occ_media = map(
        glue(
          if_else(is.na(url),
                  "<b>NO MEDIA AVAILABLE</b>",
                  "<a href={image_url} target='_blank'>
                      <img src='{download_path}' height = '200'
                           style='max-width:267px;height:100%;max-height:200px;'>
                  </a>")
        ),
        gt::html
      )
    ) |>
    select(species_names, observation, location, occ_media)

  save(table_df, file = paste0(cache_path, "table_df.RData"))
  return(table_df)
}


build_map_thumbnail <- function(list_row, cache_path){
  ##### Install PhantomJS if not installed #####
  if (!is_phantomjs_installed()) {
    inform("PhantomJS will be installed using package `webshot` to facilitate map creation.")
    install_phantomjs()
  }

  ##### Function Implementation #####
  # need to add defensive programming + check for existence of the maps directory
  occurrence_map <- leaflet(options = leafletOptions(crs = leafletCRS(code = "WGS84"))) |>
    addTiles() |>
    #addProviderTiles(providers$Esri.WorldTopoMap) |>
    setView(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude, zoom = 14) |>
    addCircleMarkers(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude,
                     opacity = 0.75, color = "darkblue", radius = 25)
  mapshot(occurrence_map, file = paste0(cache_path, "maps/", list_row$recordID, ".png"))
}

###### send_email() ######
send_email <- function(recipients, output_file, email_send, email_password,
                       email_host = "smtp-relay.gmail.com", email_port = 587,
                       email_subject = "ALA Biosecurity Alert",
                       test = TRUE) {

  ##### Function Implementation #####
  if (length(recipients) == 0) {
    inform("No email recipients for this list. No email sent but .html table has been saved.")
  } else {
    if (test) {
      email <- envelope() |>
        from(email_send) |>
        bcc(recipients) |>
        subject(email_subject) |>
        emayili::html(read_html(output_file))
      # render("email_template.Rmd", include_css = "rmd")
    } else {
      email <- envelope() |>
        from(email_send) |>
        to(email_send) |>
        bcc(recipients) |>
        subject(email_subject) |>
        emayili::html(read_html(output_file))
      # render("email_template.Rmd", include_css = "rmd")
    }

    smtp <- server(
      host = email_host,
      port = email_port,
      username = email_send,
      password = email_password
    )

    smtp(email, verbose = TRUE)
  }
}





###### Testing build_gt_table() and build_map_thumbnail() ######
list_name <- list_names[1]
list_col <- alerts_data[[list_name]]
# arguments
df <- alerts_data |> filter(list_col)
cache_path

df2 <- df |>
  filter(!duplicated(recordID)) |>
  # format dates
  mutate(date_html = (format(eventDate, "%H:%M %d-%m-%Y")))

list_row <- df2[1, ]

occurrence_map <- leaflet(options = leafletOptions(crs = leafletCRS(code = "WGS84"))) |>
  addTiles() |>
  #addProviderTiles(providers$Esri.WorldTopoMap) |>
  setView(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude, zoom = 14) |>
  addCircleMarkers(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude,
                   opacity = 0.75, color = "darkblue", radius = 25)
mapshot(occurrence_map, file = paste0(cache_path, "maps/", list_row$recordID, ".png"))




##### LGA Checking #####
aus_LGAs <- search_fields("cl10923") |>
  show_values() |>
  pull(category) |>
  sort()

for (LGA in aus_LGAs) {
  matches <- stringr::str_detect(aus_LGAs, stringr::fixed(LGA))
  counts <- sum(matches)
  if (counts > 1) {
    cat(LGA, "\n")
    print(aus_LGAs[which(matches & LGA != aus_LGAs)])
  }
}

##### CW Shapefile #####
library(tidyverse)
library(sf)
library(sfheaders)

CWA <- st_read("shapefiles/CWA1980_zones/Coastal_Waters_AMB2020_Areas.shp")

coastal_waters_shp <- CWA |>
  rbind(
    CWA |>
      filter(COMMENT == "New South Wales") |>
      mutate(geometry = st_difference(sf_remove_holes(geometry),
                                      geometry),
             COMMENT = "Australian Capital Territory",
             NAME = "Coastal Waters (State Powers) Act 1980 - AMB2020 - Area - Australian Capital Territory",
             OBJNAM = NA,
             MRN = NA,
             Shape_Leng = st_length(geometry), #doesn't work for now
             Shape_Area = st_area(geometry)) #different units to rest of the column
  ) |>
  dplyr::select(COMMENT, geometry) |>
  rename(state_long = COMMENT) |>
  mutate(state_abbr = c("QLD", "NT", "WA", "SA", "NSW", "VIC", "TAS", "ACT")) |>
  relocate(state_abbr, 1)

###### LGA Shapefile #####
LGA_shp <- st_read("shapefiles/LGA2018/LGA_2018_AUST.shp")
