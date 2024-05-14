# Test running koel changes
##### Libraries #####
{
  library(remotes)
  install_github("atlasoflivingaustralia/galah-R@dev")
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
  library(leaflet)
  remotes::install_version("webshot2", version = "0.1.0")
  library(webshot2)
  library(htmlwidgets)
  library(gt) # tables
  library(rmarkdown)
  library(htmltools)
  library(emayili)
  library(xml2)
  library(rlang)
}

galah_config(
  email = Sys.getenv("ALA_EMAIL"),
  run_checks = FALSE,
  verbose = TRUE)

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

##### get_species_lists() #####
lists_path <- "dummy_biosecurity_lists/"
# lists_path <- "biosecurity_alerts/supplied_data/lists/"
list_suffix <- "_list"
synonym_delimiter <- ","

file_names <- list.files(lists_path)
file_paths <- paste0(lists_path, file_names)
labels <- gsub(paste0(list_suffix, ".csv"), "", file_names, ignore.case = T)
labels_lower <- tolower(labels)

list_df <- tibble(label = labels,
                  source = labels_lower,
                  path = file_paths,
                  csv_names = file_names)

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
         across(1:7, as.character)) |>
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
  mutate(common_name = toTitleCase(common_name),
         lga = toupper(lga)) |>
  distinct()

##### assign_common_names() #####
species_names <- combined_df

# take the list of all species, and keep only species names columns
common_names <- species_names |>
  select(correct_name, common_name) |>
  group_by(correct_name) |>
  # take the first common_name present for each correct species name
  summarise(common_name = na.omit(common_name)[1], .groups = "drop") |>
  mutate(common_name = replace_na(common_name, "[Common Name Unknown]"))

###### search_name_fields() ######
search_name_fields <- function(field,
                               event_date_start, event_date_end,
                               upload_date_start, upload_date_end,
                               search_terms) {
  ##### Function Implementation #####
  occ_search <- request_data() |>
    galah_filter(firstLoadedDate >= upload_date_start,
                 firstLoadedDate <= upload_date_end,
                 eventDate >= event_date_start,
                 eventDate <= event_date_end,
                 {{field}} == search_terms) |>
    galah_select(scientificName, vernacularName,
                 genus, species, subspecies,
                 decimalLatitude, decimalLongitude,
                 cl22, cl10923, cl1048, cl966, cl21,
                 firstLoadedDate, basisOfRecord,
                 group = c("basic", "media")) |>
    collect() |>
    mutate(match = field,
           search_term = .data[[field]],
           across(-c(images, sounds, videos), as.character),
           across(c(images, sounds, videos), as.list))
  return(occ_search)
}

###### identify_aus() ######
identify_aus <- function(species_records) {
  ##### Function Implementation #####
  if (nrow(species_records) == 0) {
    sr_aus <- species_records
  } else {
    sr_aus <- species_records |>
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = st_crs(aus_territory),
               remove = FALSE) |>
      mutate(intersection = st_intersects(geometry, aus_territory) |> as.logical()) |>
      filter(!is.na(intersection)) |>
      select(-intersection) |>
      st_drop_geometry()
  }

  return(sr_aus)
}

###### identify_state() ######
identify_state <- function(species_records) {
  ##### Function Implementation #####
  if (nrow(species_records) == 0) {
    sr_states <- species_records |>
      mutate(cw_state = character(0))
  } else {
    sr_states <- species_records |>
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

  return(sr_states)
}

##### identify_shape() ######
identify_shape <- function(species_records, shapes_path = NULL) {
  ##### Function Implementation #####
  # first need to download all relevant shape files
  shape_names <- unique(species_records$shape) |> na.omit()
  # only proceed if we have at least one shape named and a path provided
  if (nrow(species_records) == 0) {
    sr_shapes <- species_records |>
      mutate(shape_feature = character(0))
  } else if (length(shape_names) == 0 | is.null(shapes_path)) {
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
                     "in shape"),
                   shape_feature = ifelse(
                     shape_feature == "in shape" & is_SHAPE_NAME,
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

##### exclude_records() #####
exclude_records <- function(species_records, exclusion_prefix = "!") {
  # find recordID and list combos that have provided name begin with !
  exclusions <- species_records |>
    # identify rows to exclude by !
    filter(str_sub(provided_name, 1, 1) == exclusion_prefix) |>
    select(recordID, list_name) |>
    mutate(exclude = TRUE)

  # exclude flagged rows
  inc_species_records <- species_records |>
    left_join(exclusions, by = c("recordID", "list_name")) |>
    filter(is.na(exclude)) |>
    select(-exclude)

  return(inc_species_records)
}

##### search_occurrences() #####
species_list <- combined_df
common_names
event_date_start <- 28
event_date_end <- 0
upload_date_start <- 28
upload_date_end <- 0

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
fields <- c("genus", "species", "subspecies", "scientificName")

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
    cat(paste0("\nNames ", divisions[num], "-",
               min(divisions[num] + 99, length(unique(species_df$search_term))), ": ",
               length(unique(ala_search$recordID)), " records", "\n"))
    return(ala_search)
  }, .progress = TRUE) |>
  # turn all columns into character columns in case dfs are empty
  #map(~mutate(., across(everything(), as.character))) |>
  list_rbind() |>
  mutate(eventDate = as_datetime(eventDate),
         across(c(decimalLatitude, decimalLongitude), as.numeric)) |>
  # add on list-specific data and common names
  left_join(species_df,
            by = "search_term",
            relationship = "many-to-many") |>
  left_join(common_names,
            by = "correct_name") |>
  relocate(common_name, .after = provided_name) |>
  # remove duplicates of the same record for the same search term
  distinct(recordID, provided_name, list_name, .keep_all = TRUE)

cat(paste0("Total: ", length(unique(species_records$recordID)),
           " records pre location filtering \n"))

##### Load Shapefiles #####
load("koel/data/aus_territory.rda")
load("koel/data/coastal_waters_shp.rda")
##### filter_occurrences() #####
species_records
shapes_path <- "dummy_testing_files/test_shapefiles/"

if (nrow(species_records) == 0) {
  occ_list <- tibble()
} else {
  occ_list <- species_records |>
    # records need both latitude and longitude
    filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) |>
    # state-based filtering
    identify_aus() |>
    identify_state() |>
    identify_shape(shapes_path = shapes_path) |>
    # do ID'd states + LGAs match provided ones
    mutate(flagged_state = str_detect(state, cw_state),
           flagged_shape = !is.na(shape_feature)) |>
    rowwise() |>
    mutate(flagged_lga = !is.na(cl10923) & !is.na(lga) &
             any(cl10923 == str_split(lga, ", ")[[1]])) |>
    ungroup() |>
    # filter out occurrences not in areas of interest
    filter(state == "AUS" |
             (!is.na(state) & flagged_state) |
             (!is.na(lga) & flagged_lga) |
             (!is.na(shape) & flagged_shape)) |>
    select(-flagged_state, -flagged_lga, -flagged_shape) |>
    # filter out excluded names
    exclude_records() |>
    as_tibble()

  cat(paste0("\nTotal: ", length(unique(occ_list$recordID)),
             " records post location and exclusion filtering\n",
             "       across ", length(unique(occ_list$list_name)), " lists\n"))
}

##### media_outfiles() #####
# Construct paths to where media will be downloaded
# Returns a vector of paths; one per id
media_outfiles <- function(media_id, mimetype, path) {
  ext <- switch(mimetype,
         "image/jpeg" = ".jpg",
         "image/png" = ".png",
         "audio/mpeg" = ".mpg",
         "audio/x-wav" = ".wav",
         "audio/mp4" = ".mp4",
         "image/gif" = ".gif",
         "video/3gpp" = ".3gp",
         "video/quicktime" = ".mov",
         "audio/vnd.wave" = ".wav"
  )

  file.path(path, paste0(media_id, ext))
}

##### download_records() #####
occ_list
cache_path <- paste0(withr::local_tempdir(), "/")

dir.create(paste0(cache_path, "species_images"))
dir.create(paste0(cache_path, "maps"))

# download records and save temp files in cache_path if they exist
if (nrow(occ_list) == 0) {
  occ_full <- tibble()
} else {
  galah_config(directory = paste0(cache_path, "species_images"))
  # download records and save temp files in cache_path if they exist
  if (any(!is.na(occ_list$multimedia))) {
    occ_media <- occ_list |>
      unnest_longer(col = c(images, sounds, videos)) |>
      left_join(request_metadata() |> filter(media == occ_list) |>  collect(),
                by = c("images" = "image_id")) |>
      rename("media_id" = "images") |>
      distinct(recordID, correct_name, provided_name, state, lga, shape, list_name, .keep_all = TRUE) |>
      rowwise() |>
      mutate(download_path = if (is.na(multimedia)) {NA} else {
                              media_outfiles(media_id,
                                             mimetype,
                                             paste0(cache_path, "species_images"))}) |>
      ungroup()
    collect_media((occ_media |> filter(!is.na(multimedia))), thumbnail = TRUE)
  } else {
    occ_media <- occ_list |>
      mutate(creator = NA,
             license = NA,
             mimetype = NA,
             width = NA,
             height = NA,
             image_url = NA,
             download_path = NA)
  }
}

write.csv(occ_media,
          file = paste0(cache_path, "alerts_data.csv"),
          row.names = FALSE)

##### build_email() #####
alerts_data <- occ_media
cache_path
template_path <- "c://Users/WAI045/OneDrive - CSIRO/ALA/Biosecurity/biosecurity_alerts/email_template.Rmd"
output_path <- NULL
email_subject = "[TEST] ALA Biosecurity Alert"
email_send = NA
email_password = NA
email_host = "smtp-relay.gmail.com"
email_port = 587
test = TRUE
email_list = data.frame(email = character(), list = character())#readr::read_csv("biosecurity_alerts/email_list/email_list_test.csv", show_col_types = FALSE)[c(1),]

# set current date and time for inclusion in file names
date_time <- Sys.time() |>
  gsub("\\s", "_", x = _) |>
  gsub(":", "-", x = _)

# identify list names from alerts_data
list_names <- unique(alerts_data$list_name)

map(.x = list_names,
    .f = function(list_entry) {
      cat(paste0("Writing email for list: ", list_entry, "\n"))
      table_df <- build_gt_table(alerts_data |> filter(list_name == list_entry),
                                 cache_path)
      # render and save output
      output_file <- ifelse(
        is.null(output_path),
        paste0(cache_path, "email_", date_time, "_", list_entry, ".html"),
        paste0(output_path, "html/email_", date_time, "_", list_entry, ".html")
      )
      rmarkdown::render(template_path, output_file = output_file)

      recipients <- email_list |>
        filter(list == list_entry | list == "universal") |>
        pull(email)
      if (!is.na(email_send) & !is.na(email_password)) {
        send_email(recipients, output_file,
                   email_send, email_password,
                   email_host = email_host, email_port = email_port,
                   email_subject = email_subject,
                   test = test)
      }
    }
)


build_gt_table <- function(df, cache_path) {
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
    arrange(dataResourceName, eventDate) |>
    mutate(
      path = here(),
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
          if_else(is.na(shape) | shape_feature == "in shape",
                  "", "<font size='-1'>{shape_feature}</font><br>"),
          if_else(
            is.na(cl10923),
            if_else(
              is.na(cl966),
              if_else(
                is.na(cl21), "", "<font size='-1'>{cl21}</font><br>"),
              "<font size='-1'>{cl966}</font><br>"),
            "<font size='-1'>{cl10923}</font><br>"),
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
            <img src='{cache_path}maps/{recordID}.png' width='267' height='200'
                 style='width:267px;max-width:267px;height:200px;max-height:200px;'/>
          </a>"
        ),
        gt::html
      ),
      occ_media = map(
        glue(
          if_else(is.na(multimedia),
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

  return(table_df)
}


build_map_thumbnail <- function(list_row, cache_path) {

  ##### Function Implementation #####
  # need to add defensive programming + check for existence of the maps directory
  # occurrence_map <- leaflet(options = leafletOptions(crs = leafletCRS(code = "WGS84"))) |>
  #   addTiles() |>
  #   #addProviderTiles(providers$Esri.WorldTopoMap) |>
  #   setView(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude, zoom = 10) |>
  #   addCircleMarkers(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude,
  #                    opacity = 0.75, color = "darkblue", radius = 15)
  # mapview::mapshot(occurrence_map, file = paste0(cache_path, "maps/", list_row$recordID, ".png"))
  # occurrence_map <- mapdeck(token = key, style = mapdeck_style("outdoors")) %>%
  #   add_scatterplot(data = list_row,
  #                   lat = "decimalLatitude", lon = "decimalLongitude",
  #                   radius = 400, fill_opacity = 100) %>%
  #   mapdeck_view(location = c(list_row$decimalLongitude, list_row$decimalLatitude), zoom = 12)
  # htmlwidgets::saveWidget(widget = occurrence_map, file = paste0(cache_path, "maps/", list_row$recordID, ".html"))
  # webshot2::webshot(url = paste0(cache_path, "maps/", list_row$recordID, ".html"),
  #                   file = paste0(cache_path, "maps/", list_row$recordID, ".png"),
  #                   delay = 1, zoom = 2)

  ##### Function Implementation #####
  # check if the image has already been produced
  map_in_dir <- paste0(cache_path, "maps/", list_row$recordID, ".png") %in%
    list.files(paste0(cache_path, "maps/"))
  if (!map_in_dir) {
    occurrence_map <- leaflet(options = leafletOptions(crs = leafletCRS(code = "WGS84"))) |>
      addTiles() |>
      #addProviderTiles(providers$Esri.WorldTopoMap) |>
      setView(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude, zoom = 12) |>
      addCircleMarkers(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude,
                       opacity = 0.75, color = "darkblue", radius = 15)
    saveWidget(widget = occurrence_map,
               file = paste0(cache_path, "maps/", list_row$recordID, ".html"))
    webshot(url = paste0(cache_path, "maps/", list_row$recordID, ".html"),
            file = paste0(cache_path, "maps/", list_row$recordID, ".png"),
            delay = 1, zoom = 1)
  }
}

send_email <- function(recipients, output_file, email_send = NA, email_password = NA,
                       email_host = "smtp-relay.gmail.com", email_port = 587,
                       email_subject = "ALA Biosecurity Alert",
                       test = TRUE) {

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



# mapbox token
library(mapdeck)
library(webshot2)
key <- "pk.eyJ1IjoiY2p3YWl0ZTIzIiwiYSI6ImNsa203YXlpNjFjaGUza3A0cDVidDdkMXUifQ.BbjrBC7f-y26bxbVNJHRNQ"

###### Testing build_gt_table() and build_map_thumbnail() ######
alerts_data <- occ_full
list_names <- unique(alerts_data$list_name)
list_entry <- list_names[1]
df <- alerts_data |> filter(list_name == list_entry)
cache_path

df2 <- df |>
  filter(!duplicated(recordID)) |>
  # format dates
  mutate(date_html = (format(eventDate, "%H:%M %d-%m-%Y")))

list_row <- df2[1, ]

# occurrence_map <- leaflet(options = leafletOptions(crs = leafletCRS(code = "WGS84"))) |>
#   addTiles() |>
#   #addProviderTiles(providers$Esri.WorldTopoMap) |>
#   setView(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude, zoom = 14) |>
#   addCircleMarkers(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude,
#                    opacity = 0.75, color = "darkblue", radius = 25)
# mapshot(occurrence_map, file = paste0(cache_path, "maps/", list_row$recordID, ".png"))

occurrence_map <- mapdeck(token = key, style = mapdeck_style("outdoors")) %>%
  add_scatterplot(data = list_row,
                  lat = "decimalLatitude", lon = "decimalLongitude",
                  radius = 400, fill_opacity = 100) %>%
  mapdeck_view(location = c(list_row$decimalLongitude, list_row$decimalLatitude), zoom = 12)
htmlwidgets::saveWidget(widget = occurrence_map, file = paste0(cache_path, "maps/", list_row$recordID, ".html"))
webshot2::webshot(url = paste0(cache_path, "maps/", list_row$recordID, ".html"),
                  file = paste0(cache_path, "maps/", list_row$recordID, ".png"),
                  delay = 1)



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

##### Bird checking #####
galah_call() |>
  galah_filter(scientificName == "Thalassarche bulleri", year >= 2021) |>
  galah_select(scientificName, vernacularName,
               genus, species, subspecies,
               decimalLatitude, decimalLongitude,
               cl22, cl10923, cl1048, cl966, cl21,
               firstLoadedDate, basisOfRecord,
               group = c("basic", "media")) |>
  atlas_occurrences() -> bullers_albatross

galah_call() |>
  galah_filter(scientificName == "Onychoprion fuscatus", year >= 2022) |>
  galah_select(scientificName, vernacularName,
               genus, species, subspecies,
               decimalLatitude, decimalLongitude,
               cl22, cl10923, cl1048, cl966, cl21,
               firstLoadedDate, basisOfRecord,
               group = c("basic", "media")) |>
  atlas_occurrences() -> sooty_terns


##### Testing the workflow #####
rm(clean_names, identify_shape, identify_state, search_name_fields)
library(galah)
galah_config(
  run_checks = FALSE,
  verbose = TRUE)

lists_path <- "dummy_biosecurity_lists/"
shapes_path <- "dummy_testing_files/test_shapefiles/"
cache_path <- paste0(withr::local_tempdir(), "/")
dir.create(paste0(cache_path, "species_images"))
dir.create(paste0(cache_path, "maps"))
lists_path <- "biosecurity_alerts/supplied_data/lists/"
shapes_path <- "biosecurity_alerts/supplied_data/shapefiles/"

output_path <- NULL

species_list <- get_species_lists(lists_path)

common_names <- assign_common_names(species_list)

species_records <- search_occurrences(species_list, common_names, 28, 0, 28, 0)
occ_list <- filter_occurrences(species_records, shapes_path)
occ_full <- download_occurrences(occ_list, cache_path)

# email_list <- readr::read_csv("biosecurity_alerts/email_list/email_list_test.csv", show_col_types = FALSE)
cache_path
template_path <- "c://Users/WAI045/OneDrive - CSIRO/ALA/Biosecurity/biosecurity_alerts/email_template.Rmd"
output_path <- NULL
email_subject = "[TEST] ALA Biosecurity Alert"
email_send = NA# "biosecurity@ala.org.au"
email_password = NA#
email_host = "smtp-relay.gmail.com"
email_port = 587
test = TRUE
email_list = data.frame(email = character(), list = character())#readr::read_csv("biosecurity_alerts/email_list/email_list_test.csv", show_col_types = FALSE)[c(1),]




build_email(alerts_data = occ_full,
            cache_path = cache_path,
            #template_path = "koel/inst/rmd/email_template.Rmd",
            template_path = "biosecurity_alerts/email_template.Rmd",
            output_path = output_path,
            email_list = email_list,
            # email_subject = "[TEST] ALA Biosecurity Alert",
            # email_send = "biosecurity@ala.org.au", email_password = NA,
            # email_host = "smtp-relay.gmail.com", email_port = 587,
            test = TRUE)
