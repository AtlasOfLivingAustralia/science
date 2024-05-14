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
path <- "dummy_biosecurity_lists/"
path <- "biosecurity_alerts/supplied_data/lists/"
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
    gsub(" sp\\.", "", .) %>%
    gsub(" spp\\.", "", .) %>%      # remove spp. and sp. abbreviations
    str_squish(.)

  return(cleaned_name)
}

##### get_species_lists2() #####
lists_df <- alerts_lookup
synonym_delimiter <- ","

##### Function Implementation #####
combined_df <- lists_df |>
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
  mutate(state = ifelse(is.na(state) & is.na(lga) & is.na(shape), "AUS", state))

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
  distinct()


##### assign_common_names() #####
species_list <- combined_df_joined

common_names <- species_list |>
  select(correct_name, common_name) |>
  group_by(correct_name) |>
  # take the first common_name present for each correct species name
  summarise(common_name = na.omit(common_name)[1], .groups = "drop") |>
  mutate(common_name = replace_na(common_name, "[Common Name Unknown]"))

###### search_name_fields() ######
search_name_fields <- function(field, start_date, end_date, search_terms, ...) {
  field_fixed <- ifelse(field == "raw_scientificName",
                        "verbatimScientificName",
                        field)
  occ_search <- galah_call() |>
    galah_filter(eventDate >= start_date,
                 eventDate <= end_date,
                 {{field}} == search_terms) |>
    galah_select(raw_scientificName, scientificName, vernacularName,
                 genus, species, subspecies,
                 decimalLatitude, decimalLongitude,
                 cl22, cl10923, cl1048, cl966, cl21,
                 basisOfRecord,
                 group = c("basic", "media")) |>
    atlas_occurrences() |>
    mutate(match = field_fixed,
           search_term = .data[[field_fixed]],
           across(everything(), as.character))
  return(occ_search)
}

##### get_occurrences() #####
species_list
common_names
start_date <- 30
end_date <- 0

cache_path <- paste0(withr::local_tempdir(), "/")
dir.create(paste0(cache_path, "species_images"))
dir.create(paste0(cache_path, "maps"))

shapes_path <- "dummy_testing_files/test_shapefiles/"

galah_config(
  email = Sys.getenv("ALA_EMAIL"),
  run_checks = FALSE,
  verbose = TRUE)

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
          start_date = start_date,
          end_date = end_date,
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
         across(c(decimalLatitude, decimalLongitude), as.numeric))

cat(paste0("Total: ", nrow(species_records), " records pre location filtering \n"))

##### Function to identify states ######
identify_state <- function(occ_list) {
  occ_list |>
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

##### Function to identify shapefiles #####
identify_shape <- function(occ_list, shapes_path) {
  # first need to download all relevant shape files
  shape_names <- unique(occ_list$shape) |> na.omit()
  shapefiles <- map(
    .x = shape_names,
    .f = function(shape_name) {
      st_read(paste0(shapes_path, shape_name, "/", shape_name, ".shp")) |>
        (\(.) if (all(st_is_valid(.))) . else st_make_valid(.))()
    }
  ) |>
    setNames(shape_names)
  # then figure out whether an occurrence is in its specified shpfile
  occ_list_shapes <- occ_list |>
    # group occurrences by the shapefile they use (or by NA if none provided)
    split(occ_list |>
            mutate(shape = replace_na(shape, "empty")) |>
            pull(shape)) |>
    # iterate over each identified shapefile (one df per shp)
    map(.f = function(occ_dfs) {
      # if its the NA df then just return a column of NAs
      if (all(is.na(occ_dfs$shape))) {
        occ_dfs |>
          mutate(shape_feature = NA)
      } else {
        # otherwise, check whether each occurrence lies in the shp file,
        # and return the feature name that it lies in (change to FEATURE_NAME)
        occ_dfs |>
          st_as_sf(
            coords = c("decimalLongitude", "decimalLatitude"),
            crs = st_crs(shapefiles[[unique(occ_dfs$shape)]]),
            remove = FALSE) |>
          mutate(intersection = st_intersects(geometry,
                                              shapefiles[[unique(shape)]]) |>
                   as.integer(),
                 shape_feature = if_else(
                   is.na(intersection),
                   NA,
                   shapefiles[[unique(shape)]]$SHAPE_NAME[intersection])) |>
          select(-intersection) |>
          st_drop_geometry()
      }
    }) |>
    list_rbind()

  return(occ_list_shapes)
}

# download records and save temp files in cache_path
occ_list <- species_records |>
  # remove duplicates of the same record for the same search term
  distinct(recordID, search_term, .keep_all = TRUE) |>
  # add on list-specific data and common names
  left_join(species_df,
            by = "search_term",
            relationship = "many-to-many") |>
  left_join(common_names,
            by = "correct_name") |>
  # state-based filtering
  identify_state() |>
  identify_shape(shapes_path = shapes_path) |>
  # do ID'd states + LGAs match provided ones
  mutate(flagged_state = str_detect(state, cw_state),
         flagged_lga = !is.na(cl10923) & (cl10923 %in% str_split_1(lga, " ,")),
         flagged_shape = !is.na(shape_feature)) |>
  # filter out occurrences not in areas of interest
  filter(state == "AUS" |
           (!is.na(state) & flagged_state) |
           (!is.na(lga) & flagged_lga) |
           (!is.na(shape) & flagged_shape)) |>
  select(-flagged_state, -flagged_lga, -flagged_shape) |>
  # filter by IBRA and IMCRA regions - may shift this line around a bit
  filter(!is.na(cl966) | !is.na(cl1048) | !is.na(cl21)) |>
  as_tibble() |>
  # introduce media data (if exists) for each occurrence (time sink)
  #search_media()
  (\(.) if (any(!is.na(.$multimedia))) search_media(.) else .)() |>
  # keep the first media item for each record
  distinct(recordID, correct_name, provided_name, state, lga, shape, .keep_all = TRUE)

cat(paste0("Total: ", length(unique(occ_list$recordID)), " records post location filtering \n"))

occ_media <- occ_list |>
  # only collect_media if we have images present
  (\(.) if (any(!is.na(.$multimedia))) {
    collect_media(.,
                  path = paste0(cache_path, "species_images"),
                  type = "thumbnail")
  } else {
    mutate(., url = NA, download_path = NA, creator = NA)
  })() |>
  select(recordID, state, lga, shape, url, download_path) |>
  right_join(occ_list, by = c("recordID", "state", "lga", "shape")) |>
  relocate(c(state, lga, shape), .before = common_name) |>
  relocate(creator, .after = cw_state)


##### build_email() #####
alerts_data <- occ_media
template_path <- "c://Users/WAI045/OneDrive - CSIRO/ALA/Biosecurity/biosecurity_alerts/email_template.Rmd"
cache_path
output_path <- NULL


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

        # recipients <- email_list |>
        #   filter(list == list_name | list == "universal") |>
        #   pull(email)
        # send_email(recipients, output_file,
        #            email_send, email_password,
        #            email_host = email_host, email_port = email_port,
        #            email_subject = email_subject,
        #            test = test)
      } else {
        cat(paste0("No alert sent for list: ", list_name, "\n"))
      }
    }
)

build_gt_table <- function(df, cache_path){

  ##### Defensive Programming #####
  # df
  if (!("data.frame" %in% class(df))) {
    abort("`df` argument must be a data.frame or tibble")
  } else if (nrow(df) == 0) {
    abort("`df` requires at least one row to compile a table")
  } else if (!(all(
    c(
      "recordID", "eventDate", "cl22", "creator", "url", "correct_name",
      "verbatimScientificName", "common_name", "decimalLatitude",
      "decimalLongitude", "dataResourceName",  "download_path"
    )
    %in% colnames(df)))) {
    cols_needed <- c(
      "recordID", "eventDate", "cl22", "creator", "url", "correct_name",
      "verbatimScientificName", "common_name", "decimalLatitude",
      "decimalLongitude", "dataResourceName",  "download_path"
    )
    abort(paste0("`df` requires a column named ",
                 cols_needed(which(!(cols_needed %in% col_names(df))))[1]))
  }
  # cache_path
  if (!is.character(cache_path) | substr(cache_path, nchar(cache_path), nchar(cache_path)) != "/") {
    abort("`cache_path` argument must be a string ending in '/'")
  } else if (!dir.exists(cache_path)) {
    abort("The directory specified by `cache_path` does not exist")
  } else if (!("maps" %in% list.files(cache_path))) {
    inform("No 'maps' directory exists in the provided `cache_path`. One has been created.")
    dir.create(paste0(cache_path, "maps"))
  }

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
    arrange(scientificName, eventDate, cl22) |>
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
          if_else(is.na(lga), "", "<font size='-1'>{cl10923}</font><br>"),
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

  ##### Defensive Programming #####
  # list row
  if (!("data.frame" %in% class(list_row))) {
    abort("`list_row` argument must be a data.frame or tibble")
  } else if (nrow(list_row) != 1) {
    abort("`list_row` requires exactly one row to compile a map")
  } else if (
    !(all(c("recordID", "decimalLatitude", "decimalLongitude") %in%
          colnames(list_row)))) {
    cols_needed <- c("recordID", "decimalLatitude", "decimalLongitude")
    abort(paste0("`list_row` requires a column named ",
                 cols_needed(which(!(cols_needed %in% col_names(list_row))))[1]))
  }
  # cache_path
  if (!is.character(cache_path) | substr(cache_path, nchar(cache_path), nchar(cache_path)) != "/") {
    abort("`cache_path` argument must be a string ending in '/'")
  } else if (!dir.exists(cache_path)) {
    abort("The directory specified by `cache_path` does not exist")
  } else if (!("maps" %in% list.files(cache_path))) {
    inform("No 'maps' directory exists in the provided `cache_path`. One has been created.")
    dir.create(paste0(cache_path, "maps"))
  }

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
