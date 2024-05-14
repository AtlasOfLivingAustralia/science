##### Testing Koel Functions Workflow #####

{
  remotes::install_github("atlasoflivingaustralia/koel")
  library(koel)
  library(galah)
  library(readr)
}

galah_config(
  email = Sys.getenv("ALA_EMAIL"), # Add email here
  run_checks = FALSE,
  verbose = TRUE)

# get_species_lists()
lists_path <- "dummy_biosecurity_lists/"
list_suffix <- "_list"
synonym_delimiter <- ","

combined_species_list <- get_species_lists(lists_path, list_suffix, synonym_delimiter)

# assign_common_names()
common_names <- assign_common_names(combined_species_list)

# search_occurrences()
event_date_start <- 35
event_date_end <- 0
upload_date_start <- 14
upload_date_end <- 0

species_records <- search_occurrences(combined_species_list, common_names,
                                      event_date_start, event_date_end,
                                      upload_date_start, upload_date_end)

# filter_occurrences()
shapes_path <- "dummy_testing_files/test_shapefiles/"

occ_list <- filter_occurrences(species_records, shapes_path)

# download_occurrences()
cache_path <- paste0(withr::local_tempdir(), "/")
dir.create(paste0(cache_path, "species_images"))
dir.create(paste0(cache_path, "maps"))

occ_media <- download_occurrences(occ_list, cache_path)

##### build_email_large() #####
alerts_data <- occ_media
records_threshold <- 40
records_per_email <- 20
cache_path
template_path <- "c://Users/WAI045/OneDrive - CSIRO/ALA/Biosecurity/biosecurity_alerts/email_template.Rmd"
output_path <- NULL
email_subject <- ""
email_send <- NA
email_password <- NA
email_host <- "smtp-relay.gmail.com"
email_port <- 587
test <- TRUE
email_list = data.frame(email = character(), list = character())

build_email_large(
  alerts_data, cache_path, records_threshold, records_per_email, template_path,
  output_path, email_list, email_subject, email_send, email_password,
  email_host, email_port, test)
