# Title: Insecta data gaps - using galah


library(galah)
library(dplyr)

galah_config(email = "dax.kellie@csiro.au")


# collect all the taxonomic names
taxa <- search_taxa(c("Hymenoptera", "Lepidoptera", "Odonata", "Coleoptera", "Neuroptera", "Orthoptera", "Diptera",
                      "Hemiptera", "Mantodea", "Blattodea", "Trichoptera", "Ephemeroptera", "Phasmida", 
                      "Thysanoptera", "Dermaptera", "Mecoptera", "Megaloptera", "Psocodea", "Siphonaptera", "Zygentoma",
                      "Embioptera", "Archaeognatha", "Strepsiptera"))

plecoptera <- search_taxa(tibble::tibble(family = "Erebidae", genus = "Plecoptera"))

taxa_complete <- taxa |> bind_rows(plecoptera)

# download counts for each
counts <- galah_call() |>
  filter(scientificName == taxa_complete$scientific_name) |>
  group_by(scientificName) |>
  atlas_counts()

# clean names
counts_edit <- counts |>
  mutate(
    scientificName = stringr::str_to_sentence(scientificName)
  ) |>
  janitor::clean_names()


galah_call() |> 
  identify(plecoptera) |>
  filter(rank == subspecies) |>
  atlas_taxonomy()


galah_call() |>
  identify("Insecta") |>
  # group_by(species) |>
  atlas_counts(type = "species")

# a way to get record counts for every species
# NOTE: It might not be scalable, or will take a very long time to get the 
#       big groups
galah_call() |>
  identify("odonata") |>
  # filter(taxonRank == c("species", "subspecies", "infraspecies")) |>
  # group_by(taxonRank) |>
  atlas_counts(type = "species")

species <- galah_call() |>
  identify("odonata") |>
  # filter(taxonRank == c("species", "subspecies", "infraspecies")) |>
  group_by(species) |>
  atlas_occurrences()

get_counts <- function(name) {
  
  galah_call() |>
    identify({name}) |>
    group_by(scientificName) |>
    atlas_counts()
  
}

o_counts <- species |>
  pull(species) |>
  purrr::map(\(name)
             get_counts(name)) |>
  bind_rows()