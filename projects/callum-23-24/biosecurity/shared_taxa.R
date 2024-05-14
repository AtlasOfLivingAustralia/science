library(tidyverse)

combined_species_list <- read_csv("./data/combined_list.csv")

lists_to_omit <- c("NSW_NPWS_Hawkweed",
                   "NSW_NPWS_Western_Weeds",
                   "NSW_NPWS_WesternRivers_Weeds",
                   "VIC_PlantApiary",
                   "VIC_PlantEntomology",
                   "VIC_PlantPathology",
                   "WA_DPIRD_Marine"
                   )

search_terms_by_list <- combined_species_list |>
  filter(!(list_name %in% lists_to_omit)) |>
  distinct(search_term, list_name) |>
  group_by(search_term) |>
  summarise(
    n_lists = n(),
    lists = paste(sort(list_name), collapse = ", ")
  )
  
search_terms_by_list |>
  filter(grepl("AUS_ACPPO", lists)) |>
  write_csv("../aus_acppo_shared_taxa.csv")
