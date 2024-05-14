list_taxa <- search_taxa(unique(c(collated_csvs_new$scientificName, collated_csvs_new$taxonConceptID)))

classifications <- list_taxa |> 
  filter(!is.na(scientific_name) & match_type != "vernacularMatch") |> 
  mutate(search_term = ifelse(match_type == "taxonIdMatch",
                              scientific_name, search_term)) |>
  distinct(search_term, scientific_name, taxon_concept_id) |>
  left_join(list_taxa |> distinct(taxon_concept_id, kingdom, phylum, class, order, family), by = "taxon_concept_id") |>
  select(-scientific_name) |>
  rename(scientificName = search_term, taxonConceptID_new = taxon_concept_id)

collated_csvs_new <- collated_csvs_new |> left_join(classifications, by = "scientificName") |>
  relocate(taxonConceptID_new, kingdom, phylum, class, order, family, .after = vernacularName)