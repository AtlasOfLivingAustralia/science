# cumulative species counts over time as a streamplot

# remotes::install_github("AtlasOfLivingAustralia/galah@ala_counts")
library(galah)
library(ggplot2)
library(ggstream)
library(viridis)

galah_config(email = "martinjwestgate@gmail.com")

# build filters for each clade
filter_list <- list(
  vertebrates = select_filters(
    taxonConceptID = select_taxa("Chordata")$taxon_concept_id),
  invertebrates = select_filters(
      taxonConceptID = select_taxa("Animalia")$taxon_concept_id,
      taxonConceptID != select_taxa("Chordata")$taxon_concept_id),
  plants = select_filters(
    taxonConceptID = select_taxa("Plantae")$taxon_concept_id),
  fungi = select_filters(
    taxonConceptID = select_taxa("Fungi")$taxon_concept_id),
  other = select_filters(
    taxonConceptID != select_taxa("Animalia")$taxon_concept_id,
    taxonConceptID != select_taxa("Plantae")$taxon_concept_id,
    taxonConceptID != select_taxa("Fungi")$taxon_concept_id)
  )

year_values <- c(2000:2020)
all_years <- select_filters(paste("year", year_values, sep = " <= "))
all_years_list <- split(all_years, seq_len(nrow(all_years)))

result_list <- lapply(seq_along(filter_list), function(a){
  data.frame(
    group = names(filter_list)[a],
    year = year_values,
    cumulative_spp = unlist(lapply(all_years_list, function(b){
      lookup <- rbind(filter_list[[a]], b)
      ala_counts(filters = lookup, type = "species")
    }))
  )
})

cumulative_species <- do.call(rbind, result_list)
# saveRDS(cumulative_species, "./data/cumulative_species.rds")
# cumulative_species <- readRDS("./data/cumulative_species.rds")

# plot
ggplot(cumulative_species, aes(year, cumulative_spp, fill = group)) +
  geom_stream(bw = 1) +
  theme_bw()