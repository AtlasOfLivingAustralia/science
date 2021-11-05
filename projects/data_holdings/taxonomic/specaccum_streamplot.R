# cumulative species counts over time as a streamplot

# remotes::install_github("AtlasOfLivingAustralia/galah@ala_counts")
library(galah)
library(ggplot2)
library(ggstream)
library(viridis)

# galah_config(email = "martinjwestgate@gmail.com")

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
  tibble(
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

cumulative_species <- cumulative_species %>%
  group_by(year) %>%
  group_by(group) %>%
  mutate(diff_species = lead(cumulative_spp) - cumulative_spp) # add count of species each year

# plot
ggplot(cumulative_species, aes(year, diff_species, fill = group)) +
  geom_stream(bw = 1) +
  pilot::scale_fill_pilot() +
  theme_bw()




# ------ Other stuff Dax tried --------#

# Just a reminder of how a proper species accumulation would need to work with ALA data
# Far more complicated, but possible

x <- ala_species(filters = select_filters(taxonConceptID = select_taxa("Chordata")$taxon_concept_id,
                                     year = 2000))
y <- ala_species(filters = select_filters(taxonConceptID = select_taxa("Chordata")$taxon_concept_id,
                                     year = 2001))
z <- ala_species(filters = select_filters(taxonConceptID = select_taxa("Chordata")$taxon_concept_id,
                                     year = 2002))
# species found in 2001 not found in 2000
y %>% filter(!species %in% x$species) %>% count()

xy <- full_join(x, y)

x %>% glimpse()
y %>% glimpse()
xy %>% glimpse()

# species found in 2002 not found in 2001
z %>% filter(!species %in% xy$species) %>% count()

# ...and so on...
