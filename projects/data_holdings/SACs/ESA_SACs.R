# simplified approach to SACs for ESA presentation

# goal is to create SACs for selected taxa and IBRA regions
# ideally without downloading raw occurrences

library(galah)

# test for Gibson Desert


taxon_list <- list(
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

# which IBRA regions?
# search_fields("ibra 7") # cl1048
# find_field_values("cl1048", limit = 100)
  # "Sydney Basin"
    # ala_counts(filters = select_filters(cl1048 = "Sydney Basin")) # 8,821,337
  # Cape York Peninsula - remote, but high richness
     # ala_counts(filters = select_filters(cl1048 = "Cape York Peninsula")) # 844,701
  # "Gibson Desert"  - lowest count
     # ala_counts(filters = select_filters(cl1048 = "Gibson Desert")) # 19,721

regions <- c("Sydney Basin", "Cape York Peninsula", "Gibson Desert")
regions_df <- select_filters(paste("cl1048", regions, sep = " = "))
regions_list <- split(regions_df, seq_len(nrow(regions_df)))

# combine regions and filters into a single list
index_list <- expand.grid(seq_along(regions_list), seq_along(taxon_list))
year_filter <- select_filters(year >= 1990, year <= 2020)
filter_list <- lapply(
  split(index_list, seq_len(nrow(index_list))),
  function(a){
    rbind(regions_list[[a[[1]]]], taxon_list[[a[[2]]]], year_filter)
  })

# look up required data from ALA
# Note: we need counts for all species per region/taxon
# then expand this to an obs by spp matrix

data_list <- lapply(filter_list, function(a){
# for(a in seq_along(filter_list)){
  # get counts by species
  ala_counts(filters = a, 
     group_by = select_fields("taxonConceptID", "year", expand = TRUE),
     limit = NULL)
})


sac_list <- lapply(data_list, function(a){
  # make wide format
  comm <- as.matrix(tidyr::pivot_wider(a, id_cols = year, 
     names_from = taxonConceptID,
     values_from = count,
     values_fill = 0)[, -1]  
  )  
  # # remove singletons and doubletons
  # spp_counts[spp_counts$count > 2, ]
  
  # return specaccum data.frame
  as.data.frame(vegan::specaccum(comm)[3:5])
})

# convert to single data.frame
names_df <- expand.grid(regions, names(taxon_list))
colnames(names_df) <- c("region", "taxon")
names_df <- as.data.frame(lapply(names_df, as.character))

result_list <- lapply(seq_len(nrow(names_df)), function(a){
  data.frame(
    region = names_df$region[a],
    taxon = names_df$taxon[a],
    sac_list[[a]]
  )
})
result_df <- do.call(rbind, result_list)
# str(result_df)

# plot
ggplot(result_df, aes(x = sites, y = richness, color = taxon)) + 
  geom_path() +
  facet_wrap(facets = vars(region), scales = "fixed") +
  theme_bw() +
  labs(x = "Survey Years", y = "Species Richness", color = "Taxon")
ggsave("SACs_over_time.png")

# This isn't very useful, because survey years != effort
# Ergo each plot simply reflects effort, rather than controlling for it
# as a SAC is supposed to. The best solution is to create even-sized
# sequential groups (i.e. order by date then same in ~10k blocks);
# but this requires downloading raw data.

# NOTE: The current approach could work, however, if there were event IDs in the atlas,
# and those IDs corresponded to surveys of fixed survey effort.


# code to run a SAC for very large datasets
# idea is to use summaries returned from ala_counts directly
# and also to subsample to reduce size
# However, this is based on a model that has no co-occurrence
# While this is used for atlas applications, all the curves look
# a bit the same to me, which is suspicious 
sac_small <- function(
  a,  # returned object from ala_counts(group_by = "taxonConceptID")
  precision = 100
){
  # calculate start/end of required values
  spp_cumsum <- cumsum(a$count)
  
  # attempt to run vegan::specaccum code without building a matrix
  # x is a matrix of species co-occurrences
  n_max <- sum(a$count) # number of sites; formerly n
  # n_len <- 100 # precision
  n_seq <- round(seq(1, n_max, length.out = precision))
  p <- nrow(a) # number of species
  # for method = "exact", condition = TRUE
  # freq <- colSums(x > 0) # vector of abundances
  # freq <- freq[freq > 0]
  freq <- a$count
  f <- length(freq)
  ldiv <- lchoose(n_max, n_seq)
  result <- array(dim = c(precision, f))
  for (i in seq_len(precision)) {
     result[i, ] <- ifelse(
       n_max - freq < n_seq[i], 
       0, 
       exp(lchoose(n_max - freq, n_seq[i]) - ldiv[i]))
  }
  sac <- data.frame(
    sites = n_seq,
    specaccum = rowSums(1 - result))
    
}

sac_list <- lapply(data_list, sac_small) 

# convert to single data.frame
names_df <- expand.grid(regions, names(taxon_list))
colnames(names_df) <- c("region", "taxon")
names_df <- as.data.frame(lapply(names_df, as.character))

result_list <- lapply(seq_len(nrow(names_df)), function(a){
  data.frame(
    region = names_df$region[a],
    taxon = names_df$taxon[a],
    sac_list[[a]]
  )
})
result_df <- do.call(rbind, result_list)
# str(result_df)

       
library(ggplot2)


ggplot(result_df, aes(x = sites, y = richness, color = taxon)) + 
  geom_path() +
  facet_wrap(facets = vars(region), scales = "free") +
  theme_bw()
  
ggplot(result_df, aes(x = sites, y = richness, color = taxon)) + 
  geom_path() +
  facet_wrap(facets = vars(region), scales = "fixed") +
  theme_bw() +
  labs(x = "Survey Years", y = "Species Richness", color = "Taxon")
ggsave("SACs_over_time.png")
  
# I'm suspicious of this

# log versions
ggplot(result_df, aes(x = log10(sites), y = log10(specaccum), color = taxon)) + 
  geom_path() +
  facet_wrap(facets = vars(region), scales = "fixed") +
  # facet_grid(cols = vars(region), scales = "free") +
  theme_bw()
ggsave("test_SAC_plot.png")
  
ggplot(result_df, aes(x = log10(sites), y = log10(specaccum), color = region)) + 
  geom_path() +
  facet_wrap(facets = vars(taxon), scales = "fixed") +
  # facet_grid(cols = vars(region), scales = "free") +
  theme_bw()
    
    
    
ggplot(result_df, aes(x = sites, y = specaccum, color = region)) + 
  geom_path() +
  facet_grid(cols = vars(taxon), scales = "free") +
  theme_bw()

  
# NOTES on Dax talk:
  # should GBIF logo be on the title slide? Also we're not _just_ GBIF node
  # we have data prior to 1950
  # bias: Australia one of the most urbanised nations in the world
  # text too small - make big enough to make you uncomfortable, then bigger
  # streamplot re: species numbers - previous code was cumulative
  
  # x = time, y = n_species, lines = basisOfRecord

  
  # below here attempts to build a single matrix,
  # but fails for large datasets
  index_df <- data.frame(
    start = c(1, spp_cumsum[-length(spp_cumsum)] + 1),
    end = spp_cumsum)
  n_rows <- max(spp_cumsum)
  result <- lapply(
    split(index_df, seq_len(nrow(index_df))),
    function(b){
      c(
        rep(0L, b$start - 1),
        rep(1L, b$end - b$start + 1),
        integer(n_rows - (b$end + 1)) # memory limit reached
      )
      # vec <- rep(0, n_rows)
      # vec[c(b$start:b$end)] <- 1
      # vec
    }
    # mc.cores = 7
  )
  # fails - memory exhausted
  
  matrix_list <- as.list(rep(NA, nrow(index_df)))
  for(b in seq_len(nrow(index_df))){
    matrix_list[[b]] <- rep(0, max(spp_cumsum))
    matrix_list[[b]][c(index_df$start[b] : index_df$end[b])] <- 1
  }
  
  # convert to matrix
  spp_matrix <- matrix(0, nrow = sum(spp_counts$count), ncol = nrow(spp_counts))
  return(spp_matrix)
})
