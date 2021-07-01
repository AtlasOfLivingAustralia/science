
# more occupancy models for species richness estimation

# goal here is to estimate total richness given a set of covariates,
# thereby creating a single model for 'completeness' of ALA data,
# but based on all possible data cuts, i.e.
  # 
  
# data structure needed:
  # each row is a species conditional on covariates
  # columns are time units
  # option to fit an abundance-based model using occuRN
  # total number of observations is an observation-level covariate

library(galah)
library(pbapply)
library(lme4)
source("./functions/glmer_functions.R")


# ala_counts(
#   taxa = select_taxa("amphibia"),
#   filters = select_filters(year = c("[2010 TO 2020]","1965")))
  
# where are IBRA regions?
# search_fields("IBRA") # cl1048
IBRA_regions <- find_field_values("cl1048", limit = 100)$category
# search_fields("decade")
# decades <- find_field_values("decade", limit = 200)
decades <- seq(1900, 2020, 10)

lookup_table <- expand.grid(
  ibra = IBRA_regions,
  decade = decades)


# get counts of number of records
lookup_split <- split(lookup_table, seq_len(nrow(lookup_table)))
lookup_table$n_records <- unlist(pblapply(lookup_split,
  function(a){count_function(a, taxon = select_taxa("amphibia"))})
)

lookup_split <- split(lookup_table, seq_len(nrow(lookup_table)))
species_lists <- pblapply(lookup_split,
  function(a){data_function(a, taxon = select_taxa("amphibia"))})
species_df <- do.call(rbind, species_lists)
# save out:
# saveRDS(species_df, "./cache/species_df.rds")
# species_df <- readRDS("./cache/species_df.rds")


# expand this to presence/absence
ibra_list <- split(species_df, species_df$ibra)
all_decades <- sort(unique(species_df$decade))

ibra_pa <- lapply(ibra_list, function(a){
  all_species <- sort(unique(a$species))
  occ_list <- lapply(all_decades, function(b){
    if(any(a$decade == b)){
      species_tr <- a$species[a$decade == b]
      data.frame(
        ibra = a$ibra[1],
        decade = b,
        n_records = a$n_records[a$decade == b][1],
        species = all_species,
        present = as.numeric(all_species %in% species_tr))  
    }else{
      NULL
    }
  })
  occ_df <- do.call(rbind, occ_list)
  return(occ_df)
})
data_final <- do.call(rbind, ibra_pa)

# save
# saveRDS(data_final, "./cache/data_final.rds")
# data_final <- readRDS("./cache/data_final.rds")

# model
data_final$decade_scaled <- scale(data_final$decade)
data_final$n_records_scaled <- scale(log(data_final$n_records))

model <- glmer(present ~ decade_scaled + n_records_scaled + (1|ibra) + (1|species),
  family = binomial(link = "logit"),
  data = data_final)

# summary(model)
prob_df <- data.frame(
  ibra = rownames(ranef(model)$ibra),
  prob_intercept = plogis(ranef(model)$ibra[[1]] + fixef(model)[1]))
  
  
newdata_list <- lapply(
  split(data_final, data_final$ibra),
  function(a){
    data.frame(
      ibra = a$ibra[1],
      decade_scaled = max(a$decade_scaled),
      n_records_scaled = max(a$n_records_scaled)
    )
  }
)
newdata_df <- do.call(rbind, newdata_list)
newdata_df <- newdata_df[!is.na(newdata_df$ibra), ]
rownames(newdata_df) <- NULL

# all(prob_df$ibra == newdata_df$ibra) # TRUE
prob_df$prob_max <- plogis(
  predict(model, newdata_df, re.form = ~ (1|ibra), type = "link"))
  
observed_richness <- unlist(lapply(
  split(data_final, data_final$ibra),
  function(a){
    length(unique(a$species))
  }
))
or_df <- data.frame(
  ibra = names(observed_richness),
  observed_richness = observed_richness)

prob_df <- merge(prob_df, or_df, by = "ibra", all = FALSE)

prob_df$expected_richness <- prob_df$observed_richness / prob_df$prob_max
prob_df$richness_deficit <- prob_df$observed_richness - prob_df$expected_richness
prob_df$completeness <- prob_df$observed_richness / prob_df$expected_richness

# save
# saveRDS(prob_df, "./cache/completeness_probabilities_glmer.rds")
# prob_df <- readRDS("./cache/completeness_probabilities_glmer.rds")

ibra_map <- readRDS("./cache/ibra_map.rds")
ibra_map2 <- merge(ibra_map, prob_df, by.x = "REG_NAME_7", by.y = "ibra", all = TRUE)

library(sf)
library(ggplot2)
library(viridis)
library(patchwork)


a <- ggplot(ibra_map2) + 
  geom_sf(aes(fill = observed_richness), color = NA) +
  lims(x = c(110, 155), y = c(-45, -10)) +
  scale_fill_viridis(limits = c(0, 90)) +
  theme_void() +
  labs(fill = "") +
  ggtitle("a. Observed richness")

b <- ggplot(ibra_map2) + 
  geom_sf(aes(fill = expected_richness), color = NA) +
  lims(x = c(110, 155), y = c(-45, -10)) +
  scale_fill_viridis(limits = c(0, 90)) +
  theme_void() +
  labs(fill = "") +
  ggtitle("b. Expected richness")
    
c <- ggplot(ibra_map2) + 
  geom_sf(aes(fill = richness_deficit), color = NA) +
  lims(x = c(110, 155), y = c(-45, -10)) +
  scale_fill_viridis(limits = c(-40, 0), option = "magma", direction = -1) +
  theme_void() +
  labs(fill = "") +
  ggtitle("c. Richness deficit")
      
d <- ggplot(ibra_map2) + 
  geom_sf(aes(fill = completeness), color = NA) +
  lims(x = c(110, 155), y = c(-45, -10)) +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_void() +
  labs(fill = "") +
  ggtitle("d. Completeness")

(a + b)/(c + d)
ggsave("./plots/richness_calcs_glmer.pdf")
