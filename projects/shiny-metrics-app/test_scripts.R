# metrics for Ely
install_github("atlasOfLivingAustralia/galah@development") # required for dataset counts
library(galah)

# number of occurrences; Ely gets 112.09M
counts_total <- atlas_counts()$count[1]
formatC(counts_total, big.mark = ",")
# matches

# number of species; Ely gets 111,566
counts_species <- atlas_counts(type = "species")$count[1]
formatC(counts_species, big.mark = ",") # 153,432

# number of datasets; Ely gets 846
nrow(show_all_datasets()) # 11,037
nrow(show_all_collections()) # 208
nrow(show_all_providers()) # 1,689

# number of species with images; Ely gets 61,281
galah_call() |>
  galah_filter(imageIDs != "") |>
  atlas_counts(type = "species") # 59,081