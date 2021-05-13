## ANALYSIS OF DATA PROVIDER COUNT SUMMARIES FOR SELECTED TAXA ##

# get latest version of R package for ALA
# remotes::install_github("AtlasOfLivingAustralia/koala") # current as at 15-12-2020
library(koala)
source("data_provider_functions.R")

# task: look up largest data providers for the following groups:
  # plants (kingdom plantae)
  # birds (class aves)
  # non-avian vertebrates
  # invertebrates (non-chordate animals)
# note that not all of these are monophyletic groups
# hence, the best approach is to calculate:
  # counts for kingdom animalia (A)
  # counts for phylum chordata (B)
  # counts for class aves (C)
# then sum to get the groups we want, i.e.
  # birds (class aves) == C
  # non-avian vertebrates == B - C
  # invertebrates (non-chordate animals) == A - B

taxa_list <- c("plantae", "animalia", "chordata", "aves")
id_list <- lapply(taxa_list, ala_taxa)
names(id_list) <- taxa_list

# look up counts
dr_uid_list <- lapply(taxa_list, function(a){
  result <- ala_counts(id_list[[a]], breakdown = "data_resource_uid", limit = 1000)
  colnames(result) <- c("dr_uid", a)
  return(result)
})
names(dr_uid_list) <- taxa_list

# merge into a single data.frame
dr_uid <- merge(dr_uid_list$plantae, dr_uid_list$animalia, by = "dr_uid", all = TRUE)
dr_uid <- merge(dr_uid, dr_uid_list$chordata, by = "dr_uid", all = TRUE)
dr_uid <- merge(dr_uid, dr_uid_list$aves, by = "dr_uid", all = TRUE)
for(i in 2:5){dr_uid[is.na(dr_uid[, i]), i] <- 0}

# create new summed columns
dr_uid$invertebrates <- dr_uid$animalia - dr_uid$chordata
dr_uid$nonavian_vertebrates <- dr_uid$chordata - dr_uid$aves
dr_uid <- dr_uid[, !(colnames(dr_uid) %in% c("animalia", "chordata"))]
dr_uid <- dr_uid[, c(1, 2, 4, 5, 3)]

# reduce total number of entries
dr_uid <- dr_uid[apply(dr_uid[, -1], 1, sum) > 10000, ]

# get taxon info
uid_list <- lapply(dr_uid$dr_uid, lookup_resource_uid)
# if the above doesn't work, try in a loop to check for bugs
# uid_list <- as.list(rep(NA, nrow(dr_uid)))
# for(i in c(53:length(uid_list))){
#   uid_list[[i]] <- lookup_resource_uid(dr_uid$name[i])
# }

# generate the best possible guess of data provider
uid_df <- do.call(rbind, uid_list)[, c(1, 3, 4, 2)]

# CSIRO National Fish Collection not listed as OZCAM for some reaons
ozcam_text <- "OZCAM (Online Zoological Collections of Australian Museums) Provider"
uid_df$provider[which(grepl("OZCAM", uid_df$name))] <- ozcam_text
uid_df$provider[which(grepl("ANIC", uid_df$name))[2]] <- ozcam_text
uid_df$provider[which(uid_df$provider == "Australian National Insect Collection, CSIRO")] <- ozcam_text

# generate a label with the most useful information about that provider
uid_df$label <- apply(uid_df[, c(2:4)], 1, function(a){
  if(all(is.na(a))){
    NA
  }else{
    a[min(which(!is.na(a)))]
  }
})

# CSIRO is listed using many different labels - standardize
uid_df$label[which(
  (grepl("Commonwealth Scientific and Industrial Research Organisation", uid_df$label) |
  grepl("CSIRO", uid_df$label)) &
  !grepl("OZCAM", uid_df$provider)
)] <- "CSIRO"

# combine herbaria
uid_df$label[
  uid_df$label %in% c("New Zealand Virtual Herbarium", "Australia's Virtual Herbarium")
] <- "Australasian Virtual Herbarium"

# add this content back to the count dataset
dr_uid$provider <- uid_df$label

# Because some providers have multiple uids, we now need to group repeat obs
provider_list <- split(dr_uid, dr_uid$provider)
count_df <- as.data.frame(do.call(rbind, lapply(provider_list, function(a){
  c(
    provider = a$provider[1],
    apply(a[, 2:5], 2, function(b){sum(b, na.rm = TRUE)})
  )
})))
rownames(count_df) <- NULL
count_list <- lapply(
  colnames(count_df)[2:5],
  function(a){data.frame(provider = count_df$provider, count = as.numeric(count_df[, a]))})
names(count_list) <- colnames(count_df)[2:5]
count_list <- lapply(count_list, function(a){a[order(a$count, decreasing = TRUE), ]})
count_list <- lapply(count_list, function(a){a[1:10, ]})

# convert to df
count_df <- as.data.frame(do.call(rbind, count_list))
rownames(count_df) <- NULL
count_df$taxon <- rep(names(count_list), each = 10)

# manually write better labels (messy code, but works!)
count_df$label <- NA

count_df$label[grepl("Office of Environment and Heritage", count_df$provider)] <- "Office of Environment\n& Heritage (NSW)"
count_df$label[grepl("Victorian Department of Environment", count_df$provider)] <- "DELWP (VIC)"
count_df$label[grepl("South Australia, Department for Environment", count_df$provider)] <- "Dept. Env.\n& Water (SA)"
count_df$label[grepl("Northern Territory Department of Environment", count_df$provider)] <- "Dept. Env. & Natural\nRes. (NT)"
count_df$label[grepl("Tasmanian Department of Primary Industries", count_df$provider)] <- "DPIPWE (TAS)"
count_df$label[grepl("OZCAM", count_df$provider)] <- "Museums & Collections"
count_df$label[grepl("eBird", count_df$provider)] <- "eBird"
count_df$label[grepl("BirdLife", count_df$provider)] <- "BirdLife Australia"

count_df$label[count_df$provider == "Australasian Virtual Herbarium"] <- "Australasian Virtual Herbarium"
count_df$label[count_df$provider == "Ocean Biogeographic Information System"] <- "OBIS"
count_df$label[count_df$provider == "WildNet - Queensland Wildlife Data"] <- "WildNet (QLD)"
count_df$label[count_df$provider == "iNaturalist Australia"] <- "iNaturalist"
count_df$label[count_df$provider == "NatureMap"] <- "NatureMap"
count_df$label[count_df$provider == "Australian Antarctic Data Centre"] <- "AADC"
count_df$label[count_df$provider == "Murray-Darling Basin Authority"] <- "MDBC"
count_df$label[count_df$provider == "CSIRO"] <- "CSIRO"
count_df$label[count_df$provider == "Questagame"] <- "Earth Guardians"
count_df$label[count_df$provider == "BowerBird"] <- "BowerBird"
count_df$label[count_df$provider == "Reef Life Survey"] <- "Reef Life Survey"
count_df$label[count_df$provider == "NSW Bird Atlassers"] <- "NSW Bird\nAtlassers"
count_df$label[count_df$provider == "Canberra Ornithologists Group"] <- "Canberra Ornithologists Group"

saveRDS(count_df, "labelled_count_df_highTaxa_v2.rds")
# count_df <- readRDS("./data/labelled_count_list_highTaxa.rds")

# generate plot data
library(viridis)
plot_list <- lapply(split(count_df, count_df$taxon)[c(4, 2, 3, 1)], function(a){
  result <- taxon_plot_data(a$label, a$count, log_scale = FALSE)
  result$count_label <- formatC(result$count, format = "f", digits = 0, big.mark = ",")
  return(result)
})

label_list <- lapply(plot_list, function(a){
  do.call(rbind, lapply(
    split(a, a$group),
    function(b){
      data.frame(
        x = mean(b$x),
        x_max = max(b$x),
        y = mean(b$y),
        count = b$count[1], # b$count_label[1],
        label = b$label[1])
    })
  )
})

# plot this
# plantae
plot_providers(plot_list$plantae, label_list$plantae,
  label_split = 900000,
  color = ALA_colors[["pale_moss"]]) +
  # ggtitle("Providers of plant data (kingdom Plantae) to ALA") +
  lims(x = c(-3000, 5000))
ggsave("./plots/final_provider_plot_plantae.pdf")

# invertebrates
plot_providers(plot_list$invertebrates, label_list$invertebrates[1:6, ],
  label_split = 110000,
  color = ALA_colors[["seafoam"]]
) +
  geom_text(
    data = label_list$invertebrates[7:9, ],
    mapping = aes(x = x_max, y = y, label = label),
    color = "#667073",
    hjust = 0,
    nudge_x = 50,
    nudge_y = -50,
    size = 2.5
  ) +
  geom_text(
    data = label_list$invertebrates[10, ],
    mapping = aes(x = x, y = y, label = label),
    color = "#667073",
    hjust = 0.5,
    # nudge_x = 100,
    nudge_y = 220,
    size = 3
  ) +
  # ggtitle("Providers of invertebrate data to ALA") +
  lims(x = c(-2300, 2000))
ggsave("./plots/final_provider_plot_invertebrates.pdf")

# non-avian vertebrates
plot_providers(plot_list$nonavian_vertebrates, label_list$nonavian_vertebrates[1:8, ],
  label_split = 500000,
  color = ALA_colors[["flamingo"]]
) +
  geom_text(
    data = label_list$nonavian_vertebrates[9:10, ],
    mapping = aes(x = x_max, y = y, label = label),
    color = "#667073",
    hjust = 0,
    nudge_x = 20,
    nudge_y = -150,
    size = 3
  ) +
  # ggtitle("Providers of non-avian vertebrate data to ALA") +
  lims(x = c(-2000, 3000))
ggsave("./plots/final_provider_plot_nonavian_vertebrates.pdf")

# aves
plot_providers(plot_list$aves, label_list$aves[-3, ],
  label_split = 3000000,
  color = ALA_colors[["honey"]]
) +
  geom_text(
    data = label_list$aves[3, ],
    mapping = aes(x = x_max, y = y, label = label),
    color = "#667073",
    hjust = 0,
    nudge_x = 50,
    nudge_y = -500,
    size = 3
  ) +
  # ggtitle("Providers of bird data to ALA") +
  lims(x = c(-5000, 8000))
ggsave("./plots/final_provider_plot_aves.pdf")
