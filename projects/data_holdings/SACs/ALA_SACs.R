# Run Species Accumulation Curve calculations for IBRA regions, based on ALA data

# load relevant packages & functions
library(galah)
library(vegan)
library(ggplot2)
source("./functions/SAC_functions.R")

# set config
ala_config(caching = FALSE, email = "martinjwestgate@gmail.com", verbose = TRUE)
taxon <- "amphibia"
# search_fields("ibra") # regions = cl1048; subregions = cl1049
ibra_regions <- find_field_values("cl1048", limit = 100)$category

# run downloads
lapply(ibra_regions, function(a){
  occurrence_download(ibra = a, taxon = taxon)
})

# process into list of vegan specaccum models
file_list <- paste0("./cache/", list.files("./cache"))
regions_with_data <- gsub("-", " ",
  substr(file_list, 29, nchar(file_list)-4))
model_list <- lapply(file_list, calculate_specaccum)

## optionally save/reload
# saveRDS(model_list, "./data/model_list.rds")
# model_list <- readRDS("./data/model_list.rds")

# calculate expected richness
extrapolation_list <- lapply(model_list, extrapolate_richness)
extrapolation_df <- data.frame(do.call(rbind, extrapolation_list))
extrapolation_df$region <- regions_with_data

extrapolation_df[order(extrapolation_df$proportion_complete), ]
extrapolation_df[order(extrapolation_df$extrapolated_richness), ]

# this looks ok;
# but asymptote is lower than the observed richness in many cases.
# clearly there is a need to test model fit and assess the asymptote more carefully

# to evaluate properly, get a full SAC plot for a single region
predictions <- extrapolate_SAC(model_list[[69]])

ggplot(
  predictions[predictions$model != "SAC", ],
  aes(x = sites, y = fit)) +
  geom_path(aes(color = model)) +
  geom_path(
    data = predictions[predictions$model == "SAC", ],
    color = "black",
    size = 2,
    inherit.aes = TRUE) +
  labs(x = "Number of Observations", y = "Number of Species") +
  ggtitle("Species Accumulation Curve for frogs in the Sydney Basin IBRA region") +
  theme_bw()
ggsave("./plots/Sydney_frogs.pdf")

# now plot IBRA regions

#q: can we fit e.g. a spatial effects to the slope etc of the non-linear model?

# q: can we fit other coefficients? E.g. can we fit curves and test how they change
# over time or spaace or in cit sci, all at once?

# The problem here is working out how the original SAC model is fit -
  # the implementation by vegan is doesn't distinguish between
  # data creation and modelling.
