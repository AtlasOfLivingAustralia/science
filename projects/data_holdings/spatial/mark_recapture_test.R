## ---------------------------
## Title: Marked Unmarked Occupancy Model test
## Author: Dax Kellie
## Date Created: 2021-06-07
## ---------------------------

rm(list = ls())

# packages
# remove.packages("galah")
# remotes::install_github("AtlasOfLivingAustralia/galah") # installs latest version
library(galah)
library(tidyverse)
library(data.table)

# ala_config(email = "dax.kellie@csiro.au")




#------------------------------------------------------#
#                       Get Data
#------------------------------------------------------#

parrots <- select_taxa("psittaciformes")
act_10yrs <- select_filters(cl22 = "Australian Capital Territory", year = seq(2000, 2020))
parrot_spp <- ala_species(taxa = parrots, filters = act_10yrs)

# counts by year for all parrot species
species_names <- parrot_spp$species
result_list <- species_names %>% 
  map(~ala_counts(
    taxa = .x,
    filters = act_10yrs,
    group_by = "year"))

# add species names to list
parrot_list <- tibble(
  species = parrot_spp$species_name,
  y = result_list
)

# convert back to df
parrot_df <- parrot_list %>% unnest(y)





#-------------Convert to presence/absence--------------#


# Find which years each parrot spp was not found
species_names <- parrot_spp$species_name
year_df <- species_names %>%  # create df of all years & spp we want
  map_dfr(., ~{data.frame(year = rep(2000:2020),
                          species = .x)})

parrot_df$year <- as.integer(parrot_df$year) # make class the same
parrots_allyrs <- year_df %>% left_join(parrot_df) # merge

# Now for each spp, years not recorded are in dataframe as NA rows
# parrots_allyrs %>% filter(species == "Neophema (Neophema) pulchella") # for example

# add presence/absence column
parrots_allyrs <- parrots_allyrs %>% 
  mutate(Present = if_else(is.na(count), 0, 1))
# parrots_allyrs %>% filter(species == "Neophema (Neophema) pulchella") # for example


# pivot df wide by year
parrots_allyrs_wide <- parrots_allyrs %>%
  dplyr::select(year, species, Present) %>%
  pivot_wider(names_from = year, values_from = Present)



#------------------------------------------------------#
#                   Create AUS map
#------------------------------------------------------#

library(raster)
library(terra)
library(ozmaps)
library(sf)

# ACT Map
act_map <- abs_ste %>% dplyr::filter(grepl("Australian Capital Territory", NAME))
bb <- st_bbox(act_map)

# Plot ACT
plot(act_map, reset = FALSE, main = "ACT")
rect(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])



# TODO: Use unmarked model to plot to map
# FIXME: Grid does not work
# FIXME: Raster does not work


# create grid lookup for ala records
grid <- expand.grid(
  xmin = c(148:150),
  ymin = c(-36:-35))
grid$xmax <- grid$xmin + 1
grid$ymax <- grid$ymin + 1


r <- raster(act_map, res = 1)
r <- rast(r)



# subset to those over the australian land mass
point_values <- extract(r, grid[, c("xmin", "ymin")] + 0.5)
keep_rows <-
  !apply(point_values[, -1], 1, function(a){all(is.na(a))}) &
  !(grid$ymin >= -11 & grid$xmin != 142) & # excludes offshore stuff
  !(grid$ymin == -12 & grid$xmin > 150)
grid <- grid[keep_rows, ]
point_values <- point_values[keep_rows, ]

# ggplot(grid, aes(x = xmin, y = ymin)) + geom_point() # works




# get richness calcs from ALA
wkt_list <- lapply(
  split(grid, seq_len(nrow(grid))),
  function(a){
    wkt <- paste0("POLYGON((",
                  paste(
                    paste(
                      c(a$xmin, a$xmax, a$xmax, a$xmin, a$xmin),
                      c(a$ymin, a$ymin, a$ymax, a$ymax, a$ymin),
                      sep = " "),
                    collapse = ","),
                  "))")
  })

#------------------------------------------------------#
#         Mark-recapture model using unmarked
#------------------------------------------------------#
library(unmarked)

# convert to special unmarked data frame
y <- parrots_allyrs_wide[ ,2:22] # values
umf <- unmarkedFrameOccu(y = y)
summary(umf)

# model
fm <- occu(formula = ~ 1 
           ~ 1,
           data = umf)
fm

backTransform(fm, type = "state") # get coefs

# predict values
ggPred <- data.frame(unlist(wkt_list))
occuPred <- predict(fm,
                    type = "state",
                    newdata = ggPred,
                    na.rm = TRUE,
                    inf.rm = TRUE)


#link: https://doi90.github.io/lodestar/fitting-occupancy-models-with-unmarked.html#load-data




#------------------------------------------------------#
#         Mark-recapture model using marked
#------------------------------------------------------#

library(RMark)
library(marked)

# the model requires a column of presence/absence, 
# each number corresponds with presence/absence each year 
parrots_marked <- parrots_allyrs_wide %>%
  unite(ch, -1, sep = "") # "ch" is the new col name

  
# model
model <- crm(parrots_marked)
model

# FIXME: more complex model returns error
# more complex model
parrots_marked.proc <- process.data(parrots_marked, model = "CJS")
parrots_marked.ddl <- make.design.data(parrots_marked.proc)
Phi.time <- list(formula=~time)
Phi.dot <- list(formula=~1)
p.dot <- list(formula=~1)


model <- mark(parrots_marked.proc, parrots_marked.ddl, ## DOES NOT WORK
             model.parameters = list(Phi = Phi.dot, p = p.dot), output = FALSE, delete = TRUE)

phitable = get.real(model, "p", se = TRUE)
# names(phitable)
phitable[c("estimate","se","lcl","ucl")][1,]

# LINKS
# Code: https://cran.r-project.org/web/packages/marked/vignettes/markedVignette.html
# Paper: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12065
