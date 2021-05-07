## ------------------------------------------------------#
## Title: Temporal ALA Investigation
## Author: Dax Kellie
## Date Created: 2021-05-7
## ------------------------------------------------------#

# packages
rm(list = ls())

# packages

# remotes::install_github("AtlasOfLivingAustralia/galah") # installs latest version
library(galah)
library(data.table)
library(ggplot2)


#------------------------------------------------------#
#   Trends in time of year of recording data in ALA
#------------------------------------------------------#


find_field_values("basisOfRecord")


# Get data

# Might need to config email to get ALA data
ala_config(email = "dax.kellie@csiro.au")

# How many occurrence records in Tasmania over last 10 years?
ala_counts(filters = select_filters(stateProvince = "Tasmania",
                                    year = seq(2010, 2020),
                                    basisOfRecord = "HumanObservation"))

# Get all records
occ <- ala_occurrences(filters = select_filters(stateProvince = "Tasmania",
                                               year = seq(2010, 2020),
                                               basisOfRecord = "HumanObservation"))



