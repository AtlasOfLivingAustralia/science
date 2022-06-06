# script for taxonomic name extraction from pdf

# 1. install underlying software requirements
## 1a. For Mac, install `xquartz` (https://www.xquartz.org) v 2.8.1

# 1b. get latest version of `gnfinder` (taxonomic name finder library)
## https://github.com/gnames/gnfinder/releases
## for advice on how to do this, see:
## https://github.com/gnames/gnfinder#install-with-homebrew-on-mac-os-x-linux-and-linux-on-windows-wsl2

## 2. load R and install packages
# install.packages("pdftools")
library(pdftools) # should load without errors if `xquartz` is installed

# remotes::install_github("ropenscilabs/namext")
library(namext) # should load without errors if `namext` is installed

# other libraries
library(janitor)
library(dplyr)
library(galah)
source("taxonomy_functions.R")

# 3. find zotero info
## export from zotero as csv then import to R
df <- read.csv("ala_cited.csv") |> 
  as_tibble() |> 
  clean_names()
# note: weird errors with read_csv, namely filter by year fails
  
# make a subset for analysis 
df_small <- df |> filter(
  publication_year == 2022 &  # restrict by year
  !is.na(file_attachments) &  # PDFs must be present to be included
  grepl(".pdf$", file_attachments) # attachments must be PDFs
)

# make sure files exist
df_small <- df_small |>
  filter(unlist(lapply(df_small$file_attachments, file.exists)))
  
# 4. get taxonomic info from these PDFs via new function `taxon_frequencies()`
  # look for taxonomic-like names
  # look for contractions and matches them to full names
  # calculate counts
  # order counts in decreasing order, removing anything with count < 3
  # search against atlas taxonomy
  # return only those names/counts that are matched in ALA
taxonomy_df <- get_taxonomy_info(df_small$file_attachments[15:30])

## optional code to run the above in a loop for debugging reasons
# extract_list <- vector(mode = "list", length = nrow(df_small))
# for(i in seq_along(extract_list)){
#   extract_list[[i]] <- taxon_frequencies(df_small$file_attachments[i])
# }
# taxonomy_df <- do.call(rbind, extract_list) |> as_tibble()

# export
write.csv(taxonomy_df, "taxonomy_extraction_2022.csv", row.names = FALSE)

## TODO:
  # match to article IDs for to link titles, authors, DOIs to taxa
  # visualise key taxa per paper?