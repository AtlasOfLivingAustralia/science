
library(tidyverse)
library(here)
library(readxl)

groups <- read_xlsx(here("comms", "2024-09-25_plantae", "SpeciesGroups_ALA.xlsx"),
                    sheet = 1)


# find where all the plantae names are in the spreadsheet
which(groups$topTaxon == "Plantae")
which(groups$topTaxon == "Fungi")

groups_clean <- groups |>
  slice(74:84) |>
  filter(!str_detect(Inclusions, "Equisetopsida")) |> # Vascular plants groups override smaller groups
  mutate(
    common_name = case_when(
      !is.na(level2Name) ~ level2Name,
      is.na(level2Name)  ~ level3Name,
      .default = NA
      ),
    includes = strsplit(as.character(Inclusions), ",")
  ) |>
  unnest(includes) |>
  select(common_name, includes) |>
  mutate(
    includes = str_trim(includes)
  ) |>
  filter(common_name != "Horsetails") |> # includes is NA, which affects the next step
  tibble::add_row(includes = NA, common_name = "Other")


