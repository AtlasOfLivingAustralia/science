# Extracting scientific publications from GBIF that use ALA data

##### Libraries #####
library(tidyverse)
library(rgbif)
library(httr2)
library(readr)
library(purrr)
library(tibble)
library(snakecase)
library(stringr)
library(janitor)
library(treemapify)
library(pdftools)

setwd("C:/Users/WAI045/OneDrive - CSIRO/ALA/Citations Tracker")

##### Get Datasets #####
dataset_df <- request("https://api.gbif.org/v1/dataset/search/export?publishingCountry=AU&format=CSV") |>
  req_perform() |>
  resp_body_string() |>
  read_csv()

##### Takes ~2hrs to run #####
# all_papers <- map(
#   .x = 1:nrow(dataset_df),
#   .f = function(x) {
#     lit_counts <- lit_count(datasetKey = dataset_df$dataset_key[x])
#     offset_seq <- seq(0, lit_counts, by = 500)
#     
#     this_dataset <- map(
#       .x = 1:length(offset_seq),
#       .f = function(y) {
#         lit_api <- paste0("https://api.gbif.org/v1/literature/search?gbifDatasetKey=", 
#                           dataset_df$dataset_key[x], 
#                           "&limit=500&offset=", 
#                           offset_seq[y])
#         lit_df <- (request(lit_api) |>
#               req_perform() |>
#               resp_body_json())$results
#         Sys.sleep(2)
#         return(lit_df)
#       }) |>
#       list_flatten() |>
#       map(.f = function(y) {
#             paper_df <- y |> tibble() |> t() |> data.frame() |> tibble()
#             colnames(paper_df) <- names(y)
#             return(paper_df)}) |>
#       list_rbind() |>
#       mutate(dataset = dataset_df$title[x])
#   },
#   .progress = TRUE
# ) |>
#   list_rbind()
# 
# save(all_papers, file = "data/all_papers.rds")
load("data/all_papers.rds")

##### Clean Dataframe #####
all_papers_clean <- all_papers |>
  select(-gbifDownloadKey, -gbifOccurrenceKey, -gbifNetworkKey, -gbifProjectIdentifier, -gbifProgramme, -tags) |>
  rowwise() |>
  mutate(
    across(-c(authors, dataset),
           function(x) unlist(x) |> paste0(collapse = " | "))) |>
  mutate(authors = map(.x = authors,
                       .f = function(x) paste0(x$lastName, ", ", x$firstName)) |>
           unlist() |> paste0(collapse = "; ")) |>
  distinct() |>
  mutate(true_holder = TRUE) |>
  pivot_wider(names_from = dataset, values_from = true_holder, values_fill = FALSE) |>
  mutate(across(everything(), 
                function(x) {ifelse(x %in% c("-", ""), NA, x)})) |>
  filter(publisher != "IUCN" | literatureType != "REPORT")

write_csv(all_papers_clean, file = "data/all_papers_clean.csv")

##### Compare to ALA citations #####
ALA_zotero <- read_csv("data/ala_zotero_data.csv") |>
  mutate(across(everything(), 
                function(x) {ifelse(x %in% c("-", ""), NA, x)}))

joined_papers <- all_papers_clean |>
  select(year, identifiers, literatureType) |>
  rename(`Publication Year` = year,
         DOI = identifiers,
         `Item Type` = literatureType) |> 
  mutate(`Citation Source` = "GBIF") |>
  rbind(
    ALA_zotero |>
      select(`Publication Year`, `DOI`, `Item Type`) |>
      mutate(`Citation Source` = "ALA")
  ) |>
  mutate(`Item Type` = to_title_case(`Item Type`),
         `Item Type`  = case_when(
           `Item Type` == "Journal" ~ "Journal Article",
           `Item Type` == "Webpage" ~ "Web Page",
           .default = `Item Type`
         )) |>
  distinct()

joined_papers_clean <- joined_papers |>
  filter(!is.na(DOI)) |>
  mutate(true_holder = TRUE) |>
  pivot_wider(names_from = `Citation Source`, values_from = true_holder, values_fill = FALSE) |>
  mutate(`Citation Source` = case_when(
    GBIF & !ALA ~ "GBIF",
    !GBIF & ALA ~ "ALA",
    GBIF & ALA ~ "Both",
    .default = NA)) |>
  mutate(`Publication Year` = as.numeric(`Publication Year`),
         `Citation Source` = factor(`Citation Source`, levels = c("GBIF", "Both", "ALA")))

citation_plot <- ggplot(joined_papers_clean) +
  geom_bar(aes(x = `Publication Year`, fill = `Citation Source`)) +
  scale_fill_manual(values = c("ALA" = "#FFA300", "Both" = "#843CC8", "GBIF" = "#509E2F")) +
  theme_classic() +
  ylab("Number of Citations")

ggsave(filename = "figures/citation_plot.pdf")

#### DOIs with different years
# 10.1016/j.biocon.2021.109425
# 10.1016/j.cub.2020.10.053
# 10.1016/j.ecochg.2022.100062
# 10.1088/1748-9326/aaf5db

##### Total GBIF Papers (- ALA ones) #####
GBIF_AU_totals <- all_papers_clean |>
  filter(!is.na(identifiers)) |>
  select(year, identifiers, literatureType) |>
  rename(`Publication Year` = year,
         DOI = identifiers,
         `Item Type` = literatureType) |>
  mutate(`Publication Year` = as.numeric(`Publication Year`)) |>
  count(`Publication Year`)

##### Takes ~40min to run
# GBIF_all_papers <- map(
#   .x = 2007:2024,
#   .f = function(x) {
#     lit_counts <- lit_count(datasetKey = dataset_df$dataset_key[x])
#     offset_seq <- seq(0, lit_counts, by = 800)
#     
#     this_dataset <- map(
#       .x = 1:length(offset_seq),
#       .f = function(y) {
#         lit_api <- paste0("https://api.gbif.org/v1/literature/search?year=", 
#                           x, 
#                           "&limit=800&offset=", 
#                           offset_seq[y])
#         lit_df <- (request(lit_api) |>
#                      req_perform() |>
#                      resp_body_json())$results
#         Sys.sleep(2)
#         return(lit_df)
#       },
#       .progress = TRUE
#     ) |>
#       list_flatten() |>
#       map(.f = function(y) {
#         paper_df <- y |> tibble() |> t() |> data.frame() |> tibble()
#         colnames(paper_df) <- names(y)
#         return(paper_df)}) |>
#       list_rbind()
#   }
# ) |>
#   list_rbind()
#   
# save(GBIF_all_papers, file = "data/GBIF_all_papers.rds")
load("data/GBIF_all_papers.rds")  

GBIF_all_papers_clean <- GBIF_all_papers |>
  select(-gbifDownloadKey, -gbifOccurrenceKey, -gbifNetworkKey, -gbifProjectIdentifier, -gbifProgramme, -tags) |>
  rowwise() |>
  mutate(
    across(-c(authors),
           function(x) unlist(x) |> paste0(collapse = " | "))) |>
  mutate(authors = map(.x = authors,
                       .f = function(x) paste0(x$lastName, ", ", x$firstName)) |>
           unlist() |> paste0(collapse = "; ")) |>
  distinct() |>
  mutate(across(everything(), 
                function(x) {ifelse(x %in% c("-", ""), NA, x)})) |>
  filter(publisher != "IUCN" | literatureType != "REPORT") |>
  filter(!(id %in% all_papers_clean$id))

write_csv(all_papers_clean, file = "data/GBIF_all_papers_clean.csv")

GBIF_totals <- GBIF_all_papers_clean |>
  filter(!is.na(identifiers)) |>
  select(year, identifiers, literatureType) |>
  rename(`Publication Year` = year,
         DOI = identifiers,
         `Item Type` = literatureType) |>
  count(`Publication Year`)

GBIF_citation_plot <- rbind(
  GBIF_totals |> mutate(ALA_data = "don't use ALA data"),
  GBIF_AU_totals |> mutate(ALA_data = "use ALA data")) |>
  ggplot() +
  geom_bar(aes(x = `Publication Year`, y = n, fill = ALA_data), stat = "identity") +
  scale_fill_manual(name = "GBIF citations that...",
                    values = c("use ALA data" = "#FFA300", "don't use ALA data" = "#509E2F")) +
  theme_classic() +
  ylab("Number of GBIF Citations")

ggsave(filename = "figures/GBIF_citation_plot.pdf")


##### Stage 2 #####

rbind(
  all_papers_clean |>
    select(authors, added, published, identifiers, keywords, literatureType, openAccess, peerReview, source, title, topics, year, abstract, citationType) |>
    mutate(category = "GBIF: cites ALA"),
  GBIF_all_papers_clean |>
    select(authors, added, published, identifiers, keywords, literatureType, openAccess, peerReview, source, title, topics, year, abstract, citationType) |>
    mutate(category = "GBIF: doesn't cite ALA")) |>
  # mutate(topics = str_split(topics, " \\| "),
  #        keywords = str_split(keywords, " \\| ")) |>
  write_csv("data/GBIF_citations_all.csv")


##### Plotting New Figures #####
all_citations_totals <- joined_papers_clean |>
  mutate(
    `Citation Source` = as.character(`Citation Source`),
    `Citation Source` = ifelse(`Citation Source` == "Both", "ALA", `Citation Source`),
    `Citation Source` = ifelse(`Citation Source` == "ALA", "AU data via ALA", "AU data via GBIF")) |>
  count(`Publication Year`, `Citation Source`) |>
  rbind(
    GBIF_totals |>
      mutate(`Citation Source` = "GBIF without AU data") |>
      select(`Publication Year`, `Citation Source`, n)
  ) |>
  mutate(
    `Publication Year` = as.numeric(`Publication Year`),
    `Citation Source` = factor(`Citation Source`, levels = c("GBIF without AU data", "AU data via GBIF", "AU data via ALA"))) |>
  filter(`Publication Year` <= 2022)

all_citations_plot <- ggplot(all_citations_totals) +
  geom_bar(aes(x = `Publication Year`, y = n, fill = `Citation Source`),
           position = "stack", stat = "identity") +
  scale_fill_manual(values = c("GBIF without AU data" = "#8fcceb", 
                               "AU data via GBIF" = "#239e60", 
                               "AU data via ALA" = "#83d166")) +
  theme_bw() +
  ylab("Number of Citations")
ggsave("figures/citation_plot_cw.pdf", all_citations_plot, width = 8, height = 6, units = "in")
pdf_convert("figures/citation_plot_cw.pdf",
            format = "png", dpi = 500,
            filenames = "figures/citation_plot_cw.png")

###### Tree Map #####
treemap_data <- ALA_zotero |>
  rename(item_type = `Item Type`) |>
  mutate(
    item_type = case_when(
      item_type == "manuscript" ~ "journalArticle",
      item_type %in% c("book", "bookSection") ~ "bookOrBookSection",
      item_type %in% c("magazineArticle", "encyclopediaArticle") ~ "magazineOrEncyclopedia",
      item_type %in% c("artwork", "videoRecording") ~ "mixedMedia",
      .default = item_type
    )
  ) |>
  count(item_type) |>
  mutate(
    group_type = case_when(
      item_type %in% c("blogPost", "document", "magazineOrEncyclopedia", 
                       "mixedMedia", "newspaperArticle", "report", "webpage") ~ 2,
      .default = 1
    ) |>
      factor(levels = c(1, 2),
             labels = c("Scientific content",
                        "Grey lit & cultural content")),
    item_type = to_sentence_case(item_type)) |>
  mutate(item_type = gsub("\\s", "\n", item_type)) |>
  mutate(
    item_type = case_when(
      item_type == "Book\nor\nbook\nsection" ~ "Book\nor\nBook section",
      item_type == "Magazine\nor\nencyclopedia" ~ "Magazine\nor\nEncylopedia",
      .default = item_type
    )) |>
  arrange(group_type, desc(n)) |>
  mutate(fill_col = c("#d8f5ce", 
                      "#b0e69c", 
                      "#98e37d",
                      "#83d166", 
                      "#6bba4e",
                      "#5ba63f",
                      "#f5e9ce",
                      "#e7d09d",
                      "#e3c57d",
                      "#d1b166",
                      "#ba9a4f",
                      "#a6873f",
                      "#8a6c28"))

types_treemap <- ggplot(treemap_data,
                        aes(area = n, 
                            fill = fill_col,
                            subgroup = group_type, 
                            label = item_type)) +
  geom_treemap() +
  scale_fill_identity() +
  geom_treemap_subgroup_border() +
  geom_treemap_text(
    padding.x = grid::unit(2, "mm"),
    padding.y = grid::unit(3, "mm")) +
  theme(legend.position = NULL)

ggsave("figures/paper_type_treemap.pdf", types_treemap, width = 5, height = 5, units = "in")
pdf_convert("figures/paper_type_treemap.pdf",
            format = "png", dpi = 500,
            filenames = "figures/paper_type_treemap.png")
