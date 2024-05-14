# calculate bar plots

library(readr)
library(dplyr)
library(purrr)
library(httr2)
library(ggplot2)
library(janitor)
library(snakecase)
library(treemapify)

# import gbif data
gbif <- read_csv("papers_citing_ala_via_gbif.csv")
gbif <- gbif[, 1:29] # note: identifiers column contains dois
gbif_cited <- gbif |>
  filter(!is.na(identifiers)) |>
  group_by(year) |>
  summarize(count = n()) |>
  rename("n_ala_via_gbif" = "count")

# import ALA data
ala <- read_csv("papers_citing_ala.csv") |>
  clean_names()

ala_cited <- ala |>
  filter(!is.na(doi)) |>
  group_by(publication_year) |>
  summarize(count = n())|>
  rename("year" = "publication_year",
         "n_ala" = "count")

# get total counts per year from gbif
# noting that Callum has done a more careful search based on presence of DOI
gbif_api <- request(
  "https://api.gbif.org/v1/literature/search?year=2005,2023&limit=0&facet=year&peerReview=true") |>
  req_perform() |>
  resp_body_json() 
# extract tibble
gbif_totals <- gbif_api |>
  pluck(!!!list("facets", 1, "counts")) |>
  bind_rows() |>
  rename("year" = "name",
         "n_gbif" = "count") |>
  arrange(year)
gbif_totals$year <- as.numeric(gbif_totals$year)

# count df
df <- ala_cited |>
  left_join(gbif_cited) |>
  left_join(gbif_totals) |>
  filter(year <= 2023)
# clean
df$n_ala_via_gbif[is.na(df$n_ala_via_gbif)] <- 0
df$n_gbif[is.na(df$n_gbif)] <- 0
df$n_gbif_not_ala <- df$n_gbif - df$n_ala_via_gbif

# restructure for ggplot2
df2 <- tibble(
  year = rep(df$year, 3),
  type = factor(
    rep(c(3:1), each = nrow(df)),
    labels = c("GBIF without AU data", "AU data via GBIF", "AU data via ALA")),
  count = c(df$n_ala, df$n_ala_via_gbif, df$n_gbif_not_ala)
)

# draw
p <- ggplot(df2, 
       aes(x = year, y = count, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y = "Number of Citations", fill = "Category") +
  scale_fill_manual(values = c("#8fcceb", "#239e60", "#83d166")) +
  theme_bw()

ggsave("citation_plot_mw.pdf", p, width = 8, height = 6, units = "in")

# Investigation:
# In 2023, 274 articles cited ALA directly, and another 564 cited ALA via gbif (836 total).
# GBIF received 3402 citations, of which 506 included ALA data (14.9%).
# (Note the above does not include the 274 citations directly to the ALA in 2023)
# At this point (2023), GBIF contained 89,063,896 records from ALA
# out of a total of 2,080,864,480 (4.3% of records)

# In 2023, 274 / (274 + 564) = 33% 

library(galah)
galah_config(atlas = "gbif")
# show_all_fields()
galah_call() |>
  filter(country == "AU",
         year <= 2023) |>
  count() |>
  collect()


# now look at a plotting article types as a treemap 
ala_raw <- read_csv("papers_citing_ala.csv") |>
  clean_names()

# group some categoies
ala_raw$item_type[which(ala_raw$item_type == "manuscript")] <- "journalArticle"
ala_raw$item_type[which(ala_raw$item_type %in% c("book", "bookSection"))] <- "bookOrBookSection"
ala_raw$item_type[which(ala_raw$item_type %in% c("magazineArticle", "encyclopediaArticle"))] <- "magazineOrEncyclopedia"
ala_raw$item_type[which(ala_raw$item_type %in% c("artwork", "videoRecording"))] <- "mixedMedia"

ala_types <- ala_raw |>
  group_by(item_type) |>
  summarize(count = n())

# messy code for formatting groups and labels
ala_types$group_type <- 1
group2 <- ala_types$item_type %in% c("blogPost", "document", "magazineOrEncyclopedia", 
                                     "mixedMedia", "newspaperArticle", "report", "webpage")
ala_types$group_type[group2] <- 2
ala_types$item_type <- to_sentence_case(ala_types$item_type)
ala_types$item_type <- lapply(
  strsplit(ala_types$item_type, "\\s"),
  function(x){paste(x, collapse = "\n")}) |>
  unlist()
ala_types$item_type[ala_types$item_type == "Book\nor\nbook\nsection"] <- "Book or\nBook section"
ala_types$group_type <- factor(
  ala_types$group_type,
  levels = c(1, 2),
  labels = c("Scientific content",
             "Grey lit & cultural content"))

# add colors manually
ala_types <- ala_types |>
  arrange(group_type, desc(count))
ala_types$fill_col <- c("#d8f5ce", 
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
                        "#8a6c28")

q <- ggplot(ala_types,
       aes(area = count, 
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

ggsave("paper_type_heatmap.pdf", width = 5, height = 5, units = "in")

# analysis
# ala_types |>
#   group_by(group_type) |>
#   summarize(n = sum(count))
# 
# 3402 / sum(ala_types$count)
# 80% of records are for scientific content
# largest entry is Journal articles, with 2705 / 4248 entries (64%)
