# Script to create figures for Biosecurity Alerts Paper
# ALA Flamingo: "#F26649" (with "#b3f249", "#4970f2")
{
  library(readr)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyverse)
  library(pdftools)
  library(showtext)
  library(extrafont)
}

setwd("C:/Users/WAI045/OneDrive - CSIRO/ALA/Biosecurity/paper_figures")
loadfonts(device = "win")

collated_alerts <- read_csv("collated_alerts_data.csv") |>
  filter(eventDate < as.Date("2023-12-31"))
combined_list <- read_csv("combined_list.csv")
unique_taxa <- rbind(
  read_csv("UniqueAlertTaxa.csv"),
  read_xlsx("AlertsAnalysis.xlsx", sheet = 1),
  read_xlsx("AlertsAnalysis.xlsx", sheet = 2)) |>
  distinct() |>
  group_by(verbatimScientificName) |>
  filter(row_number() == 1) |>
  ungroup()
list_data <- read_csv("list_jurisdictions.csv")

##### Figure 1. List Jurisdictions #####
list_data |>
  group_by(Jurisdiction) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(count) |>
  mutate(Jurisdiction = factor(Jurisdiction, ordered = TRUE, levels = Jurisdiction)) |>
  ggplot() +
  geom_bar(aes(x = Jurisdiction, y = count), 
           stat = "identity", fill = "grey80", width = 0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12.5),
                     breaks = seq(0, 12, by = 2)) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family = "Arial"),
        axis.text = element_text(size = unit(7, "pt")),
        axis.title = element_text(size = unit(8, "pt"), face = "bold"),
        axis.line = element_line(linewidth = unit(1, "pt")),
        axis.ticks = element_line(colour = "black")) +
  labs(x = "Jurisdiction",
       y = "Number of Lists")

ggsave(filename = "fig_1_jurisdictions.pdf", device = cairo_pdf,
       width = 7, height = 4, units = "in")
pdf_convert("fig_1_jurisdictions.pdf",
            format = "tiff", dpi = 600,
            filenames = "fig_1_jurisdictions.tiff")
ggsave(filename = "fig_1_jurisdictions.eps", device = cairo_ps,
       width = 7, height = 4, units = "in")
ggsave(filename = "fig_1_jurisdictions.tiff", compression = "lzw",
       width = 7, height = 4, units = "in", dpi = 600)


##### Figure 2. Occurrences by Phylum #####

unique_records_taxa <- collated_alerts |>
  select(recordID, scientificName, taxonConceptID, eventDate, dataResourceName) |>
  distinct() |>
  left_join(unique_taxa, by = c("scientificName" = "verbatimScientificName"))

unique_list_taxa <- combined_list |>
  select(correct_name) |>
  distinct() |>
  left_join(unique_taxa, by = c("correct_name" = "verbatimScientificName"))

unique_records_taxa |>
  group_by(phylum) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(desc(count)) |>
  mutate(phylum = factor(phylum, ordered = TRUE, levels = phylum)) |>
  ggplot() +
  geom_bar(aes(y = phylum, x = count), 
           stat = "identity", fill = "grey80", width = 0.5) +
  geom_text(aes(y = phylum, x = count + ifelse(count > 100, -30, 5), label = count),
            size = unit(7, "pt") * 25.4/72, vjust = 0.5, hjust = 0) +
  scale_x_continuous(expand = c(0, 2), limits = c(0, 820),
                     breaks = seq(0, 820, by = 100)) +
  labs(y = "Phylum",
       x = "Number of Records") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(family = "Arial"),
        axis.text = element_text(size = unit(7, "pt")),
        axis.title = element_text(size = unit(8, "pt"), face = "bold"),
        axis.line = element_line(linewidth = unit(1, "pt")),
        axis.ticks = element_line(colour = "black"))

ggsave(filename = "fig_2_phylums.pdf", device = cairo_pdf,
       width = 7, height = 4, units = "in")
pdf_convert("fig_2_phylums.pdf",
            format = "tiff", dpi = 600,
            filenames = "fig_2_phylums.tiff")
ggsave(filename = "fig_2_phylums.eps", device = cairo_ps,
       width = 7, height = 4, units = "in")
ggsave(filename = "fig_2_phylums.tiff", compression = "lzw",
       width = 7, height = 4, units = "in", dpi = 600)

##### Figure 3. Occurrences by Data Provider #####

unique_records_taxa |>
  group_by(dataResourceName) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(desc(count)) |>
  mutate(dataResourceName = factor(dataResourceName, ordered = TRUE, levels = dataResourceName)) |>
  ggplot() +
  geom_bar(aes(x = dataResourceName, y = count), 
           stat = "identity", fill = "grey80", width = 0.5) +
  geom_text(aes(x = dataResourceName, y = count + ifelse(count > 1000, -50, 15), label = count),
            size = unit(7, "pt") * 25.4/72, vjust = 0) +
  scale_y_continuous(expand = c(0, 6), limits = c(0, 1400),
                     breaks = seq(0, 1400, by = 200)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "Data Resource Provider",
       y = "Number of Records") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family = "Arial"),
        axis.text = element_text(size = unit(7, "pt")),
        axis.title = element_text(size = unit(8, "pt"), face = "bold"),
        axis.line = element_line(linewidth = unit(1, "pt")),
        axis.ticks = element_line(colour = "black"))

ggsave(filename = "fig_3_providers.pdf", device = cairo_pdf,
       width = 7, height = 4, units = "in")
pdf_convert("fig_3_providers.pdf",
            format = "tiff", dpi = 600,
            filenames = "fig_3_providers.tiff")
ggsave(filename = "fig_3_providers.eps", device = cairo_ps,
       width = 7, height = 4, units = "in")
ggsave(filename = "fig_3_providers.tiff", compression = "lzw",
       width = 7, height = 4, units = "in", dpi = 600)

  ##### Figures 4,5. Occurrence Proportions by Kingdom, Phylum #####
# FIG 4 (KINGDOM)
rbind(
  unique_list_taxa |>
    filter(kingdom != "incertae sedis") |>
    group_by(kingdom) |>
    summarise(count = n(), .groups = "drop") |>
    mutate(prop = proportions(count),
           list_records = "List %") |>
    na.omit(),
  unique_records_taxa |>
    group_by(kingdom) |>
    summarise(count = n(), .groups = "drop") |>
    mutate(prop = proportions(count),
           list_records = "Occurrence %") |>
    na.omit()
) |>
  mutate(list_records = factor(list_records, levels = c("List %", "Occurrence %"), ordered = TRUE)) |>
  arrange(list_records, desc(prop)) |>
  mutate(kingdom = factor(kingdom, levels = unique(kingdom), ordered = TRUE)) |>
  ggplot() +
  geom_bar(aes(x = kingdom, y = prop, fill = list_records), 
           stat = "identity", 
           position = position_dodge2(width = 0.5, preserve = "single"), 
           width = 0.8) +
  scale_y_continuous(expand = c(0, 0.004), limits = c(0, 0.9),
                     breaks = seq(0, 0.9, by = 0.1),
                     labels = percent(seq(0, 0.9, by = 0.1))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_fill_manual(name = NULL,
                    values = c("grey20", "grey80")) +
  labs(x = "Kingdom",
       y = "% of Taxa") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family = "Arial"),
        axis.text = element_text(size = unit(7, "pt")),
        axis.title = element_text(size = unit(8, "pt"), face = "bold"),
        axis.line = element_line(linewidth = unit(1, "pt")),
        axis.ticks = element_line(colour = "black"),
        legend.position = "top",
        legend.text = element_text(size = unit(7, "pt")),
        legend.key.spacing.x = unit(15, "pt"))

ggsave(filename = "fig_4_kingdom_prop.pdf", device = cairo_pdf,
       width = 7, height = 4, units = "in")
pdf_convert("fig_4_kingdom_prop.pdf",
            format = "tiff", dpi = 600,
            filenames = "fig_4_kingdom_prop.tiff")
ggsave(filename = "fig_4_kingdom_prop.eps", device = cairo_ps,
       width = 7, height = 4, units = "in")
ggsave(filename = "fig_4_kingdom_prop.tiff", compression = "lzw",
       width = 7, height = 4, units = "in", dpi = 600)

# FIG 5 (PHYLUM))
rbind(
  unique_list_taxa |>
    group_by(phylum) |>
    summarise(count = n(), .groups = "drop") |>
    mutate(prop = proportions(count),
           list_records = "List %") |>
    na.omit(),
  unique_records_taxa |>
    group_by(phylum) |>
    summarise(count = n(), .groups = "drop") |>
    mutate(prop = proportions(count),
           list_records = "Occurrence %") |>
    na.omit()
) |>
  mutate(list_records = factor(list_records, levels = c("List %", "Occurrence %"), ordered = TRUE)) |>
  arrange(list_records, desc(prop)) |>
  mutate(phylum = factor(phylum, levels = unique(phylum), ordered = TRUE)) |>
  ggplot() +
  geom_bar(aes(x = phylum, y = prop, fill = list_records), 
           stat = "identity", 
           position = position_dodge2(width = 0.5, preserve = "single"), 
           width = 0.8) +
  scale_y_continuous(expand = c(0, 0.0023), limits = c(0, 0.7),
                     breaks = seq(0, 0.7, by = 0.1),
                     labels = percent(seq(0, 0.7, by = 0.1))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_fill_manual(name = NULL,
                    values = c("grey20", "grey80")) +
  labs(x = "Phylum",
       y = "% of Taxa") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family = "Arial"),
        axis.text = element_text(size = unit(7, "pt")),
        axis.text.x = element_text(angle = 70, hjust = 1),
        axis.title = element_text(size = unit(8, "pt"), face = "bold"),
        axis.line = element_line(linewidth = unit(1, "pt")),
        axis.ticks = element_line(colour = "black"),
        legend.text = element_text(size = unit(7, "pt")),
        legend.key.spacing.x = unit(15, "pt"),
        legend.position = "top")

ggsave(filename = "fig_5_phylum_prop.pdf", device = cairo_pdf,
       width = 10, height = 6, units = "in")
pdf_convert("fig_5_phylum_prop.pdf",
            format = "tiff", dpi = 600,
            filenames = "fig_5_phylum_prop.tiff")
ggsave(filename = "fig_5_phylum_prop.eps", device = cairo_ps,
       width = 10, height = 6, units = "in")
ggsave(filename = "fig_5_phylum_prop.tiff", compression = "lzw",
       width = 10, height = 6, units = "in", dpi = 600)
