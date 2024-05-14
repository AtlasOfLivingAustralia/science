# comparison of 2022 and 2023 user satisfaction surveys
# mainly taken from qmd file by Dax, but copied here for speed
setwd("path/to/folder/")
##### Packages #####
library(tidyverse)
library(scales)
library(here)
library(janitor)
library(glue)
library(ggtext)
library(patchwork)
remotes::install_github("olihawkins/pilot")
library(pilot) # themes + palettes

##### Data Loading + Cleaning #####

df_2022 <- here("data", "2022-survey-all-responses.csv") |>
  read_csv() |>
  clean_names() |>
  rename(used_ALA = 10,
         how_satisfied = 12,
         likely_to_recommend = 13,
         reason_to_recommend = 14,
         sector_affiliated = 15,
         sector_other = 16) |>
  select(respondent_id,
         email_address,
         used_ALA,
         how_satisfied,
         likely_to_recommend,
         sector_affiliated) |>
  mutate(
    how_satisfied = as.numeric(gsub("[^0-9.-]", "", how_satisfied)),
    likely_to_recommend = as.numeric(gsub("[^0-9.-]", "", likely_to_recommend)),
    sector_affiliated = gsub("[^0-9A-Za-z///' ]", "", sector_affiliated, ignore.case = TRUE) |>
      str_replace_all("  ", " "),
    year = 2022) |>
  mutate(
    sector_affiliated = case_when(
      sector_affiliated == "Community based organisation club society landcare" | 
        sector_affiliated == "First Nations organisation" | 
        sector_affiliated == "Wildlife Park sanctuary zoo aquarium wildlife rescue" | 
        sector_affiliated == "Not for profit organisation" ~ 
        "Community, conservation and First Nations organisations",
      
      sector_affiliated == "Education primary and secondary schools TAFE environmental or wildlife education" ~
        "Education",
      
      sector_affiliated == "Government federal state and local" ~
        "Government",
      
      sector_affiliated == "Industry commercial business or retail" ~
        "Industry/commercial",
      
      sector_affiliated == "Medical Research Institute MRI" |
        sector_affiliated == "Publicly Funded Research Agency PFRA includes CSIRO ANSTO AIMS AAO AIATSIS AAD DSTO GA BoM" |
        sector_affiliated == "University faculty researcher" |
        sector_affiliated == "University general staff administration management" | 
        sector_affiliated == "Other research organisation unaffiliated researcher" ~
        "Research",
      
      sector_affiliated == "Museum herbarium library botanic gardens" ~
        "Collections",
      
      sector_affiliated == "Private user" |
        sector_affiliated == "Volunteer citizen scientist" |
        sector_affiliated == "University student" ~
        "Individuals",
      
      sector_affiliated == "Other" ~ "Other",
      sector_affiliated == "Prefer not to say" ~ as.character(NA),
      
      TRUE ~ as.character(NA)
    ))


df_2023 <- here("data", "2023-survey-all-responses.csv") |>
  read_csv() |>
  clean_names() |>
  rename(used_ALA = 10,
         how_satisfied = 21,
         likely_to_recommend = 22,
         reason_to_recommend = 23,
         sector_affiliated = 24) |>
  select(respondent_id,
         email_address,
         used_ALA,
         how_satisfied,
         likely_to_recommend,
         sector_affiliated) |>
  mutate(
    how_satisfied = as.numeric(gsub("[^0-9.-]", "", how_satisfied)),
    likely_to_recommend = as.numeric(gsub("[^0-9.-]", "", likely_to_recommend)),
    year = 2023) |>
  mutate(
    sector_affiliated = case_when(
      sector_affiliated == "Community-based organisation, club, society" |
      sector_affiliated == "Indigenous group or organisation" |
      sector_affiliated == "Citizen science group" ~ 
        "Community, conservation and First Nations organisations",
      
      sector_affiliated == "Collections (including museums, herbaria, libraries and botanic gardens)" ~
        "Collections",
      
      sector_affiliated == "Education (primary, secondary and tertiary)" ~ "Education",
      sector_affiliated == "Government (federal, state and local)" ~ "Government",
      sector_affiliated == "Industry and business" ~ "Industry/commercial",
      sector_affiliated == "Research user (including university, government, industry, collections or independent)" ~ "Research",
      sector_affiliated == "Other (please specify)" ~ "Other",
      sector_affiliated == "Private user" ~ "Individuals"))

df <- bind_rows(df_2022, df_2023)

##### Colour Palette #####
year_palette <- c("#1b9e77","#d95f02") |>
  setNames(c(2022, 2023))

##### Sectors #####

sector_year_count <- df |> 
  filter(!is.na(sector_affiliated)) |>
  group_by(year) |>
  count()
  
sector_df <- df |>
  group_by(year, sector_affiliated) |>
  summarise(count = n()) |>
  arrange(sector_affiliated) |>
  drop_na(sector_affiliated) |>
  full_join(sector_year_count) |>
  mutate(percent = count / n,
         sector_affiliated = str_wrap(sector_affiliated, width = 30),
         year = factor(year, levels = sort(sector_year_count$year, decreasing = TRUE)))

sector_order <- 
  sector_df %>% 
  filter(year == 2023) %>% 
  arrange(count) %>%
  pull(sector_affiliated)

plot_sector <- ggplot(sector_df,
                      aes(
                        x = factor(sector_affiliated, levels = sector_order, ordered = TRUE),
                        y = percent, 
                        fill = year,
                        group = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  # scale_fill_manual(values = custom_palette) +     
  geom_text(mapping = aes(label = glue("{count} ({scales::label_percent(accuracy = 0.1)(percent)})")),
                  color = "grey30",
                  size = 5,
                  hjust = -0.3,
                  fontface = "bold",
                  position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(breaks = c(0, .25, .50, .75, 1),
                     expand = c(0, 0),
                     labels = scales::label_percent(),
                     limits = c(0, 1)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = year_palette) +     
  pilot::theme_pilot(grid = "",
                     axes = "b") +
  coord_flip() +
  labs(
    x = "", y = "",
    title = glue("What sector are you affiliated with?"),
    subtitle = glue("<span style='color:{unname(year_palette[1])};'>**2022** (N = {sector_year_count$n[1]})</span> **vs**
                     <span style='color:{unname(year_palette[2])};'>**2023** (N = {sector_year_count$n[2]})</span>")
  ) +
  theme(legend.position = "none",
        axis.line.x.bottom = element_line(linewidth = 1.1),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(size = 14),
        plot.title.position = "plot"
  )

plot_sector
ggsave(filename = "plots/sector_plot.pdf",
       plot_sector, height = 8, width = 10, units = "in")
ggsave(filename = "plots/sector_plot.png",
       plot_sector, height = 8, width = 10, units = "in")


##### Use ALA #####

used_year_count <- df |> 
  filter(!is.na(used_ALA)) |>
  group_by(year) |>
  count()

used_df <- df |>
  group_by(year, used_ALA) |>
  summarise(count = n()) |>
  arrange(used_ALA) |>
  drop_na(used_ALA) |>
  full_join(used_year_count) |>
  mutate(percent = count / n,
         year = factor(year, levels = sort(sector_year_count$year, decreasing = TRUE)))

used_order <- 
  used_df %>% 
  filter(year == 2023) %>% 
  arrange(count) %>%
  pull(used_ALA)

plot_used <- ggplot(used_df,
                      aes(
                        x = factor(used_ALA, levels = used_order, ordered = TRUE),
                        y = percent, 
                        fill = year,
                        group = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  # scale_fill_manual(values = custom_palette) +     
  geom_text(mapping = aes(label = glue("{count} ({scales::label_percent(accuracy = 0.1)(percent)})")),
            color = "grey30",
            size = 5,
            hjust = -0.05,
            fontface = "bold",
            position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(breaks = c(0, .25, .50, .75, 1),
                     expand = c(0, 0),
                     labels = scales::label_percent(),
                     limits = c(0, 1)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = year_palette) +     
  pilot::theme_pilot(grid = "",
                     axes = "b") +
  coord_flip() +
  labs(
    x = "", y = "",
    title = glue("Have you used the ALA in the last financial year?"),
    subtitle = glue("<span style='color:{unname(year_palette[1])};'>**2022** (N = {used_year_count$n[1]})</span> **vs**
                     <span style='color:{unname(year_palette[2])};'>**2023** (N = {used_year_count$n[2]})</span>")
  ) +
  theme(legend.position = "none",
        axis.line.x.bottom = element_line(linewidth = 1.1),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(size = 14),
        plot.title.position = "plot"
  )

plot_used
ggsave(filename = "plots/used_plot.pdf",
       plot_used, height = 6, width = 10, units = "in")
ggsave(filename = "plots/used_plot.png",
       plot_used, height = 6, width = 10, units = "in")

##### Satisfaction (split) #####

sat_year_count <- df |> 
  filter(!is.na(how_satisfied)) |>
  group_by(year) |>
  count()

sat_df <- df |>
  group_by(year, how_satisfied) |>
  summarise(count = n()) |>
  arrange(how_satisfied) |>
  drop_na(how_satisfied) |>
  full_join(sat_year_count) |>
  mutate(percent = count / n)


plot_sat_23 <- ggplot(sat_df %>% filter(year == 2023),
                      aes(
                        x = how_satisfied, 
                        y = percent, 
                        fill = how_satisfied)) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values = custom_palette) +     
  geom_text(mapping = aes(label = glue("{count} ({scales::label_percent(accuracy = 0.1)(percent)})")),
            color = "grey30",
            size = 5,
            hjust = -0.2,
            fontface = "bold",
  ) +
  scale_y_continuous(breaks = c(0, .25, .50, .75, 1),
                     expand = c(0, 0),
                     labels = scales::label_percent(),
                     limits = c(0, 0.6)) +
  scale_x_continuous(breaks = c(1:7),
                     expand = c(0, 0),
                     labels = c("1\nVery dissatisfied", "2", "3", "4", "5", "6", "Very satisfied\n7")) +
  scale_fill_gradient2(low = "#2f7ab9", 
                       high = year_palette[2], 
                       mid = "grey80", 
                       midpoint = 4) +
  pilot::theme_pilot(grid = "",
                     axes = "b") +
  coord_flip() +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.text.y = element_text(hjust = 0.5, margin = margin(r = 20)),
        axis.line.x.bottom = element_line(size = 1.1),
        plot.margin = margin(0.5, 0, 0, 0, "pt")
  )
plot_sat_23

plot_sat_22 <- ggplot(sat_df %>% filter(year == 2022),
                      aes(
                        x = how_satisfied, 
                        y = -percent, 
                        fill = how_satisfied)) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values = custom_palette) +     
  geom_text(mapping = aes(label = glue("{count} ({scales::label_percent(accuracy = 0.1)(percent)})")),
            color = "grey30",
            size = 5,
            hjust = 1.2,
            fontface = "bold"
  ) +
  scale_y_continuous(breaks = c(0, -.25, -.50, -.75, -1),
                     expand = c(0, 0),
                     labels = scales::label_percent(),
                     limits = c(-0.6, 0)) +
  scale_x_continuous(breaks = c(1:7),
                     expand = c(0, 0),
                     labels = NULL) +
  scale_fill_gradient2(low = "#2f7ab9", 
                       high = year_palette[1], 
                       mid = "grey80", 
                       midpoint = 4) +
  pilot::theme_pilot(grid = "",
                     axes = "b") +
  coord_flip() +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.text.y = element_text(hjust = 0.5),
        axis.line.x.bottom = element_line(size = 1.1),
        plot.margin = margin(0.5, 0.6, 0, 0, "cm")
  )
plot_sat_22

plot_sat_split <- plot_sat_22 + plot_sat_23 +
  plot_annotation(
    title = glue("How satisfied are you with the ALA?"),
    subtitle = glue("<span style='color:{unname(year_palette[1])};'>**2022** (N = {used_year_count$n[1]})</span> **vs**
                     <span style='color:{unname(year_palette[2])};'>**2023** (N = {used_year_count$n[2]})</span>"),
    theme = theme(
      plot.title = element_markdown(size = 17, face = "bold"),
      plot.subtitle = element_markdown(size = 14),
      plot.title.position = "plot",
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  )
plot_sat_split

ggsave(filename = "plots/sat_plot_split.pdf",
       plot_sat_split, height = 6, width = 10, units = "in")
ggsave(filename = "plots/sat_plot_split.png",
       plot_sat_split, height = 6, width = 10, units = "in")

##### Satisfaction (together) #####

sat_year_count <- df |> 
  filter(!is.na(how_satisfied)) |>
  group_by(year) |>
  count()

sat_df <- df |>
  group_by(year, how_satisfied) |>
  summarise(count = n()) |>
  arrange(how_satisfied) |>
  drop_na(how_satisfied) |>
  full_join(sat_year_count) |>
  mutate(percent = count / n,
         year = factor(year))


plot_sat <- ggplot(sat_df,
                   aes(
                     x = how_satisfied, 
                     y = percent, 
                     fill = year,
                     group = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  # scale_fill_manual(values = custom_palette) +     
  geom_text(mapping = aes(label = glue("{count} ({scales::label_percent(accuracy = 0.1)(percent)})")),
            color = "grey30",
            size = 5,
            hjust = -0.2,
            fontface = "bold",
            position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(breaks = c(0, .25, .50, .75, 1),
                     expand = c(0, 0),
                     labels = scales::label_percent(),
                     limits = c(0, 0.6)) +
  scale_x_continuous(breaks = c(1:7),
                     expand = c(0, 0),
                     labels = c("1\nVery dissatisfied", "2", "3", "4", "5", "6", "Very satisfied\n7")) +
  scale_fill_manual(values = year_palette) +
  pilot::theme_pilot(grid = "",
                     axes = "b") +
  coord_flip() +
  labs(
    x = "", y = "",
    title = glue("How satisfied are you with the ALA?"),
    subtitle = glue("<span style='color:{unname(year_palette[1])};'>**2022** (N = {used_year_count$n[1]})</span> **vs**
                     <span style='color:{unname(year_palette[2])};'>**2023** (N = {used_year_count$n[2]})</span>")
  ) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.text.y = element_text(margin = margin(r = 20)),
        axis.line.x.bottom = element_line(size = 1.1),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(size = 14),
        plot.title.position = "plot"
  )
plot_sat

ggsave(filename = "plots/sat_plot.pdf",
       plot_sat, height = 8, width = 10, units = "in")
ggsave(filename = "plots/sat_plot.png",
       plot_sat, height = 8, width = 10, units = "in")


##### Recommend (split) #####

rec_year_count <- df |> 
  filter(!is.na(likely_to_recommend)) |>
  group_by(year) |>
  count()

rec_df <- df |>
  group_by(year, likely_to_recommend) |>
  summarise(count = n()) |>
  arrange(likely_to_recommend) |>
  drop_na(likely_to_recommend) |>
  full_join(rec_year_count) |>
  mutate(percent = count / n)


plot_rec_23 <- ggplot(rec_df %>% filter(year == 2023),
                      aes(
                        x = likely_to_recommend, 
                        y = percent, 
                        fill = likely_to_recommend)) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values = custom_palette) +     
  geom_text(mapping = aes(label = glue("{count} ({scales::label_percent(accuracy = 0.1)(percent)})")),
            color = "grey30",
            size = 5,
            hjust = -0.2,
            fontface = "bold",
  ) +
  scale_y_continuous(breaks = c(0, .25, .50, .75, 1),
                     expand = c(0, 0),
                     labels = scales::label_percent(),
                     limits = c(0, 0.6)) +
  scale_x_continuous(breaks = c(0:10),
                     expand = c(0, 0),
                     labels = c("Very unlikely\n0", "1", "2", "3", "4", "5", 
                                "6", "7", "8", "9", "Very likely\n10")) +
  scale_fill_gradient2(low = "#2f7ab9", 
                       high = year_palette[2], 
                       mid = "grey80", 
                       midpoint = 5) +
  pilot::theme_pilot(grid = "",
                     axes = "b") +
  coord_flip() +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.text.y = element_text(hjust = 0.5, margin = margin(r = 20)),
        axis.line.x.bottom = element_line(size = 1.1),
        plot.margin = margin(0.5, 0, 0, 0, "pt")
  )
plot_rec_23

plot_rec_22 <- ggplot(rec_df %>% filter(year == 2022),
                      aes(
                        x = likely_to_recommend, 
                        y = -percent, 
                        fill = likely_to_recommend)) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values = custom_palette) +     
  geom_text(mapping = aes(label = glue("{count} ({scales::label_percent(accuracy = 0.1)(percent)})")),
            color = "grey30",
            size = 5,
            hjust = 1.2,
            fontface = "bold"
  ) +
  scale_y_continuous(breaks = c(0, -.25, -.50, -.75, -1),
                     expand = c(0, 0),
                     labels = scales::label_percent(),
                     limits = c(-0.6, 0)) +
  scale_x_continuous(breaks = c(0:10),
                     expand = c(0, 0),
                     labels = NULL) +
  scale_fill_gradient2(low = "#2f7ab9", 
                       high = year_palette[1], 
                       mid = "grey80", 
                       midpoint = 5) +
  pilot::theme_pilot(grid = "",
                     axes = "b") +
  coord_flip() +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.text.y = element_text(hjust = 0.5),
        axis.line.x.bottom = element_line(size = 1.1),
        plot.margin = margin(0.5, 0.6, 0, 0, "cm")
  )
plot_rec_22

plot_rec_split <- plot_rec_22 + plot_rec_23 +
  plot_annotation(
    title = glue("How likely are you to recommend the ALA?"),
    subtitle = glue("<span style='color:{unname(year_palette[1])};'>**2022** (N = {used_year_count$n[1]})</span> **vs**
                     <span style='color:{unname(year_palette[2])};'>**2023** (N = {used_year_count$n[2]})</span>"),
    theme = theme(
      plot.title = element_markdown(size = 17, face = "bold"),
      plot.subtitle = element_markdown(size = 14),
      plot.title.position = "plot",
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  )
plot_rec_split

ggsave(filename = "plots/rec_plot_split.pdf",
       plot_rec_split, height = 8, width = 10, units = "in")
ggsave(filename = "plots/rec_plot_split.png",
       plot_rec_split, height = 8, width = 10, units = "in")

##### Satisfaction (together) #####

rec_year_count <- df |> 
  filter(!is.na(likely_to_recommend)) |>
  group_by(year) |>
  count()

rec_df <- df |>
  group_by(year, likely_to_recommend) |>
  summarise(count = n()) |>
  arrange(likely_to_recommend) |>
  drop_na(likely_to_recommend) |>
  full_join(rec_year_count) |>
  mutate(percent = count / n,
         year = factor(year))


plot_rec <- ggplot(rec_df,
                   aes(
                     x = likely_to_recommend, 
                     y = percent, 
                     fill = year,
                     group = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  # scale_fill_manual(values = custom_palette) +     
  geom_text(mapping = aes(label = glue("{count} ({scales::label_percent(accuracy = 0.1)(percent)})")),
            color = "grey30",
            size = 5,
            hjust = -0.2,
            fontface = "bold",
            position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(breaks = c(0, .25, .50, .75, 1),
                     expand = c(0, 0),
                     labels = scales::label_percent(),
                     limits = c(0, 0.6)) +
  scale_x_continuous(breaks = c(0:10),
                     expand = c(0, 0),
                     labels = c("Very unlikely\n0", "1", "2", "3", "4", "5", 
                                "6", "7", "8", "9", "Very likely\n10")) +
  scale_fill_manual(values = year_palette) +
  pilot::theme_pilot(grid = "",
                     axes = "b") +
  coord_flip() +
  labs(
    x = "", y = "",
    title = glue("How likely are you to recommend the ALA?"),
    subtitle = glue("<span style='color:{unname(year_palette[1])};'>**2022** (N = {used_year_count$n[1]})</span> **vs**
                     <span style='color:{unname(year_palette[2])};'>**2023** (N = {used_year_count$n[2]})</span>")
  ) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.text.y = element_text(margin = margin(r = 20)),
        axis.line.x.bottom = element_line(size = 1.1),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(size = 14),
        plot.title.position = "plot"
  )
plot_rec

ggsave(filename = "plots/rec_plot.pdf",
       plot_rec, height = 8, width = 10, units = "in")
ggsave(filename = "plots/rec_plot.png",
       plot_rec, height = 8, width = 10, units = "in")
