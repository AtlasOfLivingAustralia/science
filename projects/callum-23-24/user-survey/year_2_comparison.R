# comparison of 2022 and 2023 user satisfaction surveys
# mainly taken from qmd file by Dax, but copied here for speed

# packages
library(tidyverse)
library(here)
library(janitor)
library(glue)
library(pilot) # themes + palettes


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

df_year <- df |> 
  filter(!is.na(sector_affiliated)) |>
  group_by(year) |>
  count()
  
df_group <- df |>
  group_by(year, sector_affiliated) |>
  summarise(count = n()) |>
  arrange(sector_affiliated) |>
  drop_na(sector_affiliated) |>
  full_join(df_year) |>
  mutate(percent = (100 * count) / n)


ggplot(df_group, aes(x = sector_affiliated, 
                     y = percent, 
                     fill = as.factor(year),
                     group = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge")


# BELOW HERE IS UNUSED YET

custom_palette <- c(
  "Education (primary, secondary and tertiary)" = "#204466",
  "Research user (including university, government, industry, collections or independent)" = "#227F9A",
  "Private user" = "#B66492",
  "Government (federal, state and local)" = "#B84818",
  "Citizen science group" = "#5D9C62",
  "Other (please specify)" = "#75C662",
  "Industry and business" = "#FFC517",
  "Community-based organisation, club, society" = "#558080",
  "Collections (including museums, herbaria, libraries and botanic gardens)" = "#BB7b99",
  "Indigenous group or organisation" = "#F28100",
  "NA" = "grey70"
)

total_n <- survey_tidy |> drop_na(sector_affiliated) |> nrow()


plot_users <- survey_tidy |>
  drop_na(sector_affiliated) |>
  tabyl(sector_affiliated) |>
  
  ggplot(aes(x = reorder(str_wrap(sector_affiliated, width = 30), n), 
             y = percent)) +
             # fill = sector_affiliated)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", y = "") +
  scale_y_continuous(breaks = c(0, .25, .50, .75, 1),
                     expand = c(0, 0),
                     labels = scales::percent_format(),
                     limits = c(0, 1)) +
  # scale_fill_manual(values = custom_palette) +     
  geom_text_pilot(mapping = aes(label = glue("{n} ({scales::percent(round(percent, 2))})")),
                  color = "grey30",
                  size = 5,
                  hjust = -0.3,
                  fontface = "bold"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  pilot::theme_pilot(grid = "",
                     axes = "b") +
  coord_flip() +
  theme(legend.position = "none",
        axis.line.x.bottom = element_line(linewidth = 1.1)
  )

pilot::add_pilot_titles(plot_users, 
                        title = "What sector are you affiliated with?",
                        subtitle = glue::glue("N = {total_n}"))
