## ------------------------------------------------------#
## Title: General temporal trends of ALA
## Author: Dax Kellie
## Date Created: 2021-05-7
## ------------------------------------------------------#


rm(list = ls())

# packages

# remotes::install_github("AtlasOfLivingAustralia/galah") # installs latest version
library(galah)
library(data.table)
library(dtplyr) # provides data.table backend for dplyr
library(tidyverse)
library(lubridate) # for dates
library(patchwork)


#------------------------------------------------------#
#   Trends in time of year of recording data in ALA
#------------------------------------------------------#


find_field_values("basisOfRecord")


# Get data

# Might need to config email to get ALA data
# ala_config(email = )

# How many occurrence records in Tasmania over last 10 years?
ala_counts(filters = select_filters(stateProvince = "Tasmania",
                                    year = seq(2010, 2020),
                                    basisOfRecord = "HumanObservation"))

# Get all records
# occurrences_Tas <- ala_occurrences(filters = select_filters(stateProvince = "Tasmania",
#                                                year = seq(2010, 2020),
#                                                basisOfRecord = "HumanObservation"))

# Call saved local data file
#   ./Data Holdings/data_ALA/eventDate/TAS_occurrences.rds"
filename <- file.choose()
occurrences_Tas <- readRDS(filename)


occurrences_Tas <- setDT(occurrences_Tas) # make data.table


# add day, week
occurrences_Tas$eventDate <- as_date(occurrences_Tas$eventDate)
occurrences_Tas$weekday <- wday(occurrences_Tas$eventDate, label = TRUE) # day (sun,mon,etc)
occurrences_Tas$day <- yday(occurrences_Tas$eventDate) # day (julian)
occurrences_Tas$week <- week(occurrences_Tas$eventDate) # week
occurrences_Tas$month <- month(occurrences_Tas$eventDate, label = TRUE) # month
occurrences_Tas$year <- year(occurrences_Tas$eventDate) # year


# remove NAs
occurrences_Tas <- occurrences_Tas %>% filter(!is.na(eventDate)) %>% as.data.table() # remove NA
# any(is.na(occurrences_Tas$day))



#------------------------------------------------------#
#     Time series of records over last 10 years
#------------------------------------------------------#


# every day
everyday_counts <- occurrences_Tas[, .N, by = c("eventDate", "year")]

# Over 10 years continuous
occ_10years <-  everyday_counts[everyday_counts$year > 2011,] %>% 
  ggplot(aes(x = eventDate, 
             y = N)) + 
  geom_line(aes(colour = factor(year))) + 
  theme_minimal() + 
  scale_colour_viridis_d() + 
  theme(legend.position = "none") + 
  labs(x = "\nDay",
       y = "")

# Each year
everyday_counts_by_year <- occurrences_Tas[, .N, by = c("day", "year")]

occ_by_year <- everyday_counts_by_year[everyday_counts_by_year$year > 2011,] %>% 
  ggplot(aes(x = day, 
             y = N)) + 
  geom_line(aes(colour = factor(year)), alpha = 0.9) + 
  theme_minimal() + 
  scale_colour_viridis_d() + 
  facet_wrap(.~year) + 
  labs(x = "",
       y = "Number of Records\n") + 
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())


occ_by_year / occ_10years + plot_layout(ncol=1,heights=c(3,1))



#----------------------------------------------#
#   Which days are most popular for records?
#----------------------------------------------#

# get counts
day_counts <- occurrences_Tas[, .N, by = c("weekday")]

# Total number of occurrences on each day
p_day_of_week <- day_counts %>% ggplot(aes(y = N, x = weekday)) + 
  geom_bar(stat = "identity", fill = "#E06E53", width = .8) +
  labs(title = "Total number of records each day",
       subtitle = "Tasmania (2010 - 2020)",
       y = "Number of records\n",
       x = "\nDay") +
  theme_minimal() + scale_fill_viridis_d() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))
p_day_of_week



# get month counts
month_counts <- occurrences_Tas[, .N, by = c("month")]
month_counts$month <- ordered(factor(month_counts$month, 
                                     levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

# Total number of occurrences on each day
p_month_of_year <- month_counts %>% ggplot(aes(x = month, y = N)) + 
  geom_bar(stat = "identity", fill = "#E06E53", width = .8) +
  labs(title = "Total number of records",
       subtitle = "Tasmania (2010 - 2020)",
       y = "Day\n",
       x = "\nNumber of records") +
  theme_minimal() + scale_fill_viridis_d() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))


library(ggpubr)
p_month_of_year_dot <- ggdotchart(month_counts, x = "month", y = "N",
                              color = "month", # Color by groups
                              palette = "viridis", # Custom color palette
                              sorting = "none", # Sort value in descending order
                              add = "segments", # Add segments from y = 0 to dots
                              legend = "none",
                              rotate = FALSE,
                              dot.size = 4,
                              add.params = list(color = "month", size = 1),
                              ylab = "Number of records",
                              xlab = "Month",
                              ggtheme = theme_pubr() # ggplot2 theme
)

p_day_of_week + p_month_of_year_dot + plot_layout(ncol=1,widths=c(2,1))
