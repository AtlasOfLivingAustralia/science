## ------------------------------------------------------#
## Title: Temporal ALA Investigation
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
ala_config(email = "dax.kellie@csiro.au")

# How many occurrence records in Tasmania over last 10 years?
ala_counts(filters = select_filters(stateProvince = "Tasmania",
                                    year = seq(2010, 2020),
                                    basisOfRecord = "HumanObservation"))

# Get all records
# occurrences_Tas <- ala_occurrences(filters = select_filters(stateProvince = "Tasmania",
#                                                year = seq(2010, 2020),
#                                                basisOfRecord = "HumanObservation"))

# Call saved local data file
path <- "C:/Users/KEL329/OneDrive - CSIRO/Documents/Projects/Data Holdings/data_ALA"
data_filepath <- file.path(path, "tasmania_occurrences.rds")
occurrences_Tas <- readRDS(file=data_filepath)
# filename <- file.choose()
# occurrences_Tas <- readRDS(filename)

# occurrences_Tas <- occurrences_Tas %>% filter(!is.na(eventDate)) # remove NA
occurrences_Tas <- setDT(occurrences_Tas) # make data.table


# add day, week
occurrences_Tas$eventDate <- as_date(occurrences_Tas$eventDate)

occurrences_Tas$weekday <- wday(occurrences_Tas$eventDate, label = TRUE) # day (sun,mon,tues, etc)
occurrences_Tas$week <- week(occurrences_Tas$eventDate) # week
occurrences_Tas$day <- yday(occurrences_Tas$eventDate) # day (julian)
occurrences_Tas$month <- month(occurrences_Tas$eventDate, label = TRUE) # month
occurrences_Tas$year <- year(occurrences_Tas$eventDate) # year
?month()

# remove NAs
occurrences_Tas <- occurrences_Tas %>% filter(!is.na(eventDate)) %>% as.data.table() # remove NA
any(is.na(occurrences_Tas$day))


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
month_counts$month <- ordered(factor(month_counts$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
levels(month_counts$month)

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
                              rotate = TRUE,
                              dot.size = 4,
                              add.params = list(color = "month", size = 1),
                              ylab = "Number of records",
                              xlab = "Month",
                              ggtheme = theme_pubr() # ggplot2 theme
)

p_day_of_week + p_month_of_year_dot + plot_layout(ncol=1,widths=c(2,1))





#----------------------------------------------#
#                   spline
#----------------------------------------------#

# set time interval of interest
day_interval <- seq(ymd("2012-01-01"), ymd("2020-12-31"), by = "days")



# set dates to date format
occurrences_Tas$eventDate <- ymd(occurrences_Tas$eventDate)

# set up date dataset
date_df <- data.frame(
  date = day_interval,
  date_numeric = as.numeric(day_interval),
  date_julian = yday(day_interval)
)

# count records
date_df$record_count <- unlist(lapply(
  date_df$date,
  function(a){
    return(length(which(occurrences_Tas$eventDate == a)))
  }))


# model using GAMs
# install.packages("mgcv")
library(mgcv)
date_df$date_scaled <- scale(date_df$date_numeric)
date_df$julian_scaled <- scale(date_df$date_julian)


# try a model with interacting date terms
model <- gam(record_count ~ s(date_scaled, by = julian_scaled),
             data = date_df,
             family = poisson(link = "log"))

# make predictions
# first of change over time
model <- gam(record_count ~ s(date_scaled) + s(julian_scaled),
             data = date_df,
             family = poisson(link = "log"))

prediction_1 <- data.frame(
  date_scaled = seq(
    min(date_df$date_scaled),
    max(date_df$date_scaled),
    length.out = 100),
  julian_scaled = 0)
prediction_1$date_unscaled <- seq(min(date_df$date), max(date_df$date), length.out = 100) # readjusting scale for plotting

model_prediction <- predict(model, newdata = prediction_1, se.fit = TRUE)
prediction_1$fit <- exp(model_prediction$fit)
prediction_1$lci <- exp(model_prediction$fit - (2 * model_prediction$se.fit))
prediction_1$uci <- exp(model_prediction$fit + (2 * model_prediction$se.fit))

# then repeat for Julian Date
prediction_2 <- data.frame(
  julian_scaled = seq(
    min(date_df$julian_scaled),
    max(date_df$julian_scaled),
    length.out = 100),
  date_scaled = 0)
prediction_2$date_unscaled <- seq(min(date_df$date_julian), max(date_df$date_julian), length.out = 100)

model_prediction <- predict(model, newdata = prediction_2, se.fit = TRUE)
prediction_2$fit <- exp(model_prediction$fit)
prediction_2$lci <- exp(model_prediction$fit - (2 * model_prediction$se.fit))
prediction_2$uci <- exp(model_prediction$fit + (2 * model_prediction$se.fit))

# finally calculate residuals from the model
date_df$residuals <- resid(model)


# draw
library(patchwork)

a <- ggplot(prediction_1, aes(x = date_unscaled, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "#ff6e6e") +
  geom_path() +
  theme_bw() +
  labs(x = "Year", y = "Number of Records")

b <- ggplot(prediction_2, aes(x = date_unscaled, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "#6e9eff") +
  geom_path() +
  theme_bw() + 
  labs(x = "Day of year", y = "Number of Records")

c <- ggplot(date_df, aes(x = date, y = residuals)) +
  geom_path() +
  # geom_point() +
  theme_bw()

a / b / c








# WA



# set time interval of interest
day_interval <- seq(ymd("2012-01-01"), ymd("2020-12-31"), by = "days")



# set dates to date format
occurrences_WA$eventDate <- ymd(occurrences_WA$eventDate)

# set up date dataset
date_df <- data.frame(
  date = day_interval,
  date_numeric = as.numeric(day_interval),
  date_julian = yday(day_interval)
)

# count records
date_df$record_count <- unlist(lapply(
  date_df$date,
  function(a){
    return(length(which(occurrences_WA$eventDate == a)))
  }))


# model using GAMs
# install.packages("mgcv")
library(mgcv)
date_df$date_scaled <- scale(date_df$date_numeric)
date_df$julian_scaled <- scale(date_df$date_julian)


# try a model with interacting date terms
model <- gam(record_count ~ s(date_scaled, by = julian_scaled),
             data = date_df,
             family = poisson(link = "log"))

# make predictions
# first of change over time
model <- gam(record_count ~ s(date_scaled) + s(julian_scaled),
             data = date_df,
             family = poisson(link = "log"))

prediction_1 <- data.frame(
  date_scaled = seq(
    min(date_df$date_scaled),
    max(date_df$date_scaled),
    length.out = 100),
  julian_scaled = 0)
prediction_1$date_unscaled <- seq(min(date_df$date), max(date_df$date), length.out = 100) # readjusting scale for plotting

model_prediction <- predict(model, newdata = prediction_1, se.fit = TRUE)
prediction_1$fit <- exp(model_prediction$fit)
prediction_1$lci <- exp(model_prediction$fit - (2 * model_prediction$se.fit))
prediction_1$uci <- exp(model_prediction$fit + (2 * model_prediction$se.fit))

# then repeat for Julian Date
prediction_2 <- data.frame(
  julian_scaled = seq(
    min(date_df$julian_scaled),
    max(date_df$julian_scaled),
    length.out = 100),
  date_scaled = 0)
prediction_2$date_unscaled <- seq(min(date_df$date_julian), max(date_df$date_julian), length.out = 100)

model_prediction <- predict(model, newdata = prediction_2, se.fit = TRUE)
prediction_2$fit <- exp(model_prediction$fit)
prediction_2$lci <- exp(model_prediction$fit - (2 * model_prediction$se.fit))
prediction_2$uci <- exp(model_prediction$fit + (2 * model_prediction$se.fit))

# finally calculate residuals from the model
date_df$residuals <- resid(model)


# draw
library(patchwork)

a <- ggplot(prediction_1, aes(x = date_unscaled, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "#ff6e6e") +
  geom_path() +
  theme_bw() +
  labs(x = "Year", y = "Number of Records")

b <- ggplot(prediction_2, aes(x = date_unscaled, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "#6e9eff") +
  geom_path() +
  theme_bw() + 
  labs(x = "Day of year", y = "Number of Records")

c <- ggplot(date_df, aes(x = date, y = residuals)) +
  geom_path() +
  # geom_point() +
  theme_bw()

a / b / c

a <- ggplot(prediction_1, aes(x = date_unscaled, y = fit)) +
  geom_area(aes(ymin = lci, ymax = uci), fill = "#ff6e6e", alpha = 0.2) +
  geom_path() +
  theme_bw() +
  labs(x = "Year", y = "Number of Records")
