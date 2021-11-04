## ------------------------------------------------------#
## Title: Temporal spline models
## Author: Dax Kellie
## Date Created: 2021-05-26
## ------------------------------------------------------#


#-----------------------------------------------------------------#
# Average number of records added daily to the ALA from 2010 2020 #
#-----------------------------------------------------------------#
#                   Splines by state/territory
#-----------------------------------------------------------------#


library(tidyverse)
library(lubridate)

# Call saved local data files
read_data_in <- function(file_path) {
  data <- readRDS(file = file_path) %>% select(eventDate) # read data, select eventDate col
  data <- data %>%
    mutate(
      state = rep(str_sub(file_path, start = -19, end = -17)) %>% str_remove(c("/")), # get state id
      eventDate = ymd(eventDate)
    ) # set dates to date format
  tib <- tidyr::nest(data, y = c(eventDate)) # nest dates
  names(tib$y) <- as.character(tib$state) # rename nested data to state names
  return(tib)
}

# set local path
path1 <- "C:/Users/KEL329/OneDrive - CSIRO/Documents/ALA/Projects/Data Holdings/data_ALA/eventDate"

# read RDS files into dataframe containing list
occ_nested <- list.files(path = path1, full.names = TRUE) %>%
  map_dfr(read_data_in)
occ_nested



#--------------- Extract day counts data -----------------#

find_daily_record_counts <- function(state_event_dates) {
  
  # time interval of interest
  day_interval <- seq(ymd("2010-01-01"), ymd("2020-12-31"), by = "days")
  
  # set up date dataset
  date_df <- data.frame(
    date = day_interval,
    date_numeric = as.numeric(day_interval),
    date_julian = yday(day_interval)
  )
  
  # count records on each day
  count_daily_records <- function(eventDate, date_df_date) {
    return(length(which(eventDate == date_df_date)))
  }
  
  date_df <- date_df %>% mutate(
    record_count = pmap_dbl(list(state_event_dates, day_interval), count_daily_records)
  )
  # tib <- tidyr::nest(date_df, data = everything()) # nest dataframe output
  # return(tib)
}

# ______________________________________________________________________________
day_counts_ACT <- occ_nested %>% pluck("y", "ACT") %>% find_daily_record_counts(.)
day_counts_NSW <- occ_nested %>% pluck("y", "NSW") %>% find_daily_record_counts(.)
day_counts_NT <- occ_nested %>% pluck("y", "NT") %>% find_daily_record_counts(.)
day_counts_QLD <- occ_nested %>% pluck("y", "QLD") %>% find_daily_record_counts(.)
day_counts_SA <- occ_nested %>% pluck("y", "SA") %>% find_daily_record_counts(.)
day_counts_TAS <- occ_nested %>% pluck("y", "TAS") %>% find_daily_record_counts(.)
day_counts_VIC <- occ_nested %>% pluck("y", "VIC") %>% find_daily_record_counts(.)
day_counts_WA <- occ_nested %>% pluck("y", "WA") %>% find_daily_record_counts(.)
# ______________________________________________________________________________

# This code almost works (but not quite) to loop the above into one line
# x <- occ_nested %>% mutate(date_counts = map_dfr(y, ~ {find_daily_record_counts(.x[1])}))




#-------------- model using GAMs --------------#

library(mgcv)

# Find records over time interval + CIs
calculate_records_over_time_period <- function(df) {

  # scale date for predictions
  df$date_scaled <- scale(df$date_numeric)
  df$julian_scaled <- scale(df$date_julian)
  
  # Run a GAM model
  model <- gam(record_count ~ s(date_scaled) + s(julian_scaled),
               data = df,
               family = poisson(link = "log")
  )
  
  # make prediction dataframe
  prediction_1 <- data.frame(
    date_scaled = seq(
      min(df$date_scaled),
      max(df$date_scaled),
      length.out = 100
    ),
    julian_scaled = 0
  )

  # predict
  model_prediction <- predict(model, newdata = prediction_1, se.fit = TRUE)

  # add fit & CIs to prediction dataframe
  prediction_1 <- prediction_1 %>%
    mutate(
      fit = exp(model_prediction$fit),
      lci = exp(model_prediction$fit - (2 * model_prediction$se.fit)),
      uci = exp(model_prediction$fit + (2 * model_prediction$se.fit)),
      date_unscaled = seq(min(df$date), max(df$date), length.out = 100) # readjusting scale for plotting
    )
  return(prediction_1)
}

#------------------------------------------------------------------------------#
# run prediction function for each state/territory
daily_count_prediction_ACT <- calculate_records_over_time_period(day_counts_ACT)
daily_count_prediction_NSW <- calculate_records_over_time_period(day_counts_NSW)
daily_count_prediction_NT <- calculate_records_over_time_period(day_counts_NT)
daily_count_prediction_QLD <- calculate_records_over_time_period(day_counts_QLD)
daily_count_prediction_SA <- calculate_records_over_time_period(day_counts_SA)
daily_count_prediction_TAS <- calculate_records_over_time_period(day_counts_TAS)
daily_count_prediction_VIC <- calculate_records_over_time_period(day_counts_VIC)
daily_count_prediction_WA <- calculate_records_over_time_period(day_counts_WA)
# ______________________________________________________________________________


#-------------- Plot GAMs --------------#
library(patchwork)
library(pilot)

# merge prediction data frames
data_daily <- bind_rows(daily_count_prediction_ACT,
                   daily_count_prediction_NSW, 
                   daily_count_prediction_NT,
                   daily_count_prediction_QLD,
                   daily_count_prediction_SA,
                   daily_count_prediction_TAS,
                   daily_count_prediction_VIC,
                   daily_count_prediction_WA, .id = c("state")) %>%
  mutate(state = recode(state,
    "1" = "ACT",
    "2" = "NSW",
    "3" = "NT",
    "4" = "QLD",
    "5" = "SA",
    "6" = "TAS",
    "7" = "VIC",
    "8" = "WA"
  ))

set_pilot_family("Lato")

plot_ten_year_trend <- ggplot(
  data = data_daily,
  mapping = aes(x = date_unscaled, y = fit, fill = state, colour = state)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), size = 1) +
  geom_path() +
  guides(fill = guide_legend(title = "State")) +
  theme_pilot(axes = c("bl"),
              grid = "") +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_pilot(breaks = c("QLD", "NSW", "VIC", "NT", "WA", "ACT", "SA", "TAS")) + 
  scale_color_pilot(breaks = "") +
  ylim(c(0, 4500)) +
  labs(x = "Year", y = "Number of Records")

plot_ten_year_trend <- add_pilot_titles(
  plot_ten_year_trend,
  title = "Number of Observation Records Added Daily \nto the Atlas of Living Australia",
  subtitle = "10-year Trend (2010-2021)")

plot_ten_year_trend


# ggsave(here::here("projects", "data_holdings", "temporal", "plots", "2021-09_obs_ten_year_trends.png"))

# _____________________________________________________________






#-----------------------------------------------------------------#
#               Seasonal trends by state/territory
#-----------------------------------------------------------------#

# Prediction function 2
calculate_records_seasonal <- function(df) {

  # scale date for predictions
  df$date_scaled <- scale(df$date_numeric)
  df$julian_scaled <- scale(df$date_julian)
  
  # Run a GAM model
  model <- gam(record_count ~ s(date_scaled, bs = "cc") + s(julian_scaled, bs = "cc"),
               data = df,
               family = poisson(link = "log")
  )
  
  # make prediction dataframe
  prediction_2 <- data.frame(
    julian_scaled = seq(
      min(df$julian_scaled),
      max(df$julian_scaled),
      length.out = 100
    ),
    date_scaled = 0
  )

  # predict
  model_prediction <- predict(model, newdata = prediction_2, se.fit = TRUE)

  # add fit & CIs to prediction dataframe
  prediction_2 <- prediction_2 %>%
    mutate(
      fit = exp(model_prediction$fit),
      fit_prop = fit/sum(fit),
      lci = exp(model_prediction$fit - (2 * model_prediction$se.fit)),
      uci = exp(model_prediction$fit + (2 * model_prediction$se.fit)),
      lci_prop = lci/sum(lci),
      uci_prop = uci/sum(uci),
      julian_unscaled = seq(min(df$date_julian), max(df$date_julian), length.out = 100), # readjusting scale for plotting
      fit_total = sum(fit),
      fit_percent = fit/fit_total * 100,
      lci_percent = lci/fit_total * 100,
      uci_percent = uci/fit_total * 100
    )
  return(prediction_2)
}


#------------------------------------------------------------------------------#
# run prediction function for each state/territory
seasonal_count_prediction_ACT <- calculate_records_seasonal(day_counts_ACT)
seasonal_count_prediction_NSW <- calculate_records_seasonal(day_counts_NSW)
seasonal_count_prediction_NT <- calculate_records_seasonal(day_counts_NT)
seasonal_count_prediction_QLD <- calculate_records_seasonal(day_counts_QLD)
seasonal_count_prediction_SA <- calculate_records_seasonal(day_counts_SA)
seasonal_count_prediction_TAS <- calculate_records_seasonal(day_counts_TAS)
seasonal_count_prediction_VIC <- calculate_records_seasonal(day_counts_VIC)
seasonal_count_prediction_WA <- calculate_records_seasonal(day_counts_WA)
# ______________________________________________________________________________


#---------------------------------------------------#

# merge prediction data frames
data_seasonal <- bind_rows(seasonal_count_prediction_ACT,
                   seasonal_count_prediction_NSW, 
                   seasonal_count_prediction_NT,
                   seasonal_count_prediction_QLD,
                   seasonal_count_prediction_SA,
                   seasonal_count_prediction_TAS,
                   seasonal_count_prediction_VIC,
                   seasonal_count_prediction_WA, .id = c("state")) %>%
  mutate(state = recode(state,
                        "1" = "ACT",
                        "2" = "NSW",
                        "3" = "NT",
                        "4" = "QLD",
                        "5" = "SA",
                        "6" = "TAS",
                        "7" = "VIC",
                        "8" = "WA"
  ))



plot_seasonal <- data_seasonal %>%
  ggplot(
    mapping = aes(x = julian_unscaled, y = fit_prop, fill = state, colour = state)) +
  geom_ribbon(aes(ymin = lci_prop, ymax = uci_prop), size = 1) +
  # geom_area(alpha = 0.5) +
  geom_path() +
  facet_wrap(~ state) +
  guides(fill = guide_legend(title = "State")) +
  theme_pilot(axes = "", 
              grid = "hv",
              legend_position = "none") +
  scale_fill_pilot() + 
  scale_color_pilot(breaks = "") +
  scale_x_continuous(limits = c(0, 366), breaks = c(0, 92, 183, 275), labels = c("Jan", "Apr", "Jul", "Oct")) +
  labs(x = "Month of Year", y = "Number of Records (Proportional)")  + 
  coord_polar() + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11),
        )

# plot_seasonal <- add_pilot_titles(
#   plot_seasonal,
#   title = "Number of Observation Records Added Daily \nto the Atlas of Living Australia",
#   subtitle = "Yearly Seasonal Trends 2010-2021",
#   title_color = "#E06E53")

plot_seasonal

# ggsave(here::here("projects", "data_holdings", "temporal", "plots", "2021-09_obs_seasonal_trends.png"),
#        width = 10, height = 10, units = "in", dpi = 600)

#---------------------------------------------------#

# Add the above plots to a map for ultimate awesomeness
?pilot_color()
library(ozmaps)
library(patchwork)

# Make the above plot into a single plot function
plot_seasonal_by_state <- function(state_name, state_colour) {
  plot <- data_seasonal %>%
    filter(state == as.character(state_name)) %>%
    ggplot(
      mapping = aes(x = julian_unscaled, y = fit_prop)
    ) +
    geom_ribbon(aes(ymin = lci_prop, ymax = uci_prop),
      fill = state_colour,
      colour = state_colour,
      size = 1
    ) +
    # geom_area(alpha = 0.5) +
    geom_path(colour = state_colour) +
    # facet_wrap(~ state) +
    guides(fill = guide_legend(title = "State")) +
    theme_pilot(
      axes = "",
      grid = "hv",
      legend_position = "none"
    ) +
    # scale_fill_pilot() +
    # scale_color_pilot(breaks = "") +
    scale_x_continuous(
      limits = c(0, 366),
      breaks = c(0, 92, 183, 275),
      labels = c("Jan", "Apr", "Jul", "Oct")
    ) +
    ggtitle(as.character(state_name)) +
    labs(x = "", y = "") +
    coord_polar() +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 9, hjust = 5),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      axis.title = element_text(colour = state_colour, hjust = 0.5)
    )
  
  return(plot)
}

s_plot_ACT <- plot_seasonal_by_state("ACT", pilot_color("navy"))
s_plot_NSW <- plot_seasonal_by_state("NSW", pilot_color("blue"))
s_plot_VIC <- plot_seasonal_by_state("VIC", pilot_color("purple"))
s_plot_QLD <- plot_seasonal_by_state("QLD", "#6A9057")
s_plot_SA <- plot_seasonal_by_state("SA", pilot_color("green"))
s_plot_WA <- plot_seasonal_by_state("WA", pilot_color("orange"))
s_plot_TAS <- plot_seasonal_by_state("TAS", pilot_color("yellow"))
s_plot_NT <- plot_seasonal_by_state("NT", pilot_color("brown"))

# ggsave(here::here("projects", "data_holdings", "temporal", 
#                   "plots", "state-plots", "2021-11_NT_season.png"),
#        width = 3.5, height = 3.5, units = "in", dpi = 600)


map <- ggplot(data = ozmap_states) + 
  geom_sf(aes(fill = NAME)) + 
  scale_fill_manual(values = c(
    "New South Wales" = pilot::pilot_color("blue"),
    "Victoria" = pilot_color("purple"),
    "Queensland"= "#6A9057",
    "South Australia" = pilot_color("green"),
    "Western Australia" = pilot_color("orange"),
    "Tasmania" = pilot_color("yellow"),
    "Northern Territory" = pilot_color("brown"),
    "Australian Capital Territory" = pilot_color("navy"),
    "Other Territories" = "white"
  )) +
  theme_void() +
  theme(legend.position = "none")

# ggsave(here::here("projects", "data_holdings", "temporal",
#                   "plots", "state-plots", "2021-11_map.png"),
#        width = 12, height = 12, units = "in", dpi = 600)




#---------------------------------------------------#
# finally calculate residuals from the model
date_df$residuals <- resid(model)

c <- ggplot(data_seasonal, aes(x = date, y = residuals)) +
  geom_path() +
  # geom_point() +
  theme_bw()

c
