## ------------------------------------------------------#
## Title: Plant records added daily to the ALA from 2010 to 2020 by state
## Author: Dax Kellie
## Date Created: 2022-03-16
## ------------------------------------------------------#


#-----------------------------------------------------------------#
# Temporal spline models of plants added daily by state#
#-----------------------------------------------------------------#


library(galah)
library(tidyverse)
library(lubridate)
library(dplyr)
library(purrr)
library(here)


search_fields("Australian states")

# Expected numbers of records of plantae by state
galah_call() |>
  galah_identify("Plantae", "Chlorophyta") |>
  galah_filter(year > 2010) |>
  # galah_group_by(phylum) |>
  atlas_counts()
  
galah_call() |> # missing chlorophyta phylum
  galah_identify("Chlorophyta") |>
  atlas_species()



# Download records
# galah_config(email = "your-email@email.com")

occ <- galah_call() |>
  galah_identify("Plantae", "Chlorophyta") |>
  galah_filter(year > 2010) |>
  galah_select(eventDate, cl22) |>
  atlas_occurrences()

# Ask Dax about saved data to avoid repeatedly downloading (data could not be uploaded to GitHub due to size)

# Wrangle
occ <- occ %>% 
  select(eventDate, cl22) %>% # choose columns
  mutate(
    eventDate = ymd(as.Date(eventDate)), # convert to date
    state = cl22
  ) %>% 
  filter(state == "Australian Capital Territory" | # select relevant states
           state == "New South Wales" | 
           state == "Northern Territory" | 
           state == "Queensland" | 
           state == "South Australia" | 
           state == "Tasmania" | 
           state == "Victoria" | 
           state == "Western Australia") %>%
  mutate(
    state = recode(state, 
         "Australian Capital Territory" = "ACT",
         "New South Wales" = "NSW",
         "Northern Territory" = "NT",
         "Queensland" = "QLD",
         "South Australia" = "SA",
         "Tasmania" = "TAS",
         "Victoria" = "VIC",
         "Western Australia" = "WA")
    ) %>%
  drop_na(eventDate) # drop NAs

# Nest states into lists
occ_nested <- tidyr::nest(occ, y = c(eventDate))
names(occ_nested$y) <- as.character(occ_nested$state) # add state names to lists



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


#-------------- Plot GAMs --------------#
library(patchwork)
library(pilot)
library(geomtextpath)

# Fonts
library(extrafont)
library(showtext)
extrafont::loadfonts()
font_add_google(name = "Lato")

showtext_auto()

# Set font for {pilot} titles
pilot::set_pilot_family("Lato", title_family = "Lato") # Set font

# Draw plot
plot_ten_year_trend <- ggplot(
  data = data_daily,
  mapping = aes(x = date_unscaled, y = fit, fill = state, colour = state, label = state)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), size = 1) +
  geom_path() +
  guides(fill = guide_legend(title = "State")) +
  theme_pilot(axes = c("bl"),
              grid = "") +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_pilot(breaks = c("NSW", "QLD", "SA", "NT", "VIC", "WA", "TAS", "ACT")) + 
  scale_color_pilot(breaks = "") +
  ylim(c(0, 600)) +
  # geom_textline(size = 6, vjust = -0.2, spacing = 50, hjust = "ymax", text_smoothing = 30, fontface = 3) +
  labs(x = "Year", y = "Number of Records") + 
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 15), #change legend title font size
        legend.text = element_text(size=12))

# Add titles
plot_ten_year_trend <- add_pilot_titles(
  plot_ten_year_trend,
  title_size = 40,
  subtitle_size = 30,
  title = "Number of Observation Records Added Daily \nto the Atlas of Living Australia",
  subtitle = "10-year Trend (2010-2021)")

plot_ten_year_trend


ggsave(here::here("projects", "data_holdings_plants", "plots", "2022-03_obs_ten_year_trends.png"),
       dpi = 150)
