## ------------------------------------------------------#
## Title: Temporal spline models
## Author: Dax Kellie
## Date Created: 2021-05-26
## ------------------------------------------------------#


# NOTE: This code is not reproducible for others at the moment. Stay tuned!


rm(list = ls())

#-----------------------------------------------------#
#               Multiple state splines                #
#-----------------------------------------------------#


# Call saved local data files
library(tidyverse)
library(lubridate)

path1 <- "C:/Users/KEL329/OneDrive - CSIRO/Documents/ALA/Projects/Data Holdings/data_ALA/eventDate"
df <- list.files(path = path1, full.names = TRUE) %>%
  map_dfr(readRDS, .id = 'source')



#------------- Prepare data frame --------------#

# to get a list of what order the data sets were loaded into R, check out the files paths
list.files(path = path1)

# create column to identify states by name & select columns
df <- df %>%
  mutate(state = recode(source,
                        "1" = "NSW",
                        "2" = "QLD",
                        "3" = "VIC",
)) %>%
  select(state, eventDate)


# set dates to date format
df$eventDate <- ymd(df$eventDate)

# nest states into data frame
occ_nested <- df %>% 
  group_by(state) %>% 
  nest()
occ_nested

rm(df)



#--------------- Extract data -----------------#

# set time interval of interest
day_interval <- seq(ymd("2012-01-01"), ymd("2020-12-31"), by = "days")


# NOTE:
# On line 92, change number for event date and run to line 145
# Then save model prediction results, run plotting function, save plot



# set up date dataset
date_df <- data.frame(
  date = day_interval,
  date_numeric = as.numeric(day_interval),
  date_julian = yday(day_interval))

# states
# states <-  c("TAS", "WA")
# date_df_list <- states %>% map_df(., ~data.frame(
#     date = day_interval,
#     date_numeric = as.numeric(day_interval),
#     date_julian = yday(day_interval),
#     state = map_chr(., ~as.character(.x), id = "id"))) %>%
#   group_by(state) %>%
#   nest()
# date_df_list


# count records on each day
func <- function(eventDate, date_df_date){
  return(length(which(eventDate == date_df_date)))
}

#---------------------------------------------------------------------------------#
event_date <- occ_nested %>% pluck("data", 2) # change number for different states
#_________________________________________________________________________________

date_df <- date_df %>% mutate(
  record_count = pmap_dbl( list(event_date,day_interval), func))





#-------------- model using GAMs --------------#

library(mgcv)

# first of change over time

# scale date for predictions
date_df$date_scaled <- scale(date_df$date_numeric)
date_df$julian_scaled <- scale(date_df$date_julian)


# Run a GAM model
model <- gam(record_count ~ s(date_scaled) + s(julian_scaled),
             data = date_df,
             family = poisson(link = "log"))



# Prediction function 1
calc_prediction_1 <- function(df){
  
  # make prediction dataframe
  prediction_1 <- data.frame(
    date_scaled = seq(
      min(df$date_scaled),
      max(df$date_scaled),
      length.out = 100),
    julian_scaled = 0)
  
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
prediction_1_NSW <- calc_prediction_1(date_df)
prediction_1_QLD <- calc_prediction_1(date_df)
prediction_1_VIC <- calc_prediction_1(date_df)
#______________________________________________________________________________


# draw
library(patchwork)
library(animation)
# library(magick)




plotting_func <- function(df, colour, state_name){
  plot <- ggplot(data = df, 
                 mapping = aes(x = date_unscaled, y = fit)) +
    geom_ribbon(aes(ymin = lci, ymax = uci), fill = as.character(colour)) +
    geom_area(fill = as.character(colour), alpha = 0.2) +
    geom_path() +
    theme_bw() +
    ggtitle("Number of Observation Records Added \nto the Atlas of Living Australia from 2012-2021", 
            subtitle = as.character(state_name)) +
    ylim(c(0, 4250)) +
    labs(x = "Year", y = "Number of Records")
  return(plot)
}

# a1 <- plotting_func(prediction_1_ACT, "#ff6e6e", "ACT")
# a2 <- plotting_func(prediction_1_SA, "blue", "SA")
# a3 <- plotting_func(prediction_1_TAS, "green", "TAS")
# a4 <- plotting_func(prediction_1_VIC, "purple", "VIC")
# a5 <- plotting_func(prediction_1_WA, "orange", "WA")

a1 <- plotting_func(prediction_1_NSW, "light blue", "NSW")  # NSW
a2 <- plotting_func(prediction_1_QLD, "pink", "QLD")     # QLD
a3 <- plotting_func(prediction_1_VIC, "purple", "VIC")  # VIC



# make GIF
animation::saveGIF(
  expr = {
    plot(a1)
    plot(a2)
    plot(a3)
  },
  movie.name = "temporalA_NSW_QLD_VIC.gif"
)


#_____________________________________________________________



# then repeat for Julian Date

# Prediction function 2
calc_prediction_2 <- function(df){
  
  # make prediction dataframe
  prediction_2 <- data.frame(
    julian_scaled = seq(
      min(df$julian_scaled),
      max(df$julian_scaled),
      length.out = 100),
    date_scaled = 0)
  
  # predict
  model_prediction <- predict(model, newdata = prediction_2, se.fit = TRUE)
  
  # add fit & CIs to prediction dataframe
  prediction_2 <- prediction_2 %>% 
    mutate(
      fit = exp(model_prediction$fit),
      lci = exp(model_prediction$fit - (2 * model_prediction$se.fit)),
      uci = exp(model_prediction$fit + (2 * model_prediction$se.fit)),
      
      julian_unscaled = seq(min(df$date_julian), max(df$date_julian), length.out = 100) # readjusting scale for plotting
    )
}

#------------------------------------------------------------------------------#
# run prediction function for each state/territory
prediction_2_NSW <- calc_prediction_2(date_df)
prediction_2_QLD <- calc_prediction_2(date_df)
prediction_2_VIC <- calc_prediction_2(date_df)
#______________________________________________________________________________



# finally calculate residuals from the model
date_df$residuals <- resid(model)








#---------------------------------------------------#


plotting_func_2 <- function(df, colour, state_name){
  plot <- ggplot(data = df, 
                 mapping = aes(x = julian_unscaled, y = fit)) +
    geom_ribbon(aes(ymin = lci, ymax = uci), fill = as.character(colour)) +
    geom_area(fill = as.character(colour), alpha = 0.2) +
    geom_path() +
    theme_bw() +
    ggtitle("Number of Observation Records Added per Day \nto the Atlas of Living Australia from 2012-2021", 
            subtitle = as.character(state_name)) +
    # ylim(c(0, 4250)) +
    labs(x = "Day of Year", y = "Number of Records")
  return(plot)
}


b1 <- plotting_func_2(prediction_2_NSW, "light blue", "NSW")  # NSW
b2 <- plotting_func_2(prediction_2_QLD, "pink", "QLD")     # QLD
b3 <- plotting_func_2(prediction_2_VIC, "purple", "VIC")  # VIC


# make GIF
animation::saveGIF(
  expr = {
    plot(b1)
    plot(b2)
    plot(b3)
  },
  movie.name = "temporalB_NSW_QLD_VIC.gif"
)




#---------------------------------------------------#

a <- ggplot(prediction_1, aes(x = date_unscaled, y = fit)) +
  geom_area(aes(ymin = lci, ymax = uci), fill = "#ff6e6e", alpha = 0.2) +
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