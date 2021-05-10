# temporal analysis code

library(lubridate)
library(galah)

# set time interval of interest
day_interval <- seq(ymd("2012-01-01"), ymd("2020-12-31"), by = "days")

# try building a lookup table for 4-hour blocks between two dates
# hour_list <- lapply(day_interval, function(a){
#   data.frame(
#     linear_date = as.numeric(a),
#     julian_date = yday(a),
#     hour_start = seq(0, 20, 4),
#     hour_end = seq(4, 24, 4)
#   )
# })
# hour_df <- as.data.frame(do.call(rbind, hour_list))

# get some records
ala_config(email = "martinjwestgate@gmail.com")

records <- ala_occurrences(
  taxa = select_taxa("Eolophus roseicapilla"),
  filters = select_filters(cl22 = "Tasmania"),
  columns = select_columns("verbatimEventDate", group = "basic"))
# str(records)
# unique(records$verbatimEventDate)
## shows that minute-specific records are rare
## ignore hourly stuff

# set dates to date format
records$eventDate <- ymd(records$eventDate)

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
    return(length(which(records$eventDate == a)))
}))


# model using GAMs
# install.packages("mgcv")
library(mgcv)
date_df$date_scaled <- scale(date_df$date_numeric)
date_df$julian_scaled <- scale(date_df$date_julian)

model <- gam(record_count ~ s(date_scaled) + s(julian_scaled),
  data = date_df,
  family = poisson(link = "log"))

# make predictions

# first of change over time
prediction_1 <- data.frame(
  date_scaled = seq(
    min(date_df$date_scaled),
    max(date_df$date_scaled),
    length.out = 100),
  julian_scaled = 0)

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

model_prediction <- predict(model, newdata = prediction_2, se.fit = TRUE)
prediction_2$fit <- exp(model_prediction$fit)
prediction_2$lci <- exp(model_prediction$fit - (2 * model_prediction$se.fit))
prediction_2$uci <- exp(model_prediction$fit + (2 * model_prediction$se.fit))

# finally calculate residuals from the model
date_df$residuals <- resid(model)


# draw
library(ggplot2)
library(patchwork)

a <- ggplot(prediction_1, aes(x = date_scaled, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "#ff6e6e") +
  geom_path() +
  theme_bw()

b <- ggplot(prediction_2, aes(x = julian_scaled, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "#6e9eff") +
  geom_path() +
  theme_bw()

c <- ggplot(date_df, aes(x = date, y = residuals)) +
  geom_path() +
  # geom_point() +
  theme_bw()

a / b / c
ggsave("./plots/mgcv_example.pdf")
