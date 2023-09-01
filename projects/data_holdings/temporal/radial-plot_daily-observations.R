library(galah)
library(tidyverse)
library(purrr)
library(lubridate)

# Download records

galah_config(email = "dax.kellie@csiro.au")

occs <- galah_call() |>
  galah_identify("perameles") |>
  galah_filter(year >= 2019) |>
  atlas_occurrences()

occs <- occs |>
  mutate(
    raw_date = ymd_hms(eventDate),
    eventDate = as_date(raw_date)
  )


# time interval of interest
day_interval <- tibble(eventDate = seq(ymd("2019-01-01"), ymd("2022-12-31"), by = "days"))

# set up date dataset
date_df <- tibble(
  date = day_interval$eventDate,
  date_numeric = as.numeric(day_interval$eventDate),
  date_julian = yday(day_interval$eventDate)
)

# count number of observations each day
counts <- occs %>% 
  select(eventDate) |>
  group_by(eventDate) |>
  count() 

# merge to our column of all days, convert NAs to 0s
date_df <- date_df |>
  left_join(counts, by = c("date" = "eventDate")) |>
  replace_na(list(n = 0))


# ---------------------#
# ---- Model ----
# ---------------------#

library(mgcv)
  
# scale date for predictions
date_df$date_scaled <- scale(date_df$date_numeric)
date_df$julian_scaled <- scale(date_df$date_julian)

# Run a GAM model
model <- gam(unlist(n) ~ s(date_scaled, bs = "cc") + s(julian_scaled, bs = "cc"),
  data = date_df,
  family = poisson(link = "log")
)

# make prediction dataframe
prediction_2 <- data.frame(
  julian_scaled = seq(
    min(date_df$julian_scaled),
    max(date_df$julian_scaled),
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
    fit_prop = fit / sum(fit),
    lci = exp(model_prediction$fit - (2 * model_prediction$se.fit)),
    uci = exp(model_prediction$fit + (2 * model_prediction$se.fit)),
    lci_prop = lci / sum(lci),
    uci_prop = uci / sum(uci),
    julian_unscaled = seq(min(date_df$date_julian), max(date_df$date_julian), length.out = 100), # readjusting scale for plotting
    fit_total = sum(fit),
    fit_percent = fit / fit_total * 100,
    lci_percent = lci / fit_total * 100,
    uci_percent = uci / fit_total * 100
  )



# ---- PLOT ---- #

library(pilot)

prediction_2 %>%
  ggplot(mapping = aes(x = julian_unscaled, y = fit_prop)) +
  geom_ribbon(aes(ymin = lci_prop, ymax = uci_prop), size = 1, colour = "blue") +
  geom_area(alpha = 0.5, fill = "blue") +
  geom_path() +
  theme_pilot(axes = "", 
              grid = "hv",
              legend_position = "none") +
  scale_fill_pilot() +
  scale_color_pilot(breaks = "") +
  scale_x_continuous(limits = c(1, 366), breaks = c(1, 92, 183, 275), labels = c("Jan", "Apr", "Jul", "Oct")) +
  labs(x = "Month of Year", y = "Number of Records (Proportional)")  +
  coord_polar() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 16)
  )
