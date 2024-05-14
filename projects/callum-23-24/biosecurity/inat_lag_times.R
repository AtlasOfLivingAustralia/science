{
  library(galah)
  library(readr)
  library(lubridate)
  library(tidyverse)
}

galah_config(
  email = Sys.getenv("ALA_EMAIL"),
  run_checks = FALSE,
  verbose = TRUE)

upload_lag <- galah_call() |>
  galah_filter(dataResourceName == "iNaturalist Australia",
               eventDate >= "2023-01-01T00:00:00Z") |>
  select(eventDate, firstLoadedDate) |>
  atlas_occurrences()

upload_lag |>
  mutate(lag = difftime(firstLoadedDate, eventDate, units = "days") |> round()) |>
  count(lag) |>
  ggplot() +
  geom_bar(aes(x = lag, y = n), stat = "identity", fill = "purple3") +
  scale_x_continuous() +
  theme_bw()
