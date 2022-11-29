#install packages
install.packages("galah")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("gridExtra")
install.packages("lubridate")
install.packages("tibble")
install.packages("scales")
#call packages
library(galah)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(lubridate)
library(tibble)
library(scales)

galah_config(email = "thai.rushbrook@gmail.com")

options(scipen = 999)

#----------All Atlas records Melbourne
Allrecords_melb <- as.data.frame(
  galah_call() |>                              
    galah_identify() |>                
    galah_filter(cl10929 == "GREATER MELBOURNE", year >= 2017, year <= 2021) |>  
    galah_group_by(year) |>
    atlas_counts())
Allrecords_melb$year = as.numeric(Allrecords_melb$year)
Allrecords_melb <- Allrecords_melb[order(-Allrecords_melb$year), , drop = TRUE]
rownames(Allrecords_melb) <- c(1:5) #Rows 3 and 4 keep swapping despite drop TRUE

#----------Lockdown Dates
Lockdown <- c(1:6)
Start <- c("2020-03-31", "2020-07-09", "2021-02-13", "2021-05-28", "2021-07-16", "2021-08-05")
End <- c("2020-05-12", "2020-10-27", "2021-02-17", "2021-06-10", "2021-07-27", "2021-10-21")

Melb_dates <- data.frame(Lockdown, Start, End)
Melb_dates$Num_Days <- as.Date(as.character(Melb_dates$End), format="%Y-%m-%d")-
  as.Date(as.character(Melb_dates$Start), format="%Y-%m-%d")

Melb_dates <- Melb_dates %>% 
  mutate(weekstart = week(Start)) %>%
  mutate(weekend = week(End))

#----------Melbourne duck counts 2017-2021 (by day)
Ducks_day.data <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2017-01-01T00:00:00Z", eventDate <= "2021-12-31T23:59:00Z") |>
    galah_select(eventDate) |> 
    atlas_occurrences())
Ducks_day.data$eventDate <- as.Date(as.character(Ducks_day.data$eventDate), format="%Y-%m-%d")
Ducks_day.data <- Ducks_day.data %>% 
  count(eventDate)
Ducks_day.data <- Ducks_day.data %>% 
  mutate(year = year(eventDate)) %>%
  mutate(daymonth = (paste(day(eventDate), month(eventDate), sep = "-")))
Ducks_day.data$daymonth = as.Date(as.character(Ducks_day.data$daymonth), format = "%d-%m")
colnames(Ducks_day.data) = c("date","count", "year", "daymonth")

#----------Scale for total counts
Ducks_day.scaled <- Ducks_day.data |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)

Ducks_day.scaled$"2017" = (Ducks_day.scaled$"2017"/Allrecords_melb[5,2])*100
Ducks_day.scaled$"2018" = (Ducks_day.scaled$"2018"/Allrecords_melb[4,2])*100
Ducks_day.scaled$"2019" = (Ducks_day.scaled$"2019"/Allrecords_melb[3,2])*100
Ducks_day.scaled$"2020" = (Ducks_day.scaled$"2020"/Allrecords_melb[2,2])*100
Ducks_day.scaled$"2021" = (Ducks_day.scaled$"2021"/Allrecords_melb[1,2])*100

Ducks_day.long <- Ducks_day.scaled |>
  pivot_longer(cols=c("2017", "2018", "2019", "2020", "2021"), names_to = "year", values_to = "count")
Ducks_day.long <- na.omit(Ducks_day.long)

#----------Calculate means
Ducks_day.means <- Ducks_day.long[,-1] |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)

Ducks_day.means$"2017_19_mean" = rowMeans(Ducks_day.means[,2:4])

#----------Plot
Ducks_day.plot <- Ducks_day.means[,-c(2:4)]
Ducks_day.plot20 <- Ducks_day.plot[,-3]
Ducks_day.plot21 <- Ducks_day.plot[,-2]

year(Ducks_day.plot20$daymonth) = 2020
  colnames(Ducks_day.plot20)[2] = "count"
year(Ducks_day.plot21$daymonth) = 2021
  colnames(Ducks_day.plot21)[2] = "count"
Ducks_day.plot <- rbind(Ducks_day.plot20, Ducks_day.plot21)

cols <- c("count" = "red", "2017_19_mean" = "blue")

Ducks_day.viz <- ggplot() +
  geom_line(data = Ducks_day.plot, aes(x = daymonth, y = count, color = "count")) +
  geom_line(data = Ducks_day.plot, aes(x = daymonth, y =  Ducks_day.plot$"2017_19_mean", color = "2017_19_mean"),linetype = "twodash") +
  scale_color_manual(values=cols) +
  geom_rect(aes(NULL,NULL,xmin=as.Date("2020-03-31", "%Y-%m-%d"),xmax=as.Date("2020-05-12", "%Y-%m-%d"),fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=as.Date("2020-07-09", "%Y-%m-%d"),xmax=as.Date("2020-10-27", "%Y-%m-%d"),fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=as.Date("2021-02-13", "%Y-%m-%d"),xmax=as.Date("2021-02-17", "%Y-%m-%d"),fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=as.Date("2021-05-28", "%Y-%m-%d"),xmax=as.Date("2021-06-10", "%Y-%m-%d"),fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=as.Date("2021-07-16", "%Y-%m-%d"),xmax=as.Date("2021-07-27", "%Y-%m-%d"),fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=as.Date("2021-08-05", "%Y-%m-%d"),xmax=as.Date("2021-10-21", "%Y-%m-%d"),fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  scale_fill_manual(values=c("Lockdown" = "yellow")) +
  labs(title="Records of ducks in Greater Melbourne (with Covid-19 lockdowns highlighted)",x="Date", y = "No. of records (scaled)") + 
  theme(plot.title=element_text(size=10)) +
  xlim(as.Date("2020-01-01", "%Y-%m-%d"),as.Date("2021-12-31", "%Y-%m-%d"))

#----------Melbourne duck counts 2017-2021 (by week)
Ducks_week.data <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2017-01-01T00:00:00Z", eventDate <= "2021-12-31T23:59:00Z") |>
    galah_select(eventDate) |> 
    atlas_occurrences())
Ducks_week.data$eventDate <- as.Date(as.character(Ducks_week.data$eventDate), format="%Y-%m-%d")
Ducks_week.data <- Ducks_week.data %>% 
  count(eventDate)
Ducks_week.data <- Ducks_week.data %>% 
  mutate(year = year(eventDate)) %>%
  mutate(week = week(eventDate))
colnames(Ducks_week.data) = c("date","count", "year", "week")

#----------Scale for total counts
Ducks_week.scaled <- Ducks_week.data |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)

Ducks_week.scaled$"2017" = (Ducks_week.scaled$"2017"/Allrecords_melb[5,2])*100
Ducks_week.scaled$"2018" = (Ducks_week.scaled$"2018"/Allrecords_melb[4,2])*100
Ducks_week.scaled$"2019" = (Ducks_week.scaled$"2019"/Allrecords_melb[3,2])*100
Ducks_week.scaled$"2020" = (Ducks_week.scaled$"2020"/Allrecords_melb[2,2])*100
Ducks_week.scaled$"2021" = (Ducks_week.scaled$"2021"/Allrecords_melb[1,2])*100

Ducks_week.long <- Ducks_week.scaled |>
  pivot_longer(cols=c("2017", "2018", "2019", "2020", "2021"), names_to = "year", values_to = "count")
Ducks_week.long <- na.omit(Ducks_week.long)

#----------Combine weeks
Ducks_week.long <- data.frame(Ducks_week.long %>%
   group_by(week,year) %>%
   summarise(across(c(count), sum)))

#----------Calculate means
Ducks_week.means <- Ducks_week.long |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)

Ducks_week.means$"2017_19_mean" = rowMeans(Ducks_week.means[,2:4])

#----------Plot
Ducks_week.plot <- Ducks_week.means[,-c(2:4)] |>
  pivot_longer(cols=c("2020", "2021", "2017_19_mean"), names_to = "year", values_to = "count")

Ducks_week.plot <- Ducks_week.means[,-c(2:4)]
Ducks_week.plot20 <- Ducks_week.plot[,-3]
Ducks_week.plot21 <- Ducks_week.plot[,-2]

Ducks_week.plot21 <- as.data.frame(Ducks_week.plot21)
Ducks_week.plot21$week <- c(54:106)
rownames(Ducks_week.plot21) <- c(54:106)
colnames(Ducks_week.plot20)[2] = "count"
colnames(Ducks_week.plot21)[2] = "count"

Ducks_week.plot <- rbind(Ducks_week.plot20, Ducks_week.plot21)

#Ggplot2
Ducks_week.viz <- ggplot() +
  geom_line(data = Ducks_week.plot, aes(x = week, y = count, color = "count")) +
  geom_line(data = Ducks_week.plot, aes(x = week, y =  Ducks_week.plot$"2017_19_mean", color = "2017_19_mean"),linetype = "twodash") +
  scale_color_manual(values=cols) +
  geom_rect(aes(NULL,NULL,xmin=13,xmax=19,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=28,xmax=43,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=60,xmax=60,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=77,xmax=78,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=82,xmax=83,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=84,xmax=95,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  scale_fill_manual(values=c("Lockdown" = "yellow")) +
  labs(title="Records of ducks in Greater Melbourne (with Covid-19 lockdowns highlighted)",x="Week", y = "No. of records (scaled)") + 
  theme(plot.title=element_text(size=10))

#Ducks records by year

Allducks_Melb.data <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", year >= 2017, year <= 2021) |> 
    galah_group_by(year) |>
    atlas_counts())
Allducks_Melb.data$year = as.numeric(Allducks_Melb.data$year)
Allducks_Melb.data <- Allducks_Melb.data[order(-Allducks_Melb.data$year), , drop = TRUE]
rownames(Allducks_Melb.data) <- c(1:5) 

#Scale
Allducks_Melb.scaled <- Allducks_Melb.data

Allducks_Melb.scaled[5,2] = (Allducks_Melb.scaled[5,2]/Allrecords_melb[5,2])*100
Allducks_Melb.scaled[4,2] = (Allducks_Melb.scaled[4,2]/Allrecords_melb[4,2])*100
Allducks_Melb.scaled[3,2] = (Allducks_Melb.scaled[3,2]/Allrecords_melb[3,2])*100
Allducks_Melb.scaled[2,2] = (Allducks_Melb.scaled[2,2]/Allrecords_melb[2,2])*100
Allducks_Melb.scaled[1,2] = (Allducks_Melb.scaled[1,2]/Allrecords_melb[1,2])*100

  
