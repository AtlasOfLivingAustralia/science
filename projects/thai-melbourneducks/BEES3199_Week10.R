#install packages
install.packages("galah")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("gridExtra")
install.packages("lubridate")
install.packages("tibble")
#call packages
library(galah)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(lubridate)
library(tibble)

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

#----------Melbourne duck counts 2017-2021 (by day)
Ducks_occ.day <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2017-01-01T00:00:00Z", eventDate <= "2021-12-31T23:59:00Z") |>
    galah_select(eventDate) |> 
    atlas_occurrences())
Ducks_occ.day$eventDate <- as.Date(as.character(Ducks_occ.day$eventDate), format="%Y-%m-%d")
Ducks_occ.day <- Ducks_occ.day %>% 
  count(eventDate)
Ducks_occ.day <- Ducks_occ.day %>% 
  mutate(year = year(eventDate)) %>%
  mutate(daymonth = (paste(day(eventDate), month(eventDate), sep = "-")))
Ducks_occ.day$daymonth = as.Date(as.character(Ducks_occ.day$daymonth), format = "%d-%m")
colnames(Ducks_occ.day) = c("date","count", "year", "daymonth")

#----------Scale for total counts
Ducks_occ.scaled.d <- Ducks_occ.day |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)

Ducks_occ.scaled.d$"2017" = (Ducks_occ.scaled.d$"2017"/Allrecords_melb[5,2])*100
Ducks_occ.scaled.d$"2018" = (Ducks_occ.scaled.d$"2018"/Allrecords_melb[4,2])*100
Ducks_occ.scaled.d$"2019" = (Ducks_occ.scaled.d$"2019"/Allrecords_melb[3,2])*100
Ducks_occ.scaled.d$"2020" = (Ducks_occ.scaled.d$"2020"/Allrecords_melb[2,2])*100
Ducks_occ.scaled.d$"2021" = (Ducks_occ.scaled.d$"2021"/Allrecords_melb[1,2])*100

Ducks_occlong.d <- Ducks_occ.scaled.d |>
  pivot_longer(cols=c("2017", "2018", "2019", "2020", "2021"), names_to = "year", values_to = "count")
Ducks_occlong.d <- na.omit(Ducks_occlong.d)

#----------Plot
#2020
Ducks_occ.plot20.d <- Ducks_occ.long.d[!(Ducks_occ.long.d$year == "2021"),]

Ducks_occ.chart20.d <- ggplot(Ducks_occ.plot20.d, aes(x=daymonth, y=count, group=year)) +
  geom_rect(aes(NULL,NULL,xmin=as.Date("2022-03-31", "%Y-%m-%d"),xmax=as.Date("2022-05-12", "%Y-%m-%d"),fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=as.Date("2022-07-09", "%Y-%m-%d"),xmax=as.Date("2022-10-27", "%Y-%m-%d"),fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  scale_fill_manual(values=c("Lockdown" = "yellow")) +
  geom_line(aes(color = year), linewidth = 1) +
  labs(title="Records of ducks in Greater Melbourne (with 2020 lockdowns highlighted)",x="Month", y = "No. of records") + 
  theme(plot.title=element_text(size=10)) +
  scale_color_manual(values=c("red","orange","green", "blue", "purple"))

#----------Melbourne duck counts 2017-2021 (by week)
Ducks_occ.week <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2017-01-01T00:00:00Z", eventDate <= "2021-12-31T23:59:00Z") |>
    galah_select(eventDate) |> 
    atlas_occurrences())
Ducks_occ.week$eventDate <- as.Date(as.character(Ducks_occ.week$eventDate), format="%Y-%m-%d")
Ducks_occ.week <- Ducks_occ.week %>% 
  count(eventDate)
Ducks_occ.week <- Ducks_occ.week %>% 
  mutate(year = year(eventDate)) %>%
  mutate(week = (paste(week(eventDate), year(eventDate), sep = "-")))
Ducks_occ.week$week = as.Date(as.character(Ducks_occ.week$week), format = "%U-%Y")
colnames(Ducks_occ.week) = c("date","count", "year", "week")

class(Ducks_occ.week$month)

#----------Scale for total counts
Ducks_occ.scaled.w <- Ducks_occ.week |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)

Ducks_occ.scaled.w$"2017" = (Ducks_occ.scaled.w$"2017"/Allrecords_melb[5,2])*100
Ducks_occ.scaled.w$"2018" = (Ducks_occ.scaled.w$"2018"/Allrecords_melb[4,2])*100
Ducks_occ.scaled.w$"2019" = (Ducks_occ.scaled.w$"2019"/Allrecords_melb[3,2])*100
Ducks_occ.scaled.w$"2020" = (Ducks_occ.scaled.w$"2020"/Allrecords_melb[2,2])*100
Ducks_occ.scaled.w$"2021" = (Ducks_occ.scaled.w$"2021"/Allrecords_melb[1,2])*100

Ducks_occlong.w <- Ducks_occ.scaled.w |>
  pivot_longer(cols=c("2017", "2018", "2019", "2020", "2021"), names_to = "year", values_to = "count")
Ducks_occlong.w <- na.omit(Ducks_occlong.w)

#----------Plot
#2020
Ducks_occ.plot20.w <- Ducks_occ.long.w[!(Ducks_occ.long.w$year == "2021"),]

Ducks_occ.chart20 <- ggplot(Ducks_occ.plot20, aes(x=daymonth, y=count, group=year)) +
  geom_rect(aes(NULL,NULL,xmin=as.Date("2022-03-31", "%Y-%m-%d"),xmax=as.Date("2022-05-12", "%Y-%m-%d"),fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=as.Date("2022-07-09", "%Y-%m-%d"),xmax=as.Date("2022-10-27", "%Y-%m-%d"),fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", linewidth=0.5, alpha=0.2) +
  scale_fill_manual(values=c("Lockdown" = "yellow")) +
  geom_line(aes(color = year), linewidth = 1) +
  labs(title="Records of ducks in Greater Melbourne (with 2020 lockdowns highlighted)",x="Month", y = "No. of records") + 
  theme(plot.title=element_text(size=10)) +
  scale_color_manual(values=c("red","orange","green", "blue", "purple"))




#2021
Ducks_occ.plot21 <- Ducks_occ.long[!(Ducks_occ.long$year == "2020"),]

Waterbirds_melb2021.plot <- ggplot(Anatidae_bymonth.scaled21, aes(x=month, y=count, group=year)) +
  geom_rect(aes(NULL,NULL,xmin=2.464,xmax=2.607,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", size=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=5.903,xmax=6.333,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", size=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=7.516,xmax=7.871,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", size=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=8.167,xmax=10.677,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", size=0.5, alpha=0.2) +
  scale_fill_manual(values=c("Lockdown" = "yellow")) +
  geom_line(aes(color = year), size = 1) +
  labs(title="Records of waterbirds in Greater Melbourne (with 2021 lockdowns highlighted)",x="Month", y = "No. of records") + 
  theme(plot.title=element_text(size=10)) +
  scale_color_manual(values=c("red","orange","green", "blue", "purple"))



