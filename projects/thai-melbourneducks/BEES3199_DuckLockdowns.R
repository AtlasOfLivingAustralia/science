#Melbourne duck counts through each lockdown

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
Allrecords_melb <- Allrecords_melb[order(-Allrecords_melb$year), , drop = FALSE]
rownames(Allrecords_melb) <- c(1:5) #Rows 3 and 4 keep swapping despite drop FALSE

#----------Lockdown Dates
Lockdown <- c(1:6)
Start <- c("2020-03-31", "2020-07-09", "2021-02-13", "2021-05-28", "2021-07-16", "2021-08-05")
End <- c("2020-05-12", "2020-10-27", "2021-02-17", "2021-06-10", "2021-07-27", "2021-10-21")

Melb_dates <- data.frame(Lockdown, Start, End)
Melb_dates$Num_Days <- as.Date(as.character(Melb_dates$End), format="%Y-%m-%d")-
  as.Date(as.character(Melb_dates$Start), format="%Y-%m-%d")

#----------How did the lockdowns impact duck counts in Melbourne?

#----------Melbourne Lockdown 1 duck counts
#2020
Ducks_1.data20 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2020-03-31T00:00:00Z", eventDate <= "2020-05-12T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_1.data20$year = "2020"
Ducks_1.data20$lockdown = "1"
Ducks_1.data20 <- Ducks_1.data20[,-c(1)]
#2019
Ducks_1.data19 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2019-03-31T00:00:00Z", eventDate <= "2019-05-12T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_1.data19$year = "2019"
Ducks_1.data19$lockdown = "1"
Ducks_1.data19 <- Ducks_1.data19[,-c(1)]
#2018
Ducks_1.data18 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2018-03-31T00:00:00Z", eventDate <= "2018-05-12T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_1.data18$year = "2018"
Ducks_1.data18$lockdown = "1"
Ducks_1.data18 <- Ducks_1.data18[,-c(1)]
#2017
Ducks_1.data17 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2017-03-31T00:00:00Z", eventDate <= "2017-05-12T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_1.data17$year = "2017"
Ducks_1.data17$lockdown = "1"
Ducks_1.data17 <- Ducks_1.data17[,-c(1)]

#----------Melbourne Lockdown 2 duck counts
#2020
Ducks_2.data20 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2020-07-09T00:00:00Z", eventDate <= "2020-10-27T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_2.data20$year = "2020"
Ducks_2.data20$lockdown = "2"
Ducks_2.data20 <- Ducks_2.data20[,-c(1)]
#2019
Ducks_2.data19 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2019-07-09T00:00:00Z", eventDate <= "2019-10-27T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_2.data19$year = "2019"
Ducks_2.data19$lockdown = "2"
Ducks_2.data19 <- Ducks_2.data19[,-c(1)]
#2018
Ducks_2.data18 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2018-07-09T00:00:00Z", eventDate <= "2018-10-27T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_2.data18$year = "2018"
Ducks_2.data18$lockdown = "2"
Ducks_2.data18 <- Ducks_2.data18[,-c(1)]
#2017
Ducks_2.data17 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2017-07-09T00:00:00Z", eventDate <= "2017-10-27T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_2.data17$year = "2017"
Ducks_2.data17$lockdown = "2"
Ducks_2.data17 <- Ducks_2.data17[,-c(1)]

#----------Melbourne Lockdown 3 species counts
#2021
Ducks_3.data21 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2021-02-13T00:00:00Z", eventDate <= "2021-02-17T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_3.data21$year = "2021"
Ducks_3.data21$lockdown = "3"
Ducks_3.data21 <- Ducks_3.data21[,-c(1)]
#2019
Ducks_3.data19 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2019-02-13T00:00:00Z", eventDate <= "2019-02-17T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_3.data19$year = "2019"
Ducks_3.data19$lockdown = "3"
Ducks_3.data19 <- Ducks_3.data19[,-c(1)]
#2018
Ducks_3.data18 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2018-02-13T00:00:00Z", eventDate <= "2018-02-17T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_3.data18$year = "2018"
Ducks_3.data18$lockdown = "3"
Ducks_3.data18 <- Ducks_3.data18[,-c(1)]
#2017
Ducks_3.data17 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2017-02-13T00:00:00Z", eventDate <= "2017-02-17T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_3.data17$year = "2017"
Ducks_3.data17$lockdown = "3"
Ducks_3.data17 <- Ducks_3.data17[,-c(1)]

#----------Melbourne Lockdown 4 species counts
#2021
Ducks_4.data21 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2021-05-28T00:00:00Z", eventDate <= "2021-06-10T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_4.data21$year = "2021"
Ducks_4.data21$lockdown = "4"
Ducks_4.data21 <- Ducks_4.data21[,-c(1)]
#2019
Ducks_4.data19 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2019-05-28T00:00:00Z", eventDate <= "2019-06-10T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_4.data19$year = "2019"
Ducks_4.data19$lockdown = "4"
Ducks_4.data19 <- Ducks_4.data19[,-c(1)]
#2018
Ducks_4.data18 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2018-05-28T00:00:00Z", eventDate <= "2018-06-10T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_4.data18$year = "2018"
Ducks_4.data18$lockdown = "4"
Ducks_4.data18 <- Ducks_4.data18[,-c(1)]
#2017
Ducks_4.data17 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2017-05-28T00:00:00Z", eventDate <= "2017-06-10T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts())
Ducks_4.data17$year = "2017"
Ducks_4.data17$lockdown = "4"
Ducks_4.data17 <- Ducks_4.data17[,-c(1)]

#----------Melbourne Lockdown 5 species counts
#2021
Ducks_5.data21 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2021-07-16T00:00:00Z", eventDate <= "2021-07-27T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_5.data21$year = "2021"
Ducks_5.data21$lockdown = "5"
Ducks_5.data21 <- Ducks_5.data21[,-c(1)]
#2019
Ducks_5.data19 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2019-07-16T00:00:00Z", eventDate <= "2019-07-27T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_5.data19$year = "2019"
Ducks_5.data19$lockdown = "5"
Ducks_5.data19 <- Ducks_5.data19[,-c(1)]
#2018
Ducks_5.data18 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2018-07-16T00:00:00Z", eventDate <= "2018-07-27T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_5.data18$year = "2018"
Ducks_5.data18$lockdown = "5"
Ducks_5.data18 <- Ducks_5.data18[,-c(1)]
#2017
Ducks_5.data17 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2017-07-16T00:00:00Z", eventDate <= "2017-07-27T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_5.data17$year = "2017"
Ducks_5.data17$lockdown = "5"
Ducks_5.data17 <- Ducks_5.data17[,-c(1)]

#----------Melbourne Lockdown 6 species counts
#2021
Ducks_6.data21 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2021-08-05T00:00:00Z", eventDate <= "2021-10-21T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_6.data21$year = "2021"
Ducks_6.data21$lockdown = "6"
Ducks_6.data21 <- Ducks_6.data21[,-c(1)]
#2019
Ducks_6.data19 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2019-08-05T00:00:00Z", eventDate <= "2019-10-21T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_6.data19$year = "2019"
Ducks_6.data19$lockdown = "6"
Ducks_6.data19 <- Ducks_6.data19[,-c(1)]
#2018
Ducks_6.data18 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2018-08-05T00:00:00Z", eventDate <= "2018-10-21T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_6.data18$year = "2018"
Ducks_6.data18$lockdown = "6"
Ducks_6.data18 <- Ducks_6.data18[,-c(1)]
#2017
Ducks_6.data17 <- data.frame(
  galah_call() |>                               
    galah_identify("Anatidae")|>                 
    galah_filter(cl10929 == "GREATER MELBOURNE", eventDate >= "2017-08-05T00:00:00Z", eventDate <= "2017-10-21T14:00:00Z") |>
    galah_group_by(family) |>  
    atlas_counts()) 
Ducks_6.data17$year = "2017"
Ducks_6.data17$lockdown = "6"
Ducks_6.data17 <- Ducks_6.data17[,-c(1)]

#----------Create data frame

Duck_lockdown.data <- rbind(Ducks_1.data20, Ducks_1.data19, Ducks_1.data18, Ducks_1.data17, Ducks_2.data20, Ducks_2.data19, Ducks_2.data18, Ducks_2.data17, Ducks_3.data21, Ducks_3.data19, Ducks_3.data18, Ducks_3.data17, Ducks_4.data21, Ducks_4.data19, Ducks_4.data18, Ducks_4.data17, Ducks_5.data21, Ducks_5.data19, Ducks_5.data18, Ducks_5.data17, Ducks_6.data21, Ducks_6.data19, Ducks_6.data18, Ducks_6.data17)

Duck_lockdown.pivot <- Duck_lockdown.data |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)

Duck_lockdown.scaled <- Duck_lockdown.pivot
Duck_lockdown.scaled$"2017" = (Duck_lockdown.scaled$"2017"/as.numeric(Allrecords_melb[5,2]))*100
Duck_lockdown.scaled$"2018" = (Duck_lockdown.scaled$"2018"/as.numeric(Allrecords_melb[4,2]))*100
Duck_lockdown.scaled$"2019" = (Duck_lockdown.scaled$"2019"/as.numeric(Allrecords_melb[3,2]))*100
Duck_lockdown.scaled$"2020" = (Duck_lockdown.scaled$"2020"/as.numeric(Allrecords_melb[2,2]))*100
Duck_lockdown.scaled$"2021" = (Duck_lockdown.scaled$"2021"/as.numeric(Allrecords_melb[1,2]))*100

#Add means
Duck_lockdown.means <- Duck_lockdown.scaled

Duck_lockdown.means$"2017_19_mean" = rowMeans(Duck_lockdown.means[,2:4])

#Manipulate for plotting
Duck_lockdown.tempmeans <- Duck_lockdown.means[,c(1,7)]
colnames(Duck_lockdown.tempmeans) = c("lockdown", "count")
Duck_lockdown.tempmeans$mean_actual = "mean"
Duck_lockdown.temp2020 <- Duck_lockdown.scaled[1:2, c(1,5)]
colnames(Duck_lockdown.temp2020) = c("lockdown", "count")
Duck_lockdown.temp2020$mean_actual = "actual"
Duck_lockdown.temp2021 <- Duck_lockdown.scaled[3:6, c(1,6)]
colnames(Duck_lockdown.temp2021) = c("lockdown", "count")
Duck_lockdown.temp2021$mean_actual = "actual"

Duck_lockdown.toplot <- rbind(Duck_lockdown.tempmeans, Duck_lockdown.temp2020, Duck_lockdown.temp2021)

# PLOT

Duck_lockdown.bar <- ggplot(data=Duck_lockdown.toplot, aes(x=lockdown, y=count, fill=mean_actual)) + geom_bar(stat="identity", position = "dodge")+
  scale_fill_brewer(palette="Set1")
