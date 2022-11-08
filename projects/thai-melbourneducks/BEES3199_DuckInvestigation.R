#A closer look into Melbourne duck counts through the years

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

#All Atlas records Melbourne
Allrecords_melb <- as.data.frame(
  galah_call() |>                              
    galah_identify() |>                
    galah_filter(cl10929 == "GREATER MELBOURNE", year >= 2017, year <= 2021) |>  
    galah_group_by(year) |>
    atlas_counts())
Allrecords_melb$year = as.numeric(Allrecords_melb$year)
Allrecords_melb <- Allrecords_melb[order(-Allrecords_melb$year), , drop = FALSE]
rownames(Allrecords_melb) <- c(1:5) #Rows 3 and 4 keep swapping despite drop FALSE

#--------Order Anseriformes (Family comparison)
Anseriformes_data <- as.data.frame(
  galah_call() |>                              
    galah_identify("Anseriformes") |>                
    galah_filter(cl10929 == "GREATER MELBOURNE", year >= 2017, year <= 2021) |>  
    galah_group_by(year, family) |>
    atlas_counts())

#Scaled for overall Atlas entries
Anseriformes_scaled <- Anseriformes_data |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)

Anseriformes_scaled$"2017" = (Anseriformes_scaled$"2017"/Allrecords_melb[5,2])*100
Anseriformes_scaled$"2018" = (Anseriformes_scaled$"2018"/Allrecords_melb[4,2])*100
Anseriformes_scaled$"2019" = (Anseriformes_scaled$"2019"/Allrecords_melb[3,2])*100
Anseriformes_scaled$"2020" = (Anseriformes_scaled$"2020"/Allrecords_melb[2,2])*100
Anseriformes_scaled$"2021" = (Anseriformes_scaled$"2021"/Allrecords_melb[1,2])*100

Anseriformes_long <- Anseriformes_scaled[1:2,] |>
  pivot_longer(cols=c("2017", "2018", "2019", "2020", "2021"), names_to = "year", values_to = "count")

Anseriformes_scaled.bar <- ggplot(data=Anseriformes_long, aes(x=family, y=count, fill=year)) + geom_bar(stat="identity", position = "dodge")+
  scale_fill_brewer(palette="Set1")

#--------Family Anatidae (Genus comparison)

#Taxonomy
galah_call() |>
  galah_identify("Anatidae") |>
  galah_down_to(genus) |>
  atlas_taxonomy()

#Anatidae
Anatidae_data <- as.data.frame(
  galah_call() |>                              
    galah_identify("Anatidae") |>                
    galah_filter(cl10929 == "GREATER MELBOURNE", year >= 2017, year <= 2021) |>  
    galah_group_by(genus, year) |>
    atlas_counts())

#Scaled for overall Atlas entries
Anatidae_scaled <- Anatidae_data |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)
Anatidae_scaled[is.na(Anatidae_scaled)] = 0

Anatidae_scaled$"2017" = (Anatidae_scaled$"2017"/Allrecords_melb[5,2])*100
Anatidae_scaled$"2018" = (Anatidae_scaled$"2018"/Allrecords_melb[4,2])*100
Anatidae_scaled$"2019" = (Anatidae_scaled$"2019"/Allrecords_melb[3,2])*100
Anatidae_scaled$"2020" = (Anatidae_scaled$"2020"/Allrecords_melb[2,2])*100
Anatidae_scaled$"2021" = (Anatidae_scaled$"2021"/Allrecords_melb[1,2])*100

Anatidae_long <- Anatidae_scaled[1:13,] |>
  pivot_longer(cols=c("2017", "2018", "2019", "2020", "2021"), names_to = "year", values_to = "count")

Anatidae_scaled.bar <- ggplot(data=Anatidae_long, aes(x=genus, y=count, fill=year)) + geom_bar(stat="identity", position = "dodge")+
  scale_fill_brewer(palette="Set1")

#-------- Anatidae by year (MELB)
Anatidae_byyear.scaled <- Anseriformes_long[1:5,]

Anatidae_byyear.plot <- ggplot(data=Anatidae_byyear.scaled, aes(x=year, y=count)) + geom_bar(stat="identity")+
  scale_fill_brewer(palette="Set1")

#-------- Anatidae by month (MELB)
#Call data
Anatidae_bymonth.data <- as.data.frame(
  galah_call() |>                          
    galah_identify("Anatidae") |>               
    galah_filter(cl10929 == "GREATER MELBOURNE", year >= 2017, year <= 2021) |> 
    galah_group_by(month, year) |>     
    atlas_counts()) 
Anatidae_bymonth.data$month = as.numeric(Anatidae_bymonth.data$month)
Anatidae_bymonth.data$year = as.numeric(Anatidae_bymonth.data$year)

#Scale by total Atlas records
Anatidae_bymonth.pivot <- Anatidae_bymonth.data |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)
Anatidae_bymonth.pivot <- Anatidae_bymonth.pivot[order(Anatidae_bymonth.pivot$month),, drop=FALSE]

Anatidae_bymonth.scaled <- Anatidae_bymonth.pivot
Anatidae_bymonth.scaled$"2017" = (Anatidae_bymonth.scaled$"2017"/as.numeric(Allrecords_melb[5,2]))*100
Anatidae_bymonth.scaled$"2018" = (Anatidae_bymonth.scaled$"2018"/as.numeric(Allrecords_melb[4,2]))*100
Anatidae_bymonth.scaled$"2019" = (Anatidae_bymonth.scaled$"2019"/as.numeric(Allrecords_melb[3,2]))*100
Anatidae_bymonth.scaled$"2020" = (Anatidae_bymonth.scaled$"2020"/as.numeric(Allrecords_melb[2,2]))*100
Anatidae_bymonth.scaled$"2021" = (Anatidae_bymonth.scaled$"2021"/as.numeric(Allrecords_melb[1,2]))*100

Anatidae_bymonth.scaled.long <- Anatidae_bymonth.scaled |>
  pivot_longer(cols=c(2:6), names_to = "year", values_to = "count")

# PLOT
Anatidae_bymonth.scaled.long$month<-as.numeric(Anatidae_bymonth.scaled.long$month)
Anatidae_bymonth.scaled.long$month<-as.factor(Anatidae_bymonth.scaled.long$month)
Anatidae_bymonth.scaled.long$year<-as.factor(Anatidae_bymonth.scaled.long$year)

#2020
Anatidae_bymonth.scaled20<-Anatidae_bymonth.scaled.long[!(Anatidae_bymonth.scaled.long$year=="2021"),]

Waterbirds_melb2020.plot <- ggplot(Anatidae_bymonth.scaled20, aes(x=month, y=count, group=year)) +
  geom_rect(aes(NULL,NULL,xmin=4,xmax=5.387,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", size=0.5, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=7.290,xmax=10.871,fill="Lockdown"),
            ymin=0,ymax=Inf, colour="yellow", size=0.5, alpha=0.2) +
  scale_fill_manual(values=c("Lockdown" = "yellow")) +
  geom_line(aes(color = year), linewidth = 1) +
  labs(title="Records of ducks in Greater Melbourne (with 2020 lockdowns highlighted)",x="Month", y = "No. of records") + 
  theme(plot.title=element_text(size=10)) +
  scale_color_manual(values=c("red","orange","green", "blue", "purple"))

#2021
Anatidae_bymonth.scaled21<-Anatidae_bymonth.scaled.long[!(Anatidae_bymonth.scaled.long$year=="2020"),]

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

#-------- Anatidae by year with 2017-19 means
Anatidae_byyear.means <- Anatidae_byyear.scaled |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)

Anatidae_byyear.means$"2017_19_mean"= rowMeans(Anatidae_byyear.means[ ,2:4])

Anatidae_byyear.means$"diff_as_percent20" = ((Anatidae_byyear.means$"2020" - Anatidae_byyear.means$"2017_19_mean")/ Anatidae_byyear.means$"2017_19_mean")*100

Anatidae_byyear.means$"diff_as_percent21" = ((Anatidae_byyear.means$"2021"- Anatidae_byyear.means$"2017_19_mean")/ Anatidae_byyear.means$"2017_19_mean")*100

"family" <- "Anatidae"
"year" <- c("2017", "2018", "2019", "2020", "2021")
Anatidae_byyear.meansonly <- data.frame(family,year)
Anatidae_byyear.meansonly$"count" <- c(Anatidae_byyear.means$"2017_19_mean")   
Anatidae_byyear.meansonly$"mean_actual" = "mean"

Anatidae_byyear.toplot <- Anatidae_byyear.scaled
Anatidae_byyear.toplot$"mean_actual" = "actual"
Anatidae_byyear.toplot <- rbind(Anatidae_byyear.meansonly,Anatidae_byyear.toplot)

# PLOT

Anatidae_byyear.means.bar <- ggplot(data=Anatidae_byyear.toplot, aes(x=year, y=count, fill=mean_actual)) + geom_bar(stat="identity", position = "dodge")+
  scale_fill_brewer(palette="Set1")

#--------Next step: Specific lockdowns
