#Initial investigative steps to determine what was most interesting under the broad topic of 'birds'

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

#--------Main groups of bird
#Songbirds (passeriformes), waterbirds (podicipediformes, pelecaniformes, ciconiiformes, gruiformes, charadriiformes), birds of prey (strigiformes, falconiformes), waterfowl (anseriformes), flightless birds (struthioniformes), parrots (psittaciformes), kingfishers and bee-eaters (coraciiformes), malleefowl (galliformes), other

#CALL DATA ---- Main groups Melbourne (2017-21)
Maingroups_melb.data <- as.data.frame(
  galah_call() |>                              
    galah_identify("passeriformes","podicipediformes", "pelecaniformes", "ciconiiformes", "gruiformes", "charadriiformes", "strigiformes", "falconiformes", "anseriformes", "struthioniformes", "psittaciformes", "coraciiformes", "galliformes") |>                
    galah_filter(cl10929 == "GREATER MELBOURNE", year >= 2017, year <= 2021) |>  
    galah_group_by(order, year) |>
    atlas_counts())  
View(Maingroups_melb.data)

#CALL DATA ---- All birds Melbourne (2017-21)
Allbirds_melb.data <- as.data.frame(
  galah_call() |>                              
    galah_identify("aves") |>                
    galah_filter(cl10929 == "GREATER MELBOURNE", year >= 2017, year <= 2021) |>  
    galah_group_by(year) |>
    atlas_counts()) 
Allbirds_melb.data$order <- "All birds"
Allbirds_melb.data$year = as.numeric(Allbirds_melb.data$year)
Allbirds_melb.data <- Allbirds_melb.data[order(Allbirds_melb.data$year, decreasing=TRUE),, drop=FALSE]
rownames(Allbirds_melb.data) <- c(1:5) #Same issue as above
View(Allbirds_melb.data)

#--------Did bird counts increase 2017-21? (MELB)

Allrecords_melb.colnames <- Allrecords_melb
colnames(Allrecords_melb.colnames) = c("year", "all_records")

Allbirds_melb.colnames <- Allbirds_melb.data[,1:2]
colnames(Allbirds_melb.colnames) = c("year", "bird_records")

Birdscounts_melb.comparison <- cbind(Allrecords_melb.colnames, Allbirds_melb.colnames)
Birdscounts_melb.comparison <- Birdscounts_melb.comparison[,-3]

Birdscounts_melb.comparison$percent_of_total_records <- (Birdscounts_melb.comparison$bird_records/Birdscounts_melb.comparison$all_records)*100

Birdcount_percent_melb.plot <- ggplot(data=Birdscounts_melb.comparison, aes(x=year, y=percent_of_total_records)) +
  geom_bar(stat="identity")  #Not really

#--------Which groups of birds were noticeably different in 2020-21?

#Create data frame
Maingroups_melb.pivot.temp <- Maingroups_melb.data |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)

#Calculate 'other'
Maingroups_melb.sums <- data.frame (year = 2017:2021, order = "Other",
 countsum = colSums(Maingroups_melb.pivot.temp[,-1]))
Maingroups_melb.sums <- Maingroups_melb.sums[order(Maingroups_melb.sums$year, decreasing=TRUE),]

Maingroups_melb.other <- cbind(Maingroups_melb.sums, Allbirds_melb.data)
Maingroups_melb.other <- Maingroups_melb.other[,c(1,2,3,5)]
colnames(Maingroups_melb.other) = c("year","order","sum_maingroups", "sum_allbirds")

Maingroups_melb.other$count = Maingroups_melb.other$sum_allbirds - Maingroups_melb.other$sum_maingroups
Maingroups_melb.other <- Maingroups_melb.other[,-c(3,4)]

#Combine
Maingroups_melb.list <- rbind(Maingroups_melb.data, Maingroups_melb.other, Allbirds_melb.data)
  View(Maingroups_melb.list)

Maingroups_melb.byorder <- Maingroups_melb.list |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)
View(Maingroups_melb.byorder)

#Sort into groups
Songbirds_melb <- Maingroups_melb.byorder[1,]
Songbirds_melb$order <- "Songbirds"
Waterbirds_melb <- Maingroups_melb.byorder[c(4,5,6,7,9),]
Waterbirds_melb <- data.frame (year = 2017:2021, order = "Waterbirds", count = colSums(Waterbirds_melb[,2:6]))
Waterbirds_melb <- Waterbirds_melb |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)
Waterfowl_melb <- Maingroups_melb.byorder[3,]
Waterfowl_melb$order <- "Waterfowl"
Birds_prey_melb <- Maingroups_melb.byorder[c(10,11),]
Birds_prey_melb <- data.frame (year = 2017:2021, order = "Birds_of_prey", count = colSums(Birds_prey_melb[,2:6]))
Birds_prey_melb <- Birds_prey_melb |>
  pivot_wider(names_from = year, values_from = count, names_sort = TRUE)
Flightless_melb <- Maingroups_melb.byorder[13,]
Flightless_melb$order <- "Flightless_birds"
Parrots_melb <- Maingroups_melb.byorder[2,]
Parrots_melb$order <- "Parrots"
Kingfishers.etc_melb <- Maingroups_melb.byorder[8,]
Kingfishers.etc_melb$order <- "Kingfishers_and_bee_eaters"
Malleefowl_melb <- Maingroups_melb.byorder[12,]
Malleefowl_melb$order <- "Malleefowl"
Other_melb <- Maingroups_melb.byorder[14,]

Maingroups_melb.bygroup <- rbind(Songbirds_melb, Waterbirds_melb, Waterfowl_melb, Birds_prey_melb, Flightless_melb, Parrots_melb, Kingfishers.etc_melb, Malleefowl_melb, Other_melb)
  View(Maingroups_melb.bygroup)

Maingroups_melb.long <- Maingroups_melb.bygroup |>
  pivot_longer(cols=c("2017", "2018", "2019", "2020", "2021"), names_to = "year", values_to = "count")

#Adjust to reflect overall change in Atlas records

Maingroups_melb.scaled <- Maingroups_melb.bygroup

Maingroups_melb.scaled$"2017" = (Maingroups_melb.scaled$"2017"/as.numeric(Allrecords_melb[5,2]))*100
Maingroups_melb.scaled$"2018" = (Maingroups_melb.scaled$"2018"/as.numeric(Allrecords_melb[4,2]))*100
Maingroups_melb.scaled$"2019" = (Maingroups_melb.scaled$"2019"/as.numeric(Allrecords_melb[3,2]))*100
Maingroups_melb.scaled$"2020" = (Maingroups_melb.scaled$"2020"/as.numeric(Allrecords_melb[2,2]))*100
Maingroups_melb.scaled$"2021" = (Maingroups_melb.scaled$"2021"/as.numeric(Allrecords_melb[1,2]))*100

Maingroups_melb.scaled.long <- Maingroups_melb.scaled[1:9,] |>
  pivot_longer(cols=c("2017", "2018", "2019", "2020", "2021"), names_to = "year", values_to = "count")

Majorgroups_melb.scaled.bar <- ggplot(data=Maingroups_melb.scaled.long, aes(x=order, y=count, fill=year)) + geom_bar(stat="identity", position = "dodge")+
  scale_fill_brewer(palette="Set1")

#--------Which orders of birds stand out?
View(Maingroups_melb.byorder)

Maingroups_melb.waterbirdsonly <- Maingroups_melb.byorder[c(3,4,5,6,7,9),]
Waterbirdorders_melb.scaled <- Maingroups_melb.waterbirdsonly

Waterbirdorders_melb.scaled$"2017" = (Waterbirdorders_melb.scaled$"2017"/Allrecords_melb[5,2])*100
Waterbirdorders_melb.scaled$"2018" = (Waterbirdorders_melb.scaled$"2018"/Allrecords_melb[4,2])*100
Waterbirdorders_melb.scaled$"2019" = (Waterbirdorders_melb.scaled$"2019"/Allrecords_melb[3,2])*100
Waterbirdorders_melb.scaled$"2020" = (Waterbirdorders_melb.scaled$"2020"/Allrecords_melb[2,2])*100
Waterbirdorders_melb.scaled$"2021" = (Waterbirdorders_melb.scaled$"2021"/Allrecords_melb[1,2])*100

Waterbirdorders_melb.pivot <- Waterbirdorders_melb.scaled |>
  pivot_longer(cols=c("2017", "2018", "2019", "2020", "2021"), names_to = "year", values_to = "count")

Waterbird_orders.bar <- ggplot(data=Waterbirdorders_melb.pivot, aes(x=order, y=count, fill=year)) + geom_bar(stat="identity", position = "dodge")+
  scale_fill_brewer(palette="Set1")

#--------Conclusion: Order Anseriformes

