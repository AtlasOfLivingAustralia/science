#install packages
install.packages("galah")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("gridExtra")
install.packages("lubridate")
install.packages("tibble")
install.packages("remotes")
remotes::install_github("olihawkins/pilot")
install.packages("gtable")
install.packages("grid")
install.packages("patchwork")
install.packages("ggpubr")
#call packages
library(galah)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(lubridate)
library(tibble)
library(pilot)
library(gtable)
library(grid)
library(patchwork)
library(ggpubr)

galah_config(email = "thai.rushbrook@gmail.com")

options(scipen = 999)

#Plot using pilot
Ducks_week.pilot <- ggplot() +
  scale_fill_manual(values=c("Lockdown" = "#fff918"), name = NULL) +
  scale_color_manual(labels=c("2017-19 average", "2020-21 occurrences", name = "Year"))+
  geom_rect(aes(NULL,NULL,xmin=13,xmax=19,fill="Lockdown"),
            ymin=0,ymax=Inf, color= NA, alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=28,xmax=43,fill="Lockdown"),
            ymin=0,ymax=Inf, fill="#fff918",alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=60,xmax=60,fill="Lockdown"),
            ymin=0,ymax=Inf, fill="#fff918", alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=77,xmax=78,fill="Lockdown"),
            ymin=0,ymax=Inf, fill="#fff918", alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=82,xmax=83,fill="Lockdown"),
            ymin=0,ymax=Inf, fill="#fff918", alpha=0.2) +
  geom_rect(aes(NULL,NULL,xmin=84,xmax=95,fill="Lockdown"),
            ymin=0,ymax=Inf, fill="#fff918", alpha=0.2) +
  geom_line(data = Ducks_week.plot, aes(x = week, y = count, color = "2020-21 Records"), size=0.7) +
  geom_line(data = Ducks_week.plot, aes(x = week, y =  Ducks_week.plot$"2017_19_mean", color = "2017-19 Average"),linetype = "twodash", size=0.5) +
  labs(color= "Year",
    x = "Week Number",
    y = "Ducks recorded",
    caption = "Produced using the Galah package by Atlas of Living Australia",
    title = "Test") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 104)) + scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
   theme_pilot(grid = "", axes = "bottom") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=13)) +
  scale_color_pilot()


# After creating the plot, add a title and subtitle with add_pilot_titles
Ducks_week.pilot2 <- add_pilot_titles(
  Ducks_week.pilot,
  subtitle = "By week, with lockdowns highlighted in yellow") +
  theme(plot.title=element_text(size=10))

#Plot yearly data

MelDucks_yearly.plot <- ggplot(Allducks_Melb.scaled, aes(x=year, y=1, size = count, color = factor(year), label = year)) +
  geom_point(alpha=0.7) +
  geom_text(vjust = 0.4, size = 3, color = "white") +
  scale_size(range = c(10, 25), name="Ducks recorded in Melbourne") +
  scale_y_continuous(limits = c(0.9, 1.1), expand = c(0, 0)) +
  scale_x_continuous(limits = c(2016.5,2021.5), expand = c(0, 0)) +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_text(color = "white"),
        axis.ticks.y=element_blank(),
        axis.line.x.bottom=element_line(size=0.5), 
        axis.line.x.top=element_line(size=0.5),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
        geom_hline(yintercept=1.09)+
        labs(title = "By year", size = 10)+
        scale_color_manual(values=c("#30236d",
                              "#223577",
                              "#205b89",
                              "#e17b31",
                              "#e55e15"))


MelDucks_yearly.plot2 <- add_pilot_titles(
  MelDucks_yearly.plot,
  subtitle = "By year") +
  theme(plot.title=element_text(size=10))

#Combine!
ggarrange(arrangeGrob(MelDucks_yearly.plot, im_Duck, ncol = 2, widths = c(1.5,1)),
          Ducks_week.pilot, 
          nrow = 2, heights = c(1.5,4))+ 
  plot_annotation(
   title = "Ducks Records in Greater Melbourne 2020-21 Compared to Previous Years",
   theme = theme(plot.title = element_text(size = 15, hjust = 0.5,)))

#Add duck pic
install.packages("png")
library(png)
imgDuck <- readPNG(file.path("C:", "Users", "thair", "Desktop", "duck.png"))
im_Duck <- ggplot() + 
  background_image(imgDuck) +
  theme(aspect.ratio = 1,panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank())
