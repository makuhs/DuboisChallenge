#Dubois Challenges 2024
#Week 10 
#########################

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2024 
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 

#########################

## Packages
library(tidyverse)
library(mapdata)
library(patchwork)

## Load Data
#pie chart
dataPie <- read.csv("2024/data/week10pie.csv")%>%
  mutate(order = c(1, 6, 4, 5, 3, 2),
         labelL = c("58.5%", "", "", "", "", "28.1%"), #groups for label placement 
         labelM = c("", "4.3%", "", "", "3.8%", ""),
         labelS = c("", "", "3.2%", "2.1%", "", ""))

#map 
mapFillDat <- read.csv("data/week10state.csv")

state <- map_data("state")%>%
  filter(region != "district of columbia")

dataMap <- state %>%
  left_join(mapFillDat, by = c("region" = "state"))


## Load text chunks 
titleEnglish <- paste(readLines("Week10/text/titleE.txt"), collapse="\n")
titleFrench <- paste(readLines("Week10/text/titleF.txt"), collapse="\n")
subtitleEnglish <- paste(readLines("Week10/text/subtitleE.txt"), collapse="\n")
subtitleFrench <- paste(readLines("Week10/text/subtitleF.txt"), collapse="\n")
midEnglish <- paste(readLines("Week10/text/middleE.txt"), collapse="\n")
midFrench <- paste(readLines("Week10/text/middleF.txt"), collapse="\n")
bottomEnglish <- paste(readLines("Week10/text/bottomE.txt"), collapse="\n")
bottomFrench <- paste(readLines("Week10/text/bottomF.txt"), collapse="\n")


## Base plot piechart 
p1<- ggplot(dataPie, aes(x = "", y = Percentage, fill = reorder(Occupation, order)))+
  geom_bar(stat="identity", width=1, alpha=0.9, color = "#2e2a25", linewidth = 0.15) +
  coord_polar("y", start=27.25,
              clip = "off")+
  geom_text(aes(x= 0.95, label = labelL), 
            position = position_stack(vjust = 0.5),
            family = "B52-ULCW00-ULC",
            hjust = 0.75,
            size = 3.8)+
  geom_text(aes(x=1.3, label = labelM), 
            position = position_stack(vjust = 0.5),
            family = "B52-ULCW00-ULC",
            size = 2.9)+
  geom_text(aes(x=1.35, label = labelS), 
            position = position_stack(vjust = 0.5),
            family = "B52-ULCW00-ULC",
            size = 2.5)+
  scale_fill_manual(values = c("#e22e4e", "#febb26", "#9f907c", "#eac8b8", "#c0a38b", "#6a74a1"))+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = NA, color = NA))


## Base plot map 
colorPal <- c("#e54865", "#f5c35a", "#edcbbd", "#aeaebb", "#7a84b6", "#242a68", "#dfcab3", "#795941", "#33352e", "#a6a491")

p2<- ggplot() +
  geom_polygon(data = dataMap, aes(long, lat, group = group, fill = fillGroup), 
               color = "black", linewidth = 0.15, alpha=0.9) +
  coord_map("conic", lat0 = 30)+
  scale_fill_manual(values = colorPal)+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = NA, color = NA))


## Set background canvas
x<-c(0, 100)
y<- c(100, 100)
background<- data.frame(x, y)


## Build combination plot
ggplot(background, aes(x, y))+
  geom_area(fill = "#eedfcc")+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#eedfcc", color = "#eedfcc"))+
  
  #add legend (English)
  annotate("point", x =2 , y = 46.85, size = 5.5, color = "#33352e")+
  annotate("point", x =2 , y = 46.85, size = 5.3, color = "#e54865")+
  annotate("point", x =2 , y = 44.1, size = 5.5, color = "#33352e")+
  annotate("point", x =2 , y = 44.1, size = 5.3, color = "#aeaebb")+
  annotate("point", x =2 , y = 41.3, size = 5.5, color = "#33352e")+
  annotate("point", x =2 , y = 41.3, size = 5.3, color = "#edcbbd")+
  annotate("point", x =2 , y = 38.5, size = 5.5, color = "#33352e")+
  annotate("point", x =2 , y = 38.5, size = 5.3, color = "#dfcab3")+
  annotate("point", x =2 , y = 35.75, size = 5.5, color = "#33352e")+
  annotate("point", x =2 , y = 35.75, size = 5.3, color = "#a6a491")+
  annotate("point", x =2 , y = 33, size = 5.5, color = "#33352e")+
  annotate("point", x =2 , y = 33, size = 5.3, color = "#f5c35a")+
  
  #add legend (French)
  annotate("point", x =103 , y = 46.85, size = 5.5, color = "#33352e")+
  annotate("point", x =103 , y = 46.85, size = 5.3, color = "#e54865")+
  annotate("point", x =103 , y = 44.1, size = 5.5, color = "#33352e")+
  annotate("point", x =103 , y = 44.1, size = 5.3, color = "#aeaebb")+
  annotate("point", x =103 , y = 41.3, size = 5.5, color = "#33352e")+
  annotate("point", x =103 , y = 41.3, size = 5.3, color = "#edcbbd")+
  annotate("point", x =103 , y = 38.5, size = 5.5, color = "#33352e")+
  annotate("point", x =103 , y = 38.5, size = 5.3, color = "#dfcab3")+
  annotate("point", x =103 , y = 35.75, size = 5.5, color = "#33352e")+
  annotate("point", x =103 , y = 35.75, size = 5.3, color = "#a6a491")+
  annotate("point", x =103 , y = 33, size = 5.5, color = "#33352e")+
  annotate("point", x =103 , y = 33, size = 5.3, color = "#f5c35a")+
  
  #add legend text
  annotate("text", x = 4.5, y=40, 
           label = "TEACHERS\nMINISTERS\nGOVERNMENT SERVICE\nBUISNESS\nOTHER PROFESSIONS\nHOUSE WIVES",
           hjust=0,
           size = 3,
           family = "Vasarely-Light",
           lineheight = 1.65)+
  annotate("text", x = 101, y=40, 
           label = "PROFESSEURS ET INSTUTUTEURS\nMINISTRES DE L'EVANGLE\nEMPLOYÉS DU GOUVERNMENT\nMARCHANDS\nMEDONS ADVOCATS ET ÉTUDIÀNTS\nMÈRES DE FAMIL'LE",
           hjust=1,
           size = 3,
           family = "Vasarely-Light",
           lineheight = 1.65,
           color = "#b28474")+
  
  #add titles & text 
  annotate("text", x = 0, y = 95.75, label = titleEnglish,
           hjust = 0,
           family = "B52-ULCW00-ULC",
           size = 6,
           lineheight = 0.8)+
  annotate("text", x = 0, y = 85, label = titleFrench,
           hjust = 0,
           family = "Vasarely-Light",
           size = 4.95,
           lineheight = 0.95,
           color = "#b28474")+
  annotate("text", x = 18, y = 73.5, label = subtitleEnglish,
           hjust = 0.5,
           family = "Vasarely-Light",
           size = 4.15,
           lineheight = 0.95,
           alpha=0.8)+
  annotate("text", x = 85, y = 73.5, label = subtitleFrench,
           hjust = 0.5,
           family = "Vasarely-Light",
           size = 4.15,
           lineheight = 0.95,
           color = "#b28474")+
  annotate("text", x = 43, y = 66.5,
           label = "CENTRE OF NEGRO POPULATION.\nATLANTA UNIVERSITY.",
           hjust = 0,
           family = "Vasarely-Light",
           size = 2.5,
           lineheight = 0.99,
           alpha=0.8)+
  annotate("text", x = 50, y = 61.5, label = midEnglish,
           hjust = 0.5,
           family = "Vasarely-Light",
           size = 3.7,
           lineheight = 1.75,
           alpha=0.8)+
  annotate("text", x = 50, y = 59.8, label = midFrench,
           hjust = 0.5,
           family = "Vasarely-Light",
           size = 3.7,
           lineheight = 1.75,
           color = "#b28474")+
  annotate("text", x = 1, y = 19.5, label = bottomEnglish,
           hjust = 0,
           family = "Vasarely-Light",
           size = 3.6,
           lineheight = 0.9,
           alpha=0.8)+
  annotate("text", x = 1, y = 7, label = bottomFrench,
           hjust = 0,
           family = "Vasarely-Light",
           size = 3.6,
           lineheight = 0.9,
           alpha=0.8)+
  
  #add symbols
  annotate("polygon", x = c(39, 39, 41.5, 41.5), y=c(65.2, 67.8, 67.8, 65.2), 
           fill = "#33352e")+
  annotate("point", x =40.25 , y = 67.15, size = 1.2, color = "#dfcab3")+
  annotate("text", x =40.25 , y = 65.4, label = "*", size = 5.5, color = "#dfcab3")+

  #overlay charts
  inset_element(p1, 0.27, 0.23, 0.73, 0.6)+
  inset_element(p2, 0.37, 0.54, 0.63, 0.89)


##final 
ggsave("week10.png", width = 7.5, height = 9.25, units = "in") 
