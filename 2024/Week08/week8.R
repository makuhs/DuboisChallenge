#Dubois Challenges 2024
#Week 8 
#########################

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2024 
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 

#########################

## Packages
library(tidyverse)
library(patchwork)

## Load Data
data <- read.csv("2024/data/week8Data.csv")

data1860 <- data[1,] %>%
  pivot_longer(cols = Slave:Free,
               names_to = "status", values_to = "percent")%>%
  mutate(group = as.factor(1))

data1860$percent <- as.numeric(data1860$percent)


data1890 <- data[3,]
colnames(data1890)[2] <- "Owners"
colnames(data1890)[3] <- "Tenants"

data1890 <- data1890 %>%
  pivot_longer(cols = Owners:Tenants,
               names_to = "status", values_to = "percent")%>%
  mutate(group = 1)

data1890$percent <- as.numeric(data1890$percent)


## Base plot 1860 
p60<- ggplot(data1860, aes(group, percent, fill = status))+
  geom_col()+
  scale_fill_manual(values = c("#005a2c", "#0a0c06"))+
  theme_void()+
  coord_cartesian(clip = "off",
                  x = c(0.92,1.25),
                  y = c(10, 285))+
  theme(legend.position = "none",
        plot.background = element_rect(fill = '#dbd0c4', color = NA))+
  #labels
  annotate("text", label = "89%", x = 1, y = 48.5, 
           hjust = 0.5,
           color = "#a70022",
           size = 8,
           family = "B52-ULCW00-ULC",
           alpha=0.7)+
  annotate("text", label = "SLAVES\nESCLAVES", x = 1, y = 38.5, 
           hjust = 0.5,
           color = "#a70022",
           size = 4.5,
           family = "B52-ULCW00-ULC",
           alpha=0.7,
           lineheight = 0.8)+
  annotate("text", label = "ll%", x = 0.65, y = 94.8, 
           hjust = 0.5,
           color = "#0a0c06",
           size = 7.5,
           family = "B52-ULCW00-ULC",
           alpha=0.7)+
  annotate("text", label = "FREE LABORER\nQUVRIERS LIBRES", x = 1.07, y = 94.6, 
           hjust = 0.5,
           color = "#0a0c06",
           size = 3.9,
           family = "B52-ULCW00-ULC",
           alpha=0.7,
           lineheight = 0.8)+
  annotate("text", label = "186O", x = 1, y = 106, 
           hjust = 0.5,
           color = "#0a0c06",
           size = 7,
           family = "B52-ULCW00-ULC")
  

## Base plot 1890 
p90<- ggplot(data1890, aes(group, percent, fill = status))+
  geom_col()+
  scale_fill_manual(values = c("#b7001c", "#004d0f"))+
  theme_void()+
  coord_cartesian(clip = "off",
                  x = c(0.25,1.5),
                  y = c(-26, 135))+
  theme(legend.position = "none",
        plot.background = element_rect(fill = '#dbd0c4', color = NA))+
  #labels 
  annotate("text", label = "81%", x = 1, y = 43, 
           hjust = 0.5,
           color = "#0a0c06",
           size = 6.8,
           family = "B52-ULCW00-ULC")+
  annotate("text", label = "TENANTS\nMÈTAYERS", x = 1, y = 37.5, 
           hjust = 0.5,
           color = "#0a0c06",
           size = 4.3,
           family = "B52-ULCW00-ULC",
           lineheight = 0.8)+
  annotate("text", label = "19%", x = 1, y = 92.5, 
           hjust = 0.5,
           color = "#0a0c06",
           size = 6.8,
           family = "B52-ULCW00-ULC")+
  annotate("text", label = "PEASANT PROPRIETORS\nPAYSANA PROPRIETAIRES", x = 1, y = 87.5, 
           hjust = 0.5,
           color = "#0a0c06",
           size = 3.8,
           family = "B52-ULCW00-ULC",
           lineheight = 0.8)+
  annotate("text", label = "189O", x = 1, y = 103, 
           hjust = 0.5,
           color = "#0a0c06",
           size = 6.4,
           family = "B52-ULCW00-ULC")

## Combine into single plot  
plotFull <- p60 + p90 + plot_layout(nrow = 1)


## Add lines 
plotLine <- plotFull +
  annotate("segment", x = -0.175, xend = 0.55, y = 26.5, yend = 81,
           color = "#0a0c06",
           linetype = 5, 
           alpha=0.5, 
           linewidth = 0.1)+
  annotate("segment", x = -0.175, xend = 0.55, y = 26.5, yend = 52,
           color = "#0a0c06",
           linetype = 5, 
           alpha=0.5, 
           linewidth = 0.1)+
  annotate("segment", x = -0.175, xend = 0.55, y = 22.5, yend = 35,
           color = "#0a0c06",
           linetype = 5, 
           alpha=0.5, 
           linewidth = 0.1)+
  annotate("segment", x = -0.175, xend = 0.55, y = 22.5, yend = 12,
           color = "#0a0c06",
           linetype = 5, 
           alpha=0.5, 
           linewidth = 0.1)+
  annotate("segment", x = -0.175, xend = 0.55, y = 20, yend = 0,
           color = "#0a0c06",
           linetype = 5, 
           alpha=0.5, 
           linewidth = 0.1)

## Add titles 

title <- "THE  RISE  OF  THE  NEFROES  FROM  SLAVERY  TO  FREEDOM  ON  ONE  GENERATION."
title2 <- "PROGRÈS  GRADUEL  DES  NÈGRES  DE  L'ESCLAVAGE  À  LA  LIBERTÉ  EN  UNE  GÉNÉRATION."
subtitle <- "DONE   BY   ATLANTA   UNIVERSITY."

descriptionE1 <- "IN 189O NEARLY ONE FIFTH OF THEM OWNED THEIR OWN HOMES AND FARMS.\nTHE ADVANCE WAS ACCOMPLISHED ENTIRELY WITHOUT STATE AID, AND IN THE\nFACE OF PROSCRIPTIVE LAWS."
descriptionF1 <- "EN 189O ENVIRON UN CINQUIÈME ÉTAIENT PROPRIÉTAIRES DE LEURS HAB-\nITATIONS ET DE LEURS FERMES, CE PROGRÈS S'EST ACCOMPLI SANS\nSECOURS AUCUN DE L'ETAT ET EN PRÉSENCE DE LOIS DÉFAVORABLES."

descriptionE2 <- "IN 186O NEARLY 9O% OF THE BLACKS WERE SLAVES."
descriptionF2 <- "EN 186O ENVIRON 9O% DES NÈGRES ÉTAIENT ESCLAVES"


plotFinal <- plotLine + 
  annotate("text", x = 0.2, y = 140, label= title, 
           hjust = 0.5,
           size = 4.4,
           family = "B52-ULCW00-ULC")+
  annotate("text", x = 0.2, y = 131, label= title2, 
           hjust = 0.5,
           size = 4.4,
           family = "B52-ULCW00-ULC")+
  annotate("text", x = 0.2, y = 118, label= subtitle, 
           hjust = 0.5,
           size = 3.8,
           family = "B52-ULCW00-ULC")+
  annotate("text", x = -1.15, y = 95, label= descriptionE1, 
           hjust = 0,
           size = 3,
           family = "Vasarely-Light")+
  annotate("text", x = -1.15, y = 84, label= descriptionF1, 
           hjust = 0,
           size = 3,
           family = "Vasarely-Light")+
  annotate("text", x = -1.07, y = 65, label= descriptionE2, 
           hjust = 0,
           size = 3.2,
           family = "Vasarely-Light")+
  annotate("text", x = -1.05, y = 60, label= descriptionF2, 
           hjust = 0,
           size = 3.2,
           family = "Vasarely-Light")

plotFinal

## Final 
ggsave("week8.png", width = 7.2, height = 10, units = "in") 

