#Dubois Challenges 2024
#Week 1 

###########################

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2024 
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 

###########################


#Packages 
library(tidyverse)
library(sf)
library(patchwork)
library(ggforce)

#data load
georgiaShape <- sf::read_sf("Week 1/georgia-1880-county-shapefile")

#data clean & wrangle 
colnames(georgiaShape)[14] <- "data1870"
colnames(georgiaShape)[15] <- "data1880"

georgiaShape$data1870[is.na(georgiaShape$data1870)] <- "NoData"

dataCat <- georgiaShape %>%
  mutate(cat70 = case_when(data1870 == "> 1000" ~ 1,
                           data1870 == "1000 - 2500" ~ 2,
                           data1870 == "2500 - 5000" ~ 3,
                           data1870 == "5000 - 10000" ~ 4,
                           data1870 == "10000 - 15000" ~ 5,
                           data1870 == "15000 - 20000" ~ 6,
                           data1870 == "20000 - 30000" ~ 7,
                           data1870 == "NoData" ~ 0),
         cat80 = case_when(data1880 == "> 1000" ~ 1,
                           data1880 == "1000 - 2500" ~ 2,
                           data1880 == "2500 - 5000" ~ 3,
                           data1880 == "5000 - 10000" ~ 4,
                           data1880 == "10000 - 15000" ~ 5,
                           data1880 == "15000 - 20000" ~ 6,
                           data1880 == "20000 - 30000" ~ 7))


dataCat$cat70 <- as.factor(dataCat$cat70)
dataCat$cat80 <- as.factor(dataCat$cat80)


#plots

colorpal70 <- c("#decfbe","#395346","#e9b96f","#dea59b","#d64057","#d0b49e","#654531","#22255f")

title <-"N E G R O   P O P U L A T I O N   O F   G E O R G I A   B Y   C O U N T I E S."


#base1870plot
p70<- ggplot(dataCat)+
  geom_sf(aes(fill = cat70),
          alpha=0.9,
          linewidth = 0.15,
          color = "grey25")+
  annotate("text", x = -84.4, y=35.2,
           label = "1870",
           family = "B52-ULCW00-ULC",
           size = 3.5)+
  scale_fill_manual(values = colorpal70,
                    name = "")+
  theme_void()+
  coord_sf(crs = 4326, clip = 'off')+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#decfbe",color = "#decfbe"),
        plot.background = element_rect(fill = "#decfbe", color = "#decfbe"),
        plot.margin = margin(0.3,7,8,0.5, "cm"))+
  labs(x = "",
       y = "")+
  annotate("text", x = -81, y = 35.7, 
           label = title,
           family = "B52-ULCW00-ULC",
           size = 4)

p70

#base1880plot

colorpal80 <- c("#395346","#e9b96f","#dea59b","#d64057","#d0b49e","#654531","#22255f")


p80 <- ggplot(dataCat)+
  geom_sf(aes(fill = cat80),
          alpha=0.9,
          linewidth = 0.1,
          color = "grey25")+
  scale_fill_manual(values = colorpal80,
                    name = "")+
  annotate("text", x = -84.4, y=35.2,
           label = "1880",
           family = "B52-ULCW00-ULC",
           size = 3.5)+
  theme_void()+
  coord_sf(crs = 4326, clip = 'off')+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent",color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

p80


#upper labels: 

lab1 <- ggplot() +
  theme_void()+
  xlim(2, 6)+
  ylim(2, 6)+
  geom_circle(aes(x0 = 2.2, y0 = 5, r = 0.2, linewidth = I(0.1)), # will error but still changes linewidth
              fill = "#22255f", 
              color = "black")+
  geom_circle(aes(x0 = 2.2, y0 = 4.1, r = 0.2, linewidth = I(0.1)),
              fill = "#654531", 
              color = "black")+
  geom_circle(aes(x0 = 2.2, y0 = 3.2, r = 0.2, linewidth = I(0.1)),
              fill = "#d0b49e", 
              color = "black")+
  annotate("text", x = 2.75, y = 5, 
           label = "BETWEEN 2O,OOO AND 3O,OOO",
           family = "Vasarely-Light",
           size = 3.5,
           hjust = 0)+
  annotate("text", x = 2.75, y = 4.1, 
           label = "15,OOO TO 2O,OOO",
           family = "Vasarely-Light",
           size = 3.6,
           hjust = 0)+
  annotate("text", x = 2.75, y = 3.2, 
           label = "1O,OOO TO 15,OOO",
           family = "Vasarely-Light",
           size = 3.6,
           hjust = 0)+
  theme(panel.background = element_rect(fill = "transparent",color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))+
  labs(x = "",
       y = "")+
  coord_cartesian(clip = "off")


#bottom labels: 

lab2<- ggplot() +
  theme_minimal()+
  xlim(2, 5)+
  ylim(2, 5)+
  geom_circle(aes(x0 = 2.15, y0 = 4, r = 0.12, linewidth = I(0.1)),
              fill = "#d64057", 
              color = "black")+
  geom_circle(aes(x0 = 2.15, y0 = 3.4, r = 0.12, linewidth = I(0.1)),
              fill = "#dea59b", 
              color = "black")+
  geom_circle(aes(x0 = 2.15, y0 = 2.8, r = 0.12, linewidth = I(0.1)),
              fill = "#e9b96f", 
              color = "black")+
  geom_circle(aes(x0 = 2.15, y0 = 2.2, r = 0.12, linewidth = I(0.1)),
              fill = "#395346", 
              color = "black")+
  annotate("text", x = 2.4, y = 4, 
           label = "5OOO TO 1O,OOO",
           family = "Vasarely-Light",
           size = 3.5,
           hjust = 0)+
  annotate("text", x = 2.4, y = 3.4, 
           label = "2,5OO TO 5,OOO",
           family = "Vasarely-Light",
           size = 3.5,
           hjust = 0)+
  annotate("text", x = 2.4, y = 2.8, 
           label = "1,OOO TO 2,5OO",
           family = "Vasarely-Light",
           size = 3.5,
           hjust = 0)+
  annotate("text", x = 2.4, y = 2.2, 
           label = "UNDER 1,OOO",
           family = "Vasarely-Light",
           size = 3.5,
           hjust = 0)+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent",color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))+
  labs(x = "",
       y = "")+
  coord_cartesian(clip = "off")



#Combine Plots

maps <- p70 + 
  inset_element(test8, left = 0.9, bottom = -0.9, right = 2, top = 0.1)

labels <- maps +
  inset_element(lab1, left = 0.98, bottom = 0.25, right = 1.86, top = 0.95)+
  inset_element(lab2, left = -0.1, bottom = -0.8, right = 1.2, top = 0.2)

labels


#Final Plot 
ggsave("week1.png", width = 5.6, height = 6.6, units = "in") 


