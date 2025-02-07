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

#data load
georgiaShape <- sf::read_sf("2024/data/georgia-1880-county-shapefile")

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
colorpal80 <- c("#395346","#e9b96f","#dea59b","#d64057","#d0b49e","#654531","#22255f")

title <-"N E G R O   P O P U L A T I O N   O F   G E O R G I A   B Y   C O U N T I E S."
title2 <- "NEGRO POPULATION OF GEORGIA BY COUNTIES."
caption <- "\nDubois Challenge 2024 -- Week 1 -- Molly Kuhs"


#base1870plot
p70 <- ggplot(dataCat)+
  geom_sf(aes(fill = cat70),
          alpha=0.9,
          linewidth = 0.15,
          color = "grey20")+
  annotate("text", x = 84.5, y=35.15,
           label = "1870",
           family = "B52-ULCW00-ULC",
           size = 3.5,
           color="#362916")+
  scale_fill_manual(values = colorpal70,
                    name = "")+
  theme_void()+
  coord_sf(crs = 4327)+
  xlim(c(86, 75.5))+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e4d3bf",color = "#e4d3bf"),
        plot.background = element_rect(fill = "#e4d3bf", color = "#e4d3bf"),
        plot.title = element_text(family = "B52-ULCW00-ULC",
                                  size = 12.5,
                                  hjust = 0.5,
                                  color="#362916"),
          plot.margin = margin(0.3,0,0,0, "cm"))+
  labs(x = "",
       y = "",
       title = title2)

# add labels 70
p70L<- p70+
  annotate("text", x = 80, y = 34.5, 
           label = "BETWEEN 2O,OOO AND 3O,OOO",
           family = "Vasarely-Light",
           size = 3.1,
           hjust = 0)+ 
  annotate("text", x = 80, y = 33.6, 
           label = "15,OOO TO 2O,OOO",
           family = "Vasarely-Light",
           size = 3.1,
           hjust = 0)+
  annotate("text", x = 80, y = 32.7, 
           label = "1O,OOO TO 15,OOO",
           family = "Vasarely-Light",
           size = 3.1,
           hjust = 0)
  
#add points 70
p70LP <- p70L +
  annotate("point", x =80.5 , y = 34.5, size = 6.3, color = "#3d2e22")+
  annotate("point", x =80.5 , y = 34.5, size = 6, color = "#22255f")+
  annotate("point", x =80.5 , y = 33.6, size = 6.3, color = "#3d2e22")+
  annotate("point", x =80.5 , y = 33.6, size = 6, color = "#654531")+
  annotate("point", x =80.5 , y = 32.7, size = 6.3, color = "#3d2e22")+
  annotate("point", x =80.5 , y = 32.7, size = 6, color = "#d0b49e")




#base1880plot

p80 <- ggplot(dataCat)+
  geom_sf(aes(fill = cat80),
          alpha=0.9,
          linewidth = 0.1,
          color = "grey20")+
  scale_fill_manual(values = colorpal80,
                    name = "")+
  annotate("text", x = 84.25, y=35.15,
           label = "1880",
           family = "B52-ULCW00-ULC",
           size = 3.5,
           color = "#362916")+
  theme_void()+
  coord_sf(crs = 4327)+
  xlim(c(91, 80.5))+
  labs(caption = caption)+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#e4d3bf",color = "#e4d3bf"),
        plot.background = element_rect(fill = "#e4d3bf", color = "#e4d3bf"),
        plot.margin = margin(0,0,0.25,0, "cm"),
        plot.caption = element_text(family = "Vasarely-Light",
                                    size = 7.5,
                                    hjust = 0.5))

#add labels 80
p80L <- p80 +
  annotate("text", x =89.5 , y = 34.3, 
           label = "5,OOO TO 1O,OOO",
           family = "Vasarely-Light",
           size = 3.1,
           hjust = 0)+ 
  annotate("text", x = 89.5, y = 33.4, 
           label = "2,5OO TO 5,OOO",
           family = "Vasarely-Light",
           size = 3.1,
           hjust = 0)+
  annotate("text", x = 89.5, y = 32.5, 
           label = "1,OOO TO 2,5OO",
           family = "Vasarely-Light",
           size = 3.1,
           hjust = 0)+
  annotate("text", x = 89.5, y = 31.6, 
           label = "UNDER 1,OOO",
           family = "Vasarely-Light",
           size = 3.1,
           hjust = 0)

# add points 80
p80LP <- p80L +
  annotate("point", x =90 , y = 34.3, size = 6.3, color = "#3d2e22")+
  annotate("point", x =90 , y = 34.3, size = 6, color = "#d64057")+
  annotate("point", x =90 , y = 33.4, size = 6.3, color = "#3d2e22")+
  annotate("point", x =90 , y = 33.4, size = 6, color = "#dea59b")+
  annotate("point", x =90 , y = 32.5, size = 6.3, color = "#3d2e22")+
  annotate("point", x =90 , y = 32.5, size = 6, color = "#e9b96f")+
  annotate("point", x =90 , y = 31.6, size = 6.3, color = "#3d2e22")+
  annotate("point", x =90 , y = 31.6, size = 6, color = "#395346")


# combine
p70LP + p80LP + plot_layout(nrow = 2)

#Final Plot 
ggsave("week1.png", width = 5.45, height = 6.6, units = "in") 


