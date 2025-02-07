

#Dubois Challenges 2025
#Week 2 
#Molly Kuhs

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2025
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 


## Packages ------------------------------
library(tidyverse)
library(sf)
library(stringr)
library(patchwork)


## Data ----------------------------------

georgiaShape <- sf::read_sf("2025/data/georgia-1880-county-shapefile")%>%
  mutate(county = str_to_title(NHGISNAM)) #ensure capitalization match

# csv includes color coding &
# text alignment noted for counties with shapes where centered text would not fit
data <- read.csv("2025/data/week3Data.csv")%>%
  mutate(county = str_to_title(county))

combo <- merge(georgiaShape, data, by = "county")


# colors
colors <- c("#9a9cb3", "#b99981", "#425d4c", "#d1c2b0", "#e49090", "#cc1c39", "#f4af00")


## Plot ----------------------------------

#build map 
p1<- ggplot(combo)+
  geom_sf(aes(fill = color),
          alpha=0.8,
          linewidth = 0.15,
          color = "grey20")+
  geom_sf_text(aes(label = acres, vjust = align),
               family = "Caveat",
               size = 3,
               alpha = 0.7)+
  coord_sf(crs = 4327,
           clip= "off")+
  scale_fill_manual(values = colors)+
  theme_void()+
  theme(legend.position = "none")


#set background canvas
x<-c(0, 100)
y<- c(100, 100)
background<- data.frame(x, y)

#build plot
ggplot(background, aes(x, y))+
  geom_area(fill = "#e6d7c3")+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#e6d7c3", color = NA,))+
 
  #add title:
  annotate("text", x = 50, y = 103.75, label = "LAND OWNED BY NEGROES IN GEORGIA, U.S.A.   187O - 19OO",
           hjust = 0.5,
           family = "B52-ULCW00-ULC",
           size = 6,
           alpha=0.85)+
  
  #add annotation:
  annotate("text", x = 84, y = 78, 
           label = "THE FIGURES INDICATE THE NUMBER OF\nACRES OWNED IN EACH COUNTY IN 1899",
           hjust = 0.5,
           family = "Vasarely-Light",
           size = 2.25,
           alpha=0.9,
           lineheight = 0.95)+
  
  #add subtitle: 
  annotate("text", x = 95, y = -10, label = "MOLLY KUHS - DUBOIS CHALLENGE 2025",
           hjust = 1,
           family = "Vasarely-Light",
           size = 3,
           alpha=0.5) +
  
  #place map:
  inset_element(p1, 0.1, 0.05, 0.95, 0.9)


## Final: 
ggsave("2025/Week03/week3.png", width = 7.5, height = 9.25, units = "in") 
  