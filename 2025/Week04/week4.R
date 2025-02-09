
#Dubois Challenges 2025
#Week 4 
#Molly Kuhs

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2025
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 


## Packages ------------------------------
library(tidyverse)
library(ggalt)


## Data ------------------------------

data<-read.csv("2025/data/week4Data.csv")%>%
  mutate(adjNum = Property.Valuation/100000)

# create smooth spline
spline_full <- as.data.frame(spline(data$Year, data$adjNum))

#create highlighted sections
spline_start <- spline_full %>%
  filter(x < 1874)

spline_end <- spline_full %>%
  filter(x > 1899)


## Define Aesthetic Components ------------------------------

# set bg color
bg<- "#e6d7c1"

# generate gridline positions 
x_grid <- seq(floor(1870), ceiling(1900), by = 1)
y_grid <- seq(floor(0), ceiling(48), by = 1)

# subset gridlines to span between objects
y_grid_filtered <- y_grid[y_grid %% 10 == 0 & y_grid > 1]

# create x-axis labels 
yearLab <- data %>%
  filter(Year %% 5 == 0)%>%
  mutate(yearLab = str_replace_all(as.character(Year), "0", "O"))   #for font consistency 

# create y-axis labels
leftLabs <- data.frame(
  x = c(1862, 1862, 1863, 1862, 1862, 1863, 1862, 1862, 1863, 1862, 1862, 1863, 1862, 1862, 1863),
  y = c(4, 6, 10, 14, 16, 20, 24, 26, 30, 34, 36, 40, 44, 46, 47.6), 
  lab = c("$", "$", "1,OOO,OOO","$", "$", "2,OOO,OOO","$", "$", 
          "3,OOO,OOO","$", "$", "4,OOO,OOO","$", "$", "DOLLARS"), #swap O for 0 for font consistency
  size = c(3, 3, 2.2, 3, 3, 2.2, 3, 3, 2.2, 3, 3, 2.2, 3, 3, 2.3))


# create polygons to breakup gridlines around multi-line text annotations:
poliShape <- data.frame(
  x = c(1875, 1875, 1879.25, 1879.25, 1880.5, 1880.5, 1876.5, 1876.5),
  y = c(23, 24.5, 24.5, 23.5, 23.5, 22.2, 22.2, 23))

riseShape <- data.frame(
  x = c(1880, 1880, 1883.5, 1883.5, 1885.5, 1885.5, 
        1890.5, 1890.5, 1883.8, 1883.8, 1881.5, 1881.5),
  y = c(41, 42.2, 42.2, 41.2, 41.2, 40.2, 40.2, 39, 39, 40, 40, 41))

disShape <- data.frame(
  x = c(1895.5, 1895.5, 1894.25, 1894.25, 1895.5, 1895.5, 1893.5, 1893.5,
        1899.5, 1899.5, 1897.5, 1897.5, 1898.8, 1898.8, 1897.5, 1897.5 ),
  y = c(22.1, 22.8, 22.8, 23.5, 23.5, 24.2, 24.2, 25.5, 25.5, 
        24.2, 24.2, 23.5, 23.5, 22.8, 22.8, 22.1))



## Build Plot ------------------------------

ggplot() +

# set gridlines graph (right):
  geom_segment(data = data.frame(x = x_grid), 
               aes(x = x, xend = x, y = ymin, yend = ymax), 
               color = "#c95454",
               linewidth = 0.15,
               alpha=0.6) +
  
  geom_segment(data = data.frame(y = y_grid), 
               aes(x = xmin, xend = xmax, y = y, yend = y), 
               color = "#c95454",
               linewidth = 0.15,
               alpha=0.6) +
  
  #set gridlines scale (left):
  geom_segment(data = data.frame(y = y_grid), 
               aes(x = 1861, xend = 1865, y = y, yend = y), 
               color = "#c95454",
               linewidth = 0.15,
               alpha=0.6) +
  
  #set gridlines span:
  geom_segment(data = data.frame(y = y_grid_filtered), 
               aes(x = 1865.5, xend = 1870, y = y, yend = y), 
               color = "#c95454",
               linewidth = 0.15,
               alpha=0.6) +
  
  #set x-axis labels: 
  geom_text(data = yearLab, aes(Year, -0.5, label = yearLab),
            family = "Vasarely-Light",
            size = 2.25)+
  
  #set y axis labels
  geom_label(data = leftLabs, aes(x, y, label = lab),
            family = "Vasarely-Light",
            size = size,
            hjust = 0.5, 
            fill = bg,
            label.size = 0)+
  
  #add text annotations
  #multi-line chunks accompanied by polygon to break grid lines:
  geom_polygon(data = riseShape, aes(x, y),
               fill = bg,
               color = NA)+
  annotate("text", x = 1880, y = 42, 
           label = "RISE OF\n     THE NEW\n             INDUSTRIALISM",
           hjust = 0,
           vjust = 1,
           family = "Vasarely-Light",
           size = 3,
           lineheight = 0.9,
           color = "#242322")+
  
  geom_polygon(data = poliShape, aes(x, y),
               fill = bg,
               color = NA)+  
  annotate("text", x = 1875, y = 24, 
           label = "POLITICAL\n      UNREST.",
           hjust = 0,
           vjust = 1,
           family = "Vasarely-Light",
           size = 3,
           lineheight = 0.8,
           color = "#242322")+
  
  geom_polygon(data = disShape, aes(x, y),
               fill = bg,
               color = NA)+ 
  annotate("text", x = 1896.5, y = 25, 
           label = "DISFRANCHSMENT\nAND\nPROSCAPTIVE\nLAWS",
           hjust = 0.5,
           vjust = 1,
           family = "Vasarely-Light",
           size = 2.2,
           lineheight = 0.95,
           color = "#242322")+

  annotate("label", x = 1890, y = 15.5, 
           label = "LYNCHING",
           hjust = 0,
           vjust = 0.5,
           family = "Vasarely-Light",
           size = 2.5,
           lineheight = 0.8,
           color = "#242322",
           fill = bg,
           label.size = 0)+
  
  annotate("label", x = 1872, y = 4, 
           label = "KU-KLUXISM",
           hjust = 0,
           vjust = 0.5,
           family = "Vasarely-Light",
           size = 2.9,
           lineheight = 0.8,
           color = "#242322",
           fill = bg,
           label.size = 0,
           angle=90)+
  
  annotate("label", x = 1894, y = 3, 
           label = "FINANCIAL PANIC",
           hjust = 0,
           vjust = 0.5,
           family = "Vasarely-Light",
           size = 2.75,
           lineheight = 0.8,
           color = "#242322",
           fill = bg,
           label.size = 0,
           angle=90)+
  
  #add main chart line: 
  geom_line(data = spline_full, aes(x, y),
            linewidth = 2,
            color = "#242322")+
  
  #add line highlights: 
  geom_line(data = spline_start, aes(x, y),
            linewidth = 1,
            color = bg)+
  
  geom_line(data = spline_end, aes(x, y),
            linewidth = 1,
            color = bg)+
  
 
  #draw section boarders:
  geom_rect(aes(xmin = 1870, xmax = 1900, ymin = 0, ymax = 48), 
            fill = NA, color = "#242322", 
            linewidth = 0.2) +
  
  geom_rect(aes(xmin = 1861, xmax = 1865, ymin = 0, ymax = 48), 
            fill = NA, color = "#242322",
            linewidth = 0.2)+
  
  
  #clip ends of spline (too lazy to figure this out another way...come back to this)
  geom_rect(aes(xmin = 1868, xmax = 1869.965, ymin = 2, ymax = 5), 
            fill = bg, color = NA,
            linewidth = 0.2)+
  
  geom_rect(aes(xmin = 1900.05, xmax = 1901, ymin = 42, ymax = 44), 
            fill = bg, color = NA,
            linewidth = 0.2)+
  

  #set theme:
  theme_void() +
  theme(plot.background = element_rect(fill = bg, color = NA))+
  coord_fixed(clip = "off",
              x=c(1858,1901),
              y=c(-0.5, 51))+
  
  #add title:
  annotate("text", x = 1879.5, y = 51, 
           label = "VALUATION  OF  TOWN  AND  CITY  PROPERTY  OWNED\nBY  GEORGIA  NEGROES .",
           hjust = 0.5,
           family = "B52-ULCW00-ULC",
           size = 4.5,
           lineheight = 0.9,
           color = "#242322",
           alpha=0.85)+
  
  #add subtitle: 
  annotate("text", x = 1902, y = -2.3, label = "MOLLY KUHS - DUBOIS CHALLENGE 2025",
           hjust = 1,
           family = "Vasarely-Light",
           size = 2,
           alpha=0.5)


## Final ------------------------------
ggsave("2025/Week04/week4.png", width = 6, height = 7.5, units = "in") 
