#Dubois Challenges 2024
#Week 9 
#########################

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2024 
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 

#########################

## Packages
library(tidyverse)

## Load Data
data <- read.csv("data/week9Data.csv")%>%
  mutate(fill = 100,
         freeLab = paste0(Free, '%'),
         pos = Slave)

data[9,6]<- 89

## Base plot
plot <- ggplot(data)+
  geom_area(aes(Year, fill), fill = "#2b8157", 
            color = "#1d1e1b", linewidth=0.4)+
  geom_area(aes(Year, Slave), fill = "#1d1e1b")+
  geom_text(aes(Year, pos+1.75, label = freeLab),
            family = "B52-ULCW00-ULC")+
  geom_text(aes(Year, fill+2, label = Year),
            family = "B52-ULCW00-ULC",
            size = 4.5)+
  coord_cartesian(clip = "off",
                  x=c(1789, 1871),
                  y=c(3.5, 126))+
  theme_void()+
  theme(plot.background = element_rect(fill = '#ded3c6', color = NA))


## Add line segments 
plotLine <- plot +
  annotate("segment", x = 1800, xend = 1800, y = 100, yend = 92,
           color = "#1d1e1b", linewidth = 0.2, alpha=0.4)+
  annotate("segment", x = 1810, xend = 1810, y = 100, yend = 90.5,
           color = "#1d1e1b", linewidth = 0.2, alpha=0.4)+
  annotate("segment", x = 1820, xend = 1820, y = 100, yend = 91,
           color = "#1d1e1b", linewidth = 0.2, alpha=0.4)+
  annotate("segment", x = 1830, xend = 1830, y = 100, yend = 90,
           color = "#1d1e1b", linewidth = 0.2, alpha=0.4)+
  annotate("segment", x = 1840, xend = 1840, y = 100, yend = 91,
           color = "#1d1e1b", linewidth = 0.2, alpha=0.4)+
  annotate("segment", x = 1850, xend = 1850, y = 100, yend = 92,
           color = "#1d1e1b", linewidth = 0.2, alpha=0.4)+
  annotate("segment", x = 1860, xend = 1860, y = 100, yend = 93,
           color = "#1d1e1b", linewidth = 0.2, alpha=0.4)



## Add labels
plotLab <- plotLine + 
  annotate("text", x = 1830, y= 57, label = "SLAVES\nESCLAVES",
           family = "B52-ULCW00-ULC",
           size = 7.5,
           lineheight = 0.8,
           color = '#ded3c6') +
  annotate("text", x = 1830, y= 96, label = "FREE  -  LIBRE",
           family = "B52-ULCW00-ULC",
           size = 5.25,
           lineheight = 0.8,
           color = "#1d1e1b")


## Add titles 
title1 <- "PROPORTION  OF  FREEMEN AND  SLAVES  AMONG  AMERICAN  NEGROES ."
title2 <- "PROPORTION  DES  NÈGRES  LIBRES  ET  DES  ESCLAVES  EN  AMÉRIQUE ."
subtitle <- "DONE BY ATLANTA UNIVERSITY ."

plotFinal <- plotLab +
  annotate("text", x = 1830, y = 129.5, label = title1, 
           family = "B52-ULCW00-ULC",
           size = 4,
           alpha=0.8)+
  annotate("text", x = 1830, y = 122, label = title2, 
           family = "B52-ULCW00-ULC",
           size = 4,
           alpha=0.8)+
  annotate("text", x = 1830, y = 115, label = subtitle, 
           family = "B52-ULCW00-ULC",
           size = 3,
           alpha=0.8)

plotFinal

## Final
ggsave("week9.png", width = 6, height = 7.75, units = "in")
