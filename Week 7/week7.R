
#Dubois Challenges 2024
#Week 7 
#########################

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2024 
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 

#########################

# Packages 
library(tidyverse)
library(ggpattern)
library(magick)


## Load data 
data <- read.csv("data/week7Data.csv", 
                 header = F) 

colnames(data)<- c("con", "rate")

data[10,1] <- "Suéde" #add accent 


## Add factor for color fill

dataFull <- data %>%
  mutate(fill = case_when(
    con != "Negroes, U.S.A." ~ "A",
    con == "Negroes, U.S.A." ~ "B"
  ))


## Load filepath of images of hand-colored bars from original plate
## I moved these into ggpattern library manually before loading here -- not sure if there is a better way. 

fillFile <- c(
  system.file("img", "duboisFillGreen.png", package="ggpattern"),
  system.file("img", "duboisFillRed.png", package="ggpattern"))



## Base Plot

plot <- ggplot(dataFull, aes(rate, reorder(con, rate)))+
  geom_col_pattern(aes(pattern_filename = fill),
    width = 0.57,
           color = "#151613",
           linewidth = 0.2,
           pattern = 'image',
           pattern_type = 'squish')+
  scale_pattern_filename_discrete(choices = fillFile)+ 
  coord_cartesian(clip = "off", 
                  x = c(-14, 82),
                  y=c(-0.25, 13.9))+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#d6cabd"))


## Add labels
  plotLab <- plot +
    geom_text(aes(x=-14, y=con, label = con),
            hjust=0,
            family = "Perpetua",
            size = 3.6,
            color = "#151613",
            alpha=0.8)+
  #bounding lines
  annotate("segment", x=-16, xend = 85, y = 11.4, yend = 11.4,
         color ="#151613",
         alpha=0.2,
         linewidth = 0.1)+
  annotate("segment", x=0, xend = 0, y = -5, yend = 11.4,
           color ="#151613",
           alpha=0.2,
           linewidth = 0.1)
  

## Add plot titles
  
  title <- "Illiteracy of the American Negroes compared with that of other nations."
  subtitle <- "Proportion d' illettrés parmi les Nègres Americains comparée à celle des autres nations."
  subsubtitle <- "Done by Atlanta University."
  caption <- "#DuBoisChallenge2024 -- Week 7 -- Molly Kuhs"
  
plotTitle <- plotLab +
annotate("text", x= 35, y = 13.6, label = title, 
           color = "#151613",
           family = "Perpetua",
           size = 5.8,
           hjust = 0.5,
           lineheight = 0.9,
           alpha=0.6)+
  annotate("text", x= 35, y = 12.95, label = subtitle, 
           color = "#151613",
           family = "Perpetua",
           size = 4.3,
           hjust = 0.5,
           alpha=0.6)+
  annotate("text", x= 35, y = 12.32, label = subsubtitle, 
           color = "#151613",
           family = "Perpetua",
           size = 3.7,
           hjust = 0.5,
           alpha=0.6)+
  annotate("text", x= 83, y = -0.5, label = caption, 
           color = "#151613",
           family = "Perpetua",
           size = 3.3,
           hjust = 1,
           alpha=0.4)+
#line dividers
annotate("segment", x = 27, xend = 43, y = 13.2, yend = 13.2, color = "#151613", linewidth = 0.1, alpha=0.4)+
  annotate("segment", x = 27, xend = 43, y = 12.6, yend = 12.6, color = "#151613", linewidth = 0.1, alpha=0.4)


plotTitle


## final 


 


