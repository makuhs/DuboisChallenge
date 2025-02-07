
#Dubois Challenges 2024
#Week 3 

#################

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2024 
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 

#################
  
## Packages 
library(tidyverse)
library(ggpattern)
library(magick)

#data load
data <- readr::read_csv("2024/data/week3Data.csv",
                        col_types = "fn")

## Data wrangle & clean
dataLab <- data %>%
  mutate(yearLab = as.character(year),
         acreLab = as.character(""),
         pos = acres/2)

#replace 0 with O for closer font similarity
dataLab$yearLab[7] = "188O"
dataLab$yearLab[16] = "189O"

#space after 1 for font spacing
dataLab$yearLab[17] = "1891 "

#add bar labs into empty acreLab col
dataLab$acreLab[1] = "338,769"
dataLab$acreLab[25] = "1,O62,223" #0 replace


## Load image of hand-filled bars from original plate to use as fill (package: ggpattern)
## note: I moved this file into ggpattern library manually before loading here -- there's probably a better way... 
redFill <- system.file("img", "duboisFillBrightRed.png", package="ggpattern")

## Base plot
plot <- ggplot(dataLab, aes(acres, year))+ 
  geom_col_pattern(
           color = "grey15",
           linewidth = 0.15,
           width = 0.5,
           pattern = 'image',
           pattern_type = 'squish',
           pattern_filename = redFill)+
  scale_y_discrete(limits=rev)+
  labs (x = "",
        y = "")+
  coord_cartesian(clip = "off",
                  x=c(-70000, 1100000),
                  y=c(0.3, 27))+ 
  theme_void()+
  theme(plot.background = element_rect(fill = '#e9e1d0', color = NA))


## Add labels
plotLab <- plot +
  geom_text(aes(x = pos, y = year, label = acreLab),
          family = "B52-ULCW00-ULC",
          size = 4.5,
          color = "grey15")+
  geom_text(aes(x = -1000, y = year, label = yearLab),
            family = "Vasarely-Light",
            hjust = 1.1,
            size = 4.7)


# Add title 
caption <- "Dubois Challenge 2024 -- Week 3 -- Molly Kuhs "
title <- "\nACRES OF LAND OWNED BY NEGROES\nIN GEORGIA."

plotFinal <- plotLab +
  annotate("text", x=531000, y = 26.4, label = title,
           family = "B52-ULCW00-ULC",
           hjust = 0.5,
           size = 6.8,
           lineheight = 0.85,
           color = "#151613",
           alpha=0.88)+
  annotate("text", x= 1150000, y = 0, label = caption, 
           color = "#151613",
           family = "Vasarely-Light",
           size = 3,
           hjust = 1,
           alpha=0.6)
  
plotFinal  


#final 
ggsave("week3.png", width = 7.8, height = 9.45, units = "in") 

