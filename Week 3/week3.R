
#Dubois Challenges 2024
#Week 3 
------------------------

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2024 
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 

------------------------
  
#Packages 
library(tidyverse)

#data load
data <- readr::read_csv("week3Data.csv",
                        col_types = "fn")

#data wrangle
dataLab <- data %>%
  mutate(yearLab = as.character(year),
         acreLab = as.character(""),
         pos = acres/2)

#replace 0 with O for closer font similarity
dataLab$yearLab[7] = "188O"
dataLab$yearLab[16] = "189O"

#add bar labs
dataLab$acreLab[1] = "338,769"
dataLab$acreLab[25] = "1,O62,223" #0 replace
  
#space after 1 for font spacing
dataLab$yearLab[17] = "1891 "


#base plot
plot<- ggplot(dataLab, aes(acres, year))+ 
  geom_col(fill = "#cb2c44",
           color = "grey30",
           linewidth = 0.1,
           width = 0.5)+
  scale_y_discrete(limits=rev)+
  geom_text(aes(x = pos, y = year, label = acreLab),
                family = "B52-ULCW00-ULC",
                size = 3.35,
            color = "grey15")+
  geom_text(aes(x = 0, y = year, label = yearLab),
            family = "Vasarely-Light",
            hjust = 1.1)+
  labs (x = "",
        y = "")+
  coord_cartesian(clip = "off")+ 
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0.25,0.5,0.25,1, "cm"),
        plot.background = element_rect(fill = '#decfbe'))

#titles 
caption <- "Dubois Challenge 2024 -- Week 3 -- Molly Kuhs "
title <- "\nACRES OF LAND OWNED BY NEGROES\nIN GEORGIA."

plot +
  labs(title = title,
       caption = caption)+
  theme(plot.title = element_text(family = "B52-ULCW00-ULC",
                                  size = 16,
                                  hjust = 0.5),
        plot.caption = element_text(family = "Vasarely-Light",
                                    size = 8))

#final 
ggsave("week3.png", width = 6, height = 8, units = "in") 

