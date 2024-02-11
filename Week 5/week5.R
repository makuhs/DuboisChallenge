#Dubois Challenges 2024
#Week 5 

###########################

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2024 
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 

###########################


#Packages 
library(tidyverse)

#data 
data <- read.csv("data/week5Data.csv")%>%
  mutate(col = 1)

#plot 
p<- ggplot(data, aes(col, Percentage, fill = Category))+
  geom_col(width = 4.5,
           color = "#38271f",
           linewidth = 0.1)+
  theme_void()+
  scale_fill_manual(values = c("#1e1e1e", "#42271b", "#fdbb00"))+
  coord_cartesian(y = (c(4, 101)), clip = "off")+
  xlim(c(-6, 5))+
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#e2d3c2",color = "#e2d3c2"),
    plot.background = element_rect(fill = "#e2d3c2", color = "#e2d3c2")
  )

#add labels 

title <-"\nRACE  AMALGAMATION  IN  GEORGIA ."
subtitle <- "BASED  ON  A  STUDY  OF  4O,OOO  INDIVIDUALS  OF  NEGRO  DESCENT."
caption <- "\nDubois Challenge 2024 -- Week 5 -- Molly Kuhs\n"

pLabs<- p + 
  labs(title = title,
       subtitle = subtitle,
       caption = caption)+
  theme(plot.title = element_text(family = "B52-ULCW00-ULC",
                                  size = 14.5,
                                  hjust = 0.5,
                                  color="#38271f"),
        plot.subtitle = element_text(family = "Vasarely-Light",
                                     size = 9.5,
                                     hjust = 0.5,
                                     color="#38271f"),
        plot.caption = element_text(family = "Vasarely-Light",
                                    size = 7.5,
                                    hjust = 0.5))

#annotate
pLabs +
  #barlabels
  #textlabels
  annotate("text", x =-4.4 , y = 85, 
           label = "BLACK.",
           family = "B52-ULCW00-ULC",
           size = 4.5,
           hjust = 0)+
  #subtext
  annotate("text", x =-3.8 , y = 55, 
           label = "I.E.  FULL-BLOODED\nNEGROES",
           family = "Vasarely-Light",
           size = 2.5,
           hjust = 0)


