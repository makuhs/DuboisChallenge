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
  coord_cartesian(y = (c(4, 103.5)), clip = "off")+
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
       caption = caption)+
  theme(plot.title = element_text(family = "B52-ULCW00-ULC",
                                  size = 14.5,
                                  hjust = 0.5,
                                  color="#38271f"),
        plot.caption = element_text(family = "Vasarely-Light",
                                    size = 7.5,
                                    hjust = 0.5))+
  annotate("text", x= -0.5, y= 107.15, label = subtitle,
          family = "Vasarely-Light",
          size = 3.1,
          hjust = 0.5,
          color="#38271f")

#annotate
pLabs +
 
   #barlabels
  annotate("text", x =1 , y = 78, 
           label = "44%",
           family = "B52-ULCW00-ULC",
           size = 4,
           hjust = 0.5,
           color = "#e5d6c3")+
  annotate("text", x =1 , y = 36, 
           label = "40%",
           family = "B52-ULCW00-ULC",
           size = 4,
           hjust = 0.5,
           color = "#9b0826",
           alpha = 0.4)+
  annotate("text", x =1 , y = 8, 
           label = "16%",
           family = "B52-ULCW00-ULC",
           size = 4,
           hjust = 0.5,
           color = "#38271f")+
  
  #textlabels
  annotate("text", x =-4.6 , y = 94, 
           label = "BLACK.",
           family = "B52-ULCW00-ULC",
           size = 4.8,
           hjust = 0,
           color = "#38271f")+
  annotate("text", x =-4.6 , y = 49, 
           label = "BROWN.",
           family = "B52-ULCW00-ULC",
           size = 4.8,
           hjust = 0,
           color = "#38271f")+
  annotate("text", x =-4.6 , y = 10.2, 
           label = "YELLOW.",
           family = "B52-ULCW00-ULC",
           size = 4.8,
           hjust = 0,
           color = "#38271f")+
 
   #subtext
  annotate("text", x =-3.8 , y = 89.8, 
           label = "I.E.  FULL-BLOODED\nNEGROES.",
           family = "Vasarely-Light",
           size = 2.3,
           hjust = 0,
           lineheight = .9,
           color = "#38271f")+
  annotate("text", x =-3.8 , y = 41.5, 
           label = "I.E.  PERSONS  WITH\nSOME  WHITE  BLOOD\nOR DESCENDANTS\nOF  LIGHT  COLORED\nAFRICANS.",
           family = "Vasarely-Light",
           size = 2.3,
           hjust = 0,
           lineheight = .9,
           color = "#38271f")+
  annotate("text", x =-3.8 , y = 4.9, 
           label = "I.E.  PERSONS  WITH\nMORE  WHITE  THAN\nNERGRO  BLOOD.",
           family = "Vasarely-Light",
           size = 2.3,
           hjust = 0,
           lineheight = .9,
           color = "#38271f")


#final 
ggsave("week5.png", width = 5.65, height = 7, units = "in") 



##INCREASE Y AXIS 
##MESS WITH TEXT SIZE

