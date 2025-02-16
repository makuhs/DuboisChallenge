
#Dubois Challenges 2025
#Week 6
#Molly Kuhs

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2025
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 


## Packages ------------------------------
library(tidyverse)
library(patchwork)

# for curly brackets:
devtools::install_github("NicolasH2/ggbrace") #https://github.com/nicolash2/ggbrace
library(ggbrace)



## Data ------------------------------

data<- read.csv("2025/data/week6Data.csv")%>%
  rename(PropVal = Property.Value..Dollars.)%>%
  
  #create labels: add commas, $, and replace 0 with O for better font match
  mutate(yearLabs = gsub("0", "O", as.character(Year)),
         yearLabs = ifelse(yearLabs == 1898, 1899, yearLabs),
         ownersLabs = gsub("0", "O", formatC(Owners, 
                                             format = "f", 
                                             big.mark = ",", 
                                             digits = 0)),
         propLabs = paste0("$", 
                          gsub("0", "O", formatC(PropVal, 
                                                format = "f", 
                                                big.mark = ",", 
                                                digits = 0))))


## Create Layers ------------------------------

## Create backgound reference
bg <- data.frame(
  x = c(0, 100),
  y=c(100,100))

## Create individual plots for layering 
l1<- data %>%
  mutate(across(c(Owners, ownersLabs), 
                ~ ifelse(Year > 1897, NA, .)))%>%
  
  ggplot(., aes(Owners, factor(Year), fill = City))+  
  geom_col(position = position_dodge(width = 0.12),
         width = 0.12,
         color = "#443f3c",
         linewidth = 0.15)+
 geom_text(aes(x= ifelse(Owners < 1000, 320, Owners/2), 
                label = ownersLabs),
            position = position_dodge(width = 0.12),
            family = "Vasarely-Light",
            size = 3)+
  geom_text(aes(x= -100, label = yearLabs),
            family = "Vasarely-Light",
            size = 2.4,
            color = "#242322")+
  scale_y_discrete(limits = rev(levels(factor(data$Year))))+
  scale_fill_manual(values = c("#8c91ae", "#ecac28"))+
  coord_cartesian(xlim = c(-100, 2200))+
  theme_void()+
  theme(legend.position = "none")


l2<- data %>%
  mutate(across(c(PropVal, propLabs), 
                ~ ifelse(Year < 1890, NA, .)),
         City = factor(City, levels = rev(c("Atlanta", "Savannah"))))%>%
  
  ggplot(., aes(factor(Year), PropVal, fill = City))+  
  geom_col(position = position_dodge(width = 0.17),
           width = 0.17,
           color = "#443f3c",
           linewidth = 0.15)+
  geom_text(aes(y= ifelse(PropVal < 800000, PropVal*0.62, PropVal/2), 
                label = propLabs),
            position = position_dodge(width = 0.17),
            angle=90,
            family = "Vasarely-Light",
            size = 2.9)+
  geom_text(aes(y= -20000, label = yearLabs),
            family = "Vasarely-Light",
            size = 2.6,
            color = "#242322")+
  scale_fill_manual(values = c("#ecac28", "#8c91ae"))+
  coord_cartesian(ylim = c(-10000, 1400000))+
  theme_void()+
  theme(legend.position = "none")


l3<- data %>%
  mutate(across(c(Owners, ownersLabs), 
                ~ ifelse(Year < 1891, NA, .)))%>%

  ggplot(., aes(Owners, factor(Year), fill = City))+  
  geom_col(position = position_dodge(width = 0.12),
           width = 0.12,
           color = "#443f3c",
           linewidth = 0.15)+
    geom_text(aes(x= ifelse(Owners > 2000, Owners*0.45, Owners/1.8), 
                  label = ownersLabs),
              position = position_dodge(width = 0.12),
              family = "Vasarely-Light",
              size = 3)+
  scale_y_discrete(limits = rev(levels(factor(data$Year))))+
  scale_fill_manual(values = c("#8c91ae", "#ecac28"))+
  coord_cartesian(xlim = c(-100, 2200))+
  theme_void()+
  theme(legend.position = "none")


l4<- data %>%
  mutate(across(c(PropVal, propLabs), 
                ~ ifelse(Year > 1880, NA, .)),
         City = factor(City, levels = rev(c("Atlanta", "Savannah"))))%>%
  
  ggplot(., aes(factor(Year), PropVal, fill = City))+  
  geom_col(position = position_dodge(width = 0.17),
           width = 0.17,
           color = "#443f3c",
           linewidth = 0.15)+
  geom_text(aes(y= PropVal/2, label = propLabs),
            position = position_dodge(width = 0.17),
            angle=90,
            family = "Vasarely-Light",
            size = 2.9)+
  scale_fill_manual(values = c("#ecac28", "#8c91ae"))+
  coord_cartesian(ylim = c(-10000, 1400000))+
  theme_void()+
  theme(legend.position = "none")


# Build Plot ------------------------------

ggplot(bg, aes(x, y))+
  geom_area(fill = NA)+
  theme_void()+
  theme(plot.margin = margin(t = 10, r = 10, b = 65, l = 15),
        plot.background = element_rect(fill = "#dbc9b9", color = "#dbc9b9"))+
  
  coord_cartesian(clip = "off",
                  xlim = c(0,100),
                  ylim = c(0,100))+

  # add large brackets: 
  stat_brace(aes(x=8, y=c(11, 89)),
             rotate = 270,
             width = 7,
             bending = 4,
             color = "#443f3c",
             linewidth = 0.1)+
  
  stat_brace(aes(x=c(16, 87), y=1),
             rotate = 180,
             width = 6,
             bending = 4.5,
             color = "#443f3c",
             linewidth = 0.1)+
  
  # inefficiently add small brackets...(couldn't get this to work with factors)  
  stat_brace(aes(x=9.4, y=c(82, 87)),
             rotate = 270,
             width = 1.2,
             color = "#443f3c",
             linewidth = 0.1)+
  
  stat_brace(aes(x=9.4, y=c(47.5, 52.5)),
             rotate = 270,
             width = 1.2,
             color = "#443f3c",
             linewidth = 0.1)+
  
  stat_brace(aes(x=9.4, y=c(13, 18)),
             rotate = 270,
             width = 1.2,
             color = "#443f3c",
             linewidth = 0.1)+
  
  # add text annotations:
  annotate("text", x = 52, y = -8, label = "PROPERTY.",
           family = "Vasarely-Light",
           hjust = 0.5,
           size = 3.2)+
  
  annotate("text", x = -0.5, y = 50, label = "OWNERS.",
           family = "Vasarely-Light",
           hjust = 1,
           size = 2.4)+
  
  # add legend: 
  geom_rect(aes(xmin = 16.5, xmax = 22.5, ymin = -14, ymax = -11.5), 
            fill = "#ecac28", color = "#443f3c",
            linewidth = 0.1)+
  
  geom_rect(aes(xmin = 80.5, xmax = 86, ymin = -14, ymax = -11.5), 
            fill = "#8c91ae", color = "#443f3c",
            linewidth = 0.1)+
  
  # add legend text: 
  annotate("text", x = 23.5, y = -12.75, label = "= SAVANNAH",
           family = "Vasarely-Light",
           hjust = 0,
           size = 3)+
  
  annotate("text", x = 79, y = -12.75, label = "ATLANTA =",
           family = "Vasarely-Light",
           hjust = 1,
           size = 3)+
  
  # add title:
  annotate("text", x = 50, y = 100, 
           label = "NEGRO PROPERTY IN TWO CITIES\nOF GEORGIA .",
           hjust = 0.5,
           family = "B52-ULCW00-ULC",
           size = 5.8,
           lineheight = 0.8,
           color = "#242322",
           alpha=0.85)+
  
  annotate("text", x = 106.5, y = -17, label = "MOLLY KUHS - DUBOIS CHALLENGE 2025",
           hjust = 1,
           family = "Vasarely-Light",
           size = 2.25)+
  
  
  
  # scale and layer bars
  inset_element(l1, l=0.05, b=0.0, r=1, t=1)+
  inset_element(l2, l=0.05, b=0.0, r=0.97, t=0.93)+
  inset_element(l3, l=0.05, b=0.0, r=1, t=1)+
  inset_element(l4, l=0.05, b=0.0, r=0.97, t=0.88)
  

## Final ------------------------------
ggsave("2025/Week06/week6.png", width = 7, height = 8.5, units = "in") 



