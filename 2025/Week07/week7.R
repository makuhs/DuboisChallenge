
#Dubois Challenges 2025
#Week 7
#Molly Kuhs

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2025
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 


## Packages ------------------------------
library(tidyverse)
library(patchwork)


## Data ------------------------------

data <- read.csv("2025/data/week7Data.csv")%>%
  rename(hhVal = Houshold.Value..Dollars.)%>%
  
  #reformat for labels (add $, commas, dashes, and swap 0 for O for font match)
  mutate(valLab = gsub("0", "O", formatC(hhVal,
                                            format = "f", 
                                            big.mark = ",",
                                            digits = 0)),
         labs = case_when(
                    Year == 1875 ~paste0(" -------- $ ", valLab),
                    Year == 1880 ~paste0(" ---- $ ", valLab),
                    Year == 1885 ~paste0(" ---- ''   ", valLab),
                    Year == 1890 ~paste0(" ---- ''  ", valLab),
                    Year > 1890  ~paste0(" ---- '' ", valLab)),
         yearLabs = gsub("0", "O", as.character(Year)),
         
         #set y coordinates in cartesian coords for text
         yPos = (86 - (row_number() - 1)*1.55))
  

## Create spiral limits:
### Modeled off Ijeamaka Anyene's tidytuesday submission from 2021 that split
### spiraling circles into 2 groups!! (https://github.com/Ijeamakaanyene) 

  dataSpiral <- data %>%
  mutate(max = max(hhVal)*(0.8),
         x = 0,
         y = seq(10, by = -1.25, length.out = 6),
         slope = (10*1.25)/(-max-400000), #adjust denominator to control spiral tightness
         
         xEnd = pmin(hhVal, max),
         xEnd2 = ifelse(hhVal < max, NA, 
                         hhVal - max),
        
         yEnd = slope * xEnd + y,
         yEnd2 = (slope*2.5) * xEnd2 + yEnd) #adjusted slope to control spiral tightness
  

  ## Create plotting components ------------------------------
 
# background canvas cartesian coords for reference 
bg <- data.frame(
    x = c(0, 100),
    y=c(100,100)) 

# set colors 
colors <- c("#d5a9a0", "#9da0b0", "#c3ab91", "#dda51c", "#d0c3b2", "#c7374d")

# build spiral 
p1<- ggplot(data = dataSpiral) +
  
  # create spiral segments in black slightly larger to act as outline
  geom_segment(aes(x = x, xend = xEnd+200,
                   y = y, yend = yEnd),
               color = "#262719", 
               size = 4.2,
               alpha = 0.8) +
  geom_segment(aes(x = x, 
                   xend = ifelse(Year == 1890, (xEnd2*10.49)+900, (xEnd2*3.5)+1000), #adjust 1890 to match original
                   y = yEnd, 
                   yend = ifelse(Year == 1890, yEnd2-0.98, yEnd2)),
               color = "#262719",
               size = 4.2,
               alpha=0.8) +
  #create spiral segments with color 
  geom_segment(aes(x = x+200, xend = xEnd,
                   y = y, yend = yEnd,
                   color = as.factor(Year)), 
               size = 3.85) +
  geom_segment(aes(x = x, 
                   xend = ifelse(Year == 1890, xEnd2*10.5, xEnd2*3.5), #adjust 1890 to match original
                   y = yEnd, 
                   yend = ifelse(Year == 1890, yEnd2-1, yEnd2),
                   color = as.factor(Year)),
               size = 3.85) +
  #set aesthetics
  scale_color_manual(values = colors)+
  coord_polar(clip = "off") +
  ylim(-22, 13) +
  xlim(0, 1148200)+ # xmax to largest value
  theme_void() +
  theme(legend.position = "none")


## Build Plot ------------------------------

ggplot(bg, aes(x,y))+
  geom_area(fill = NA)+
  
  #set theme
  coord_cartesian(clip = "off")+
  theme_void()+
  theme(plot.background = element_rect(fill = "#d4c7b6", color = "#d4c7b6"))+
  
  #add text annotations:
  geom_text(data=data, aes(25.3, yPos, label = yearLabs),
            family = "Vasarely-Light",
            size = 3.7)+
  geom_text(data=data, aes(45, yPos, label = labs),
            family = "Vasarely-Light",
            size = 3.7,
            hjust = 1)+
  
  #add title: 
  annotate("text", x = 50, y = 99, 
           label = "ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE\nOWNED BY GEORGIA NEGROES .",
           hjust = 0.5,
           family = "B52-ULCW00-ULC",
           size = 5.25,
           lineheight = 0.9,
           color = "#262719",
           alpha=0.85)+
  
  #add subtitle: 
  annotate("text", x = 105, y = 0, label = "MOLLY KUHS - DUBOIS CHALLENGE 2025",
           hjust = 1,
           family = "Vasarely-Light",
           size = 3)+

  # inset spiral 
  inset_element(p1, l=-0.25, b=-0.03, r=1.15, t=0.96)



# Final ------------------------------
ggsave("2025/Week07/week7.png", width = 7, height = 8.5, units = "in") 

