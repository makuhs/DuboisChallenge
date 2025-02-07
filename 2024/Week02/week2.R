
#Dubois Challenges 2024
#Week 2 

###########################

#Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2024 
#DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 

###########################


#Packages 
library(tidyverse)
library(gridExtra)

#data load
data <- read.csv("2024/data/week2.csv")
data$Year <- as.factor(data$Year)

#set value for background
data2 <- data %>%
  mutate(bg = 3,
         label = Free)

#add for labeling: 0 switched with O for font consistency
data2$yearLab <- c("179O", "18OO", "181O", "182O", "183O", "184O", "185O", "186O", "187O")

#remove extra bar but maintain labels 
data2$Free[9] = 0
data2$bg[9] = 0

#Switch 0 with O for font consistency
data2$label <- as.character(data2$label)
data2$label[5] = "O.8"
data2$label[6] = "O.9"
data2$label[7] = "O.7"
data2$label[8] = "O.8"

#Add % only to first and last 
data2$label[1] = "1.3 %"
data2$label[9] = "1OO %"


#base plot

plot <- ggplot(data2)+
  geom_col(aes(bg, Year), fill="grey9", width = 0.97)+
  geom_col(aes(Free, Year), fill = "#cb2c44", width = 0.97)+
  geom_text(aes(x = -0.25, y = Year, label = label), 
            hjust = 0,
            vjust = -3,
            size = 3.7,
            family = "Vasarely-Light")+
  geom_text(aes(x = 3.5, y = Year, label = yearLab), 
            vjust = -3,
            size = 3.7,
            family = "Vasarely-Light")+
  scale_y_discrete(limits=rev)+
  scale_x_reverse(breaks = c(3, 2, 1),
                  labels = c("3%", "2%", "1%"),
                  position = "top")+
  coord_cartesian(xlim = c(3.5, -0.25),
                  ylim = c(9, 1),
                  clip = "off")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Vasarely-Light",
                                   size = 8),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0.25,4.2,0.5,3.8, "cm"),
        plot.background = element_rect(fill = '#decfbe'))+
  labs(x = "",
       y = "")


#add bar shaping 
plotShape <- plot +
  annotate("polygon", x = c(1.29, 1.7, 1.29), y = c(9.48, 8.52, 8.52), fill = "#cb2c44")+
  annotate("polygon", x = c(1.71, 1.71, 1.21), y = c(7.48, 6.51, 6.51), fill = "grey9")+
  annotate("polygon", x = c(1.21, 1.21, 0.8), y = c(6.48, 5.52, 5.52), fill = "grey9")+
  annotate("polygon", x = c(0.78, 0.9, 0.78), y = c(5.48, 4.51, 4.51), fill = "#cb2c44")+
  annotate("polygon", x = c(0.91, 0.91, 0.7), y = c(4.48, 3.52, 3.52), fill = "grey9")+
  annotate("polygon", x = c(0.69, 0.8, 0.69), y = c(3.48, 2.52, 2.52), fill = "#cb2c44")+
  annotate("polygon", x = c(0.79, 3, 3, 0.79), y = c(2.48, 2.14, 1.51, 1.51), fill = "#cb2c44")


#add white space
plotSpace <- plotShape +
  annotate("segment", x = 1.29, xend = 1.7, y = 9.48, yend = 8.52, colour = "#decfbe", linewidth = 0.4)+
  annotate("segment", x = 1.7, xend = 1.7, y = 8.48, yend = 7.52, colour = "#decfbe", linewidth = 0.4)+
  annotate("segment", x = 1.7, xend = 1.2, y = 7.48, yend = 6.51, colour = "#decfbe", linewidth = 0.4)+
  annotate("segment", x = 1.2, xend = 0.8, y = 6.48, yend = 5.52, colour = "#decfbe", linewidth = 0.4)+
  annotate("segment", x = 0.78, xend = 0.9, y = 5.48, yend = 4.51, colour = "#decfbe", linewidth = 0.4)+
  annotate("segment", x = 0.9, xend = 0.7, y = 4.48, yend = 3.52, colour = "#decfbe", linewidth = 0.4)+
  annotate("segment", x = 0.69, xend = 0.8, y = 3.48, yend = 2.52, colour = "#decfbe", linewidth = 0.4)+
  annotate("segment", x = 0.79, xend = 3, y = 2.48, yend = 2.14, colour = "#decfbe", linewidth = 0.4)
  


#add rough edge
#generated a bunch of random numbers around 3 and set as a polygon shape with decending y coordinate 

x <- c(3.05, 3.02, 2.95, 2.88,
       2.99, 2.94, 2.9, 2.95, 2.88, 
       3.02, 2.9, 
       2.95, 2.88, 2.83, 2.86, 3,
       2.95, 3.0,3.02, 2.9, 
       2.95, 3.0, 
       3.02, 2.94, 2.98, 2.9, 2.85, 
       2.84, 3.0, 2.98, 2.92, 
       2.85, 2.95, 2.99, 3.1)

y<- c(9.5, 9.5, 9.4, 9.0,
      8.8, 8.6, 8.48, 8.2, 8.1, 
      7.8, 7.4, 
      6.9, 6.8, 6.7, 6.3, 6.1, 
      5.8, 5.7, 5.3, 5, 
      4.5, 4.3, 
      3.9, 3.6, 3.55, 3.2, 3, 
      2.9, 2.55, 2.5, 2.3, 
      1.9, 1.8, 1.5, 1.5)

plotEdge <- plotSpace+ 
  annotate("polygon", 
           x = x,
           y = y,
           fill = "#decfbe")

           

#add titles

title <- "\nS L A V E S    A N D    F R E E    N E G R O S.\n"
caption <- "\nDubois Challenge 2024 -- Week 2 -- Molly Kuhs"


plotTitle<- plotEdge +
  labs(title = title,
       caption = caption)+
  theme(plot.title = element_text(family = "B52-ULCW00-ULC",
                                  size = 14,
                                  hjust = 0.5),
        plot.caption = element_text(family = "Vasarely-Light",
                                    size = 8))
#add subtitle 
plotTitle+
  annotate("text", x = -0.5, y = 9.9, 
               label = "\nPERCENT\nOF\nFREE NEGROS",
               size = 2.5,
               family = "Vasarely-Light",
               lineheight = .8)

#final 
ggsave("week2.png", width = 6.5, height = 8, units = "in") 




