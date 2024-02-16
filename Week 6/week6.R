
#Dubois Challenges 2024
#Week 6 
#########################
  
  #Challege: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2024 
  #DuBois Styleguide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf 
  
#########################
  
#Packages 
library(tidyverse)
library(ggpattern)


#Data: generate boundaries of two main groups

yearB <- c(90, 90, 60, 30, 0, 0)
popB <- c(1.2, 0.95, 0.52, 0.25, -0.35, -0.35)
dataBlack <- data.frame(yearB, popB)

yearW<- c(0,92,92,0)
popW <- c(1.2, 1.2, 2, 2)
dataWhite <- data.frame(yearW, popW)


## Base Plot

plot <- ggplot()+
  geom_area_pattern(data = dataWhite, aes(popW, yearW, group=1), #add subtle striping on white
                    pattern_spacing = 0.0065,
                    pattern_angle = 135,
                    pattern_size = 0.3,
                    pattern_colour  = '#ddd0c1',
                    fill = "#dfd2c5")+
  coord_cartesian(clip = "off",
                  x=c(-0.28,1.85),
                  y=c(0.5, 109))+
  geom_area(data = dataBlack, aes(popB, yearB, group=1),
            fill = "#151613")+
  theme_void()+
  theme(plot.background = element_rect(fill = "#d1c4b7", color = NA),
        legend.position = "none")



## Add other shaped sections

xBrown <- c(1, 1.2, 1.205, 1.2, 1.208, 1.196,1.198, 1.19,1.201, 1.196,1.204, 1.208, 1.196, 1.198, 1.196, 1.195, 1.2)
yBrown <- c(0, 90, 80, 70, 60, 50, 40, 30, 20, 16.5, 14, 10, 5, 4, 3.5, 2, 0)

pShape <- plot +
  annotate("polygon", x = c(1.05, 1.2, 1.35), y = c(0, 90, 0), fill = "#f2ab00")+
  annotate("polygon", x = c(1.35, 1.2, 1.5), y = c(0, 90, 0), fill = "#f2ab00", alpha=0.3)+
  annotate("polygon", x= xBrown, y = yBrown, fill = "#794c27", alpha=0.9)



## Add serrated sections on top
set.seed(1019)  

#generate random, increasing & oscillating Y coordinates (black)

trend <- seq(from = 90.5, to = 92.5, length.out = 40)
fluctuations <- rnorm(40, mean = 0, sd = 0.15)  

random_numbers <- trend + fluctuations
blackY <- round(random_numbers, 2)
blackY[1] <- 90.00 #first and last at 90 to create polygon
blackY[40] <- 90.00
blackY[39] <- 92 #ensure connectivity to white shape

#generate increasing X coordinates (black): 
start_value <- 0.95
end_value <- 1.2
num_steps <- 40
increment <- (end_value - start_value) / (num_steps - 1)

increasing_numbers <- seq(start_value, end_value, by = increment)

blackX <- round(increasing_numbers, 2)
blackX[39] <- 1.2
blackX[40] <- 1.2

#generate random, oscillating Y coordinates (tan)
lower_numbers <- seq(from = 91.5, to = 92.1, by = 0.02)
upper_numbers <- seq(from = 92.2, to = 92.4, by = 0.02)
random_numbers <- numeric(75)

for (i in 1:75) {
  if (i %% 2 == 0) {
    random_numbers[i] <- sample(upper_numbers, 1)
  } else {
    random_numbers[i] <- sample(lower_numbers, 1)
  }
}

tanY <- round(random_numbers, 2)
tanY[1] <- 90.00 # set 1st and last to 90 to make polygon
tanY[75] <- 90.00
tanY[70:74] <- 91.4 #add decrease upper right corner
tanY[69] <- 91.8 


#generate increasing X coordinates (tan): 
start_value <- 1.2
end_value <- 1.95
num_steps <- 75
increment <- (end_value - start_value) / (num_steps - 1)

increasing_numbers <- seq(start_value, end_value, by = increment)

tanX <- round(increasing_numbers, 2)
tanX[2] <- 1.2 # set to get straight line at start
tanX[70:74] <- 1.9 #add decrease upper right corner

#plot
pShapeF<- pShape +
  annotate("polygon", x = tanX, y = tanY, fill = NA, color = "#151613", linewidth = 0.2)+
  annotate("polygon", x = blackX, y = blackY, fill = "#151613", color = "#151613", linewidth = 0.1)+
  annotate("polygon", x = c(1.885, 2, 2) , y = c(92.15, 92.15, 88.5), fill = "#d1c4b7", color = NA) #hide patterned corner
  


## Add grid lines and shape outlines
  
pOut <- pShapeF +
  annotate("polygon", x = c(1.05, 1.2, 1.35), y = c(0, 90, 0), fill = NA,  color = "#151613", linewidth=0.2)+
  annotate("segment", x = 0, xend = 1.298, y = 30, yend = 30, color = "#d1c4b7", linewidth = 0.4)+
  annotate("segment", x = 0, xend = 1.247, y = 60, yend = 60, color = "#d1c4b7", linewidth = 0.4)+
  annotate("segment", x = 0, xend = 1.2, y = 90, yend = 90, color = "#d1c4b7", linewidth = 0.4)+
  annotate("segment", x = 1.3, xend = 2, y = 30, yend = 30, color = "#151613", linewidth = 0.2)+
  annotate("segment", x = 1.247, xend = 2, y = 60, yend = 60, color = "#151613", linewidth = 0.2)+
  annotate("segment", x = 0, xend = 2, y = 0, yend = 0, color = "#151613", linewidth = 0.2)+ 
  annotate("segment", x = 1.95, xend = 2, y = 90, yend = 88.5, color = "#151613", linewidth = 0.2)+
  annotate("segment", x = -1, xend = 2, y = 92.8, yend = 92.8, color = "#151613", linewidth = 0.1, alpha=0.2)
  

## Add text annotations (REALLY inefficiently...)

pLab<-  pOut + 
   annotate("text", x= 0.4, y = 1.8, label = "6.337.98O", 
           color = "#d1c4b7",
           family = "B52-ULCW00-ULC",
           size = 5.5)+
  annotate("text", x= 0.68, y = 31.5, label = "3.542.147", 
           color = "#d1c4b7",
           family = "B52-ULCW00-ULC",
           size = 4.5)+
  annotate("text", x= 0.68, y = 28.9, label = "9O%", 
           color = "#d1c4b7",
           family = "B52-ULCW00-ULC",
           size = 3.5)+
  annotate("text", x= 0.65, y = 18, label = "NEGROES", 
           color = "#d1c4b7",
           family = "B52-ULCW00-ULC",
           size = 4.5)+
  annotate("text", x= 1.6, y = 18, label = "WHITES", 
           color = "#151613",
           family = "B52-ULCW00-ULC",
           size = 4.5)+
  annotate("text", x= -0.3, y = -1.2, label = "189O", 
           color = "#151613",
           family = "B52-ULCW00-ULC",
           size = 3.8)+
  annotate("text", x= 0.15, y = 30, label = "186O", 
           color = "#151613",
           family = "B52-ULCW00-ULC",
           size = 3.7)+
  annotate("text", x= 0.42, y = 60, label = "184O", 
           color = "#151613",
           family = "B52-ULCW00-ULC",
           size = 4.1)+
  annotate("text", x= 0.84, y = 90, label = "18OO", 
           color = "#151613",
           family = "B52-ULCW00-ULC",
           size = 4)+
  annotate("text", x= 0.4, y = -1.2, label = "85%", 
           color = "#151613",
           family = "B52-ULCW00-ULC",
           size = 3.8)+
  annotate("text", x= 1.2, y = -1.2, label = "15%", 
           color = "#151613",
           family = "B52-ULCW00-ULC",
           size = 3.8)+
  annotate("text", x= 1.2, y = 1.4, label = "1.113.O63", 
           color = "#151613",
           alpha=0.8,
           family = "B52-ULCW00-ULC",
           size = 3.5)+
  annotate("text", x= 1.2, y = 18, label = "MULATTOES", 
           color = "#151613",
           alpha=0.8,
           family = "B52-ULCW00-ULC",
           size = 3.2)+
  annotate("text", x= 1.2, y = 16.4, label = "MULATRES", 
           color = "#151613",
           alpha=0.8,
           family = "B52-ULCW00-ULC",
           size = 2.8)+
  annotate("text", x= 1.2, y = 28.9, label = "1O%", 
           color = "#151613",
           alpha=0.8,
           family = "B52-ULCW00-ULC",
           size = 3.5)+
  annotate("text", x= 1.19, y = 31, label = "411.613", 
           color = "#151613",
           alpha=0.8,
           family = "B52-ULCW00-ULC",
           size = 2.9)
pLab  

## Add chart titles

title <- "The Amalgamation of the White and Black elements of the population\nin the United States."
subtitle <- "Amalgamation des elements blancs et nois parmi la population Americaine."
subsubtitle <- "Done by Atlanta University."
caption <- "#DuboisChallenge2024 -- Week 6 -- Molly Kuhs"

 pFinal <- pLab +
   annotate("text", x= 0.8, y = 110, label = title, 
           color = "#151613",
           family = "Perpetua",
           size = 6,
           hjust = 0.5,
           lineheight = 0.9,
           alpha=0.6)+
  annotate("text", x= 0.85, y = 103, label = subtitle, 
           color = "#151613",
           family = "Perpetua",
           size = 4.3,
           hjust = 0.5,
           alpha=0.6)+
  annotate("text", x= 0.85, y = 98.5, label = subsubtitle, 
           color = "#151613",
           family = "Perpetua",
           size = 3.7,
           hjust = 0.5,
           alpha=0.6)+
   annotate("text", x= 1.9, y = -3.5, label = caption, 
            color = "#151613",
            family = "Perpetua",
            size = 3.3,
            hjust = 1,
            alpha=0.4)+
   #line dividers
  annotate("segment", x = 0.65, xend = 1, y = 105.3, yend = 105.3, color = "#151613", linewidth = 0.1, alpha=0.4)+
  annotate("segment", x = 0.65, xend = 1, y = 100.5, yend = 100.5, color = "#151613", linewidth = 0.1, alpha=0.4)

  
pFinal


## final
ggsave("week6.png", width = 7.8, height = 9.9, units = "in") 



