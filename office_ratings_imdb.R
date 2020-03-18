#*********************************************************************
#Purpose: Tidy Tuesday challenge (Office Ratings)
#Date: 18/3/2020
#Author: Lucy Njoki
#*********************************************************************

rm(list = ls())

#loading necessary libraries
library(tidyverse)
library(ggthemes)
library(extrafont)
library(fcuk)

#loading the data
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

head(office_ratings)

tail(office_ratings)

#checking the structure of the data
glimpse(office_ratings)

#converting factors
office_ratings$season <- as.factor(office_ratings$season)
office_ratings$episode <- as.factor(office_ratings$episode)
office_ratings$title <- as.factor(office_ratings$title)

#checking for any missing obs
sapply(office_ratings, function(x) sum(is.na(x)))

#theme settings
theme_set(theme_fivethirtyeight())

plottheme <- function(){
  theme(
    text = element_text(family = "Source Sans Pro", size =12),
    plot.title = element_text(family = "Source Sans Pro",
                              size = rel(1.2), hjust = 0.5),
    axis.title = element_text(hjust = 0.5),
    axis.title.x = element_text(family = "Source Sans Pro", size = rel(1.0),
                                hjust = 0.5),
    axis.title.y = element_text(family = "Source Sans Pro", size = rel(1.0),
                                hjust = 0.5),
    axis.line = element_line(colour = "black", size = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
    
  )
}

#Visualizing
seasons <- office_ratings %>%
  group_by(season) %>%
  summarise(Avg = round(mean(imdb_rating),2))
seasons  

ggplot(seasons, aes(x = season, y = Avg, fill = season)) +
  geom_bar(stat = "identity",
           position = position_dodge(), width = 0.6) +
  geom_text(aes(label = paste(Avg, sep = "")), size  =4,
            position = position_dodge(0.5),
            vjust = -0.25, hjust = 0.5)+
  labs(x = "Season", y = "Average", 
       title = "IDMB Rating by Season",
       subtitle = "") + 
  plottheme() 

#normality test
shapiro.test(office_ratings$imdb_rating)

#Checking if rate of idmb is statistically significant among seasons
fit <- aov(imdb_rating~season, data = office_ratings)
summary(fit)
