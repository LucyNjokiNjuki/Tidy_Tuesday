#**************************************************************************
##TidyTuesday
#Purpose: Data Exploration using 
#Author: LNN
#Date: 19/2/2020
#************************************************************************

rm(list = ls())

#loading packages
library("tidyverse")
library("ggthemes")
library("extrafont")

#theme_setting
theme_set(theme_dark())
plot_theme <- function(){
      theme(
      text = element_text(family = "Source Sans Pro", size  = 12),
      plot.title =  element_text(family = "Source Sans Pro", size = rel(1.2),
                                 hjust = 0.5),
      axis.text.x =  element_text(family = "Source Sans Pro", size = rel(1.0),                           ),
      axis.text.y = element_text(family = "Source Sans Pro", size = rel(1.0)),
      
      axis.line = element_line(size = 0.5, colour = "black"),
      axis.title.y = element_text(hjust = 0.5),
      axis.title.x = element_text(hjust = 0.5),
      title = element_text(hjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
  )
}

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
head(food_consumption)


#converting necessary variables to factors
food_consumption$country <- as.factor(food_consumption$country)
food_consumption$food_category <- as.factor(food_consumption$food_category)

#Exploring the data
glimpse(food_consumption) #Obs = 1430;variables = 4

summary(food_consumption)

#co2_em
co2_em <- food_consumption %>%
  group_by(country, food_category) %>%
  summarise(meanval = round(mean(co2_emmission),2))
 co2_em

par(mfrow = c(2,2))

#kenya
co2_em_ke  = co2_em[co2_em$country == "Kenya",]
co2_em_ug  = co2_em[co2_em$country == "Uganda",]
co2_em_rw  = co2_em[co2_em$country == "Rwanda",]
co2_em_tz  = co2_em[co2_em$country == "Tanzania",]
co2_em_eth  = co2_em[co2_em$country == "Ethiopia",]

#Kenya
ggplot(co2_em_ke, aes(x = food_category, y = meanval, fill =food_category)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6)+
  geom_text(aes(label = paste(meanval, sep = "")), size  = 4,
            position = position_dodge(0.5),
            vjust = -0.25, hjust = 0.5)+
  labs(x = "Food Category", y = "Average", 
       title = "Distribution of Carbon(IV)Oxide Emission by Food Category in Kenya",
       subtitle = "") + 
  plot_theme()

#Uganda
ggplot(co2_em_ug, aes(x = food_category, y = meanval, fill =food_category)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6)+
  geom_text(aes(label = paste(meanval, sep = "")), size  = 4,
            position = position_dodge(0.5),
            vjust = -0.25, hjust = 0.5)+
  labs(x = "Food Category", y = "Average", 
       title = "Distribution of Carbon(IV)Oxide Emission by Food Category in Uganda",
       subtitle = "") + 
  plot_theme()

#Rwanda
ggplot(co2_em_rw, aes(x = food_category, y = meanval, fill =food_category)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6)+
  geom_text(aes(label = paste(meanval, sep = "")), size  = 4,
            position = position_dodge(0.5),
            vjust = -0.25, hjust = 0.5)+
  labs(x = "Food Category", y = "Average", 
       title = "Distribution of Carbon(IV)Oxide Emission by Food Category in Rwanda",
       subtitle = "") + 
  plot_theme()

#Tanzania
ggplot(co2_em_tz, aes(x = food_category, y = meanval, fill =food_category)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6)+
  geom_text(aes(label = paste(meanval, sep = "")), size  = 4,
            position = position_dodge(0.5),
            vjust = -0.25, hjust = 0.5)+
  labs(x = "Food Category", y = "Average", 
       title = "Distribution of Carbon(IV)Oxide Emission by Food Category in Tanzania",
       subtitle = "") + 
  plot_theme()

#Ethiopia
ggplot(co2_em_eth, aes(x = food_category, y = meanval, fill =food_category)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6)+
  geom_text(aes(label = paste(meanval, sep = "")), size  = 4,
            position = position_dodge(0.5),
            vjust = -0.25, hjust = 0.5)+
  labs(x = "Food Category", y = "Average", 
       title = "Distribution of Carbon(IV)Oxide Emission by Food Category in Ethiopia",
       subtitle = "") + 
  plot_theme()

