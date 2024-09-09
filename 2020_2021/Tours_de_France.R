#*************************************************************************
#Purpose: Has the average speed of winners increased or decreased over time?
#Data: Tour de France(https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-04-07)
#Date: 11/4/2020
#Author: LNN
#Acknowledge: Alastair Rushworth(https://alastairrushworth.github.io/Visualising-Tour-de-France-data-in-R/)
#*************************************************************************

rm(list = ls())

windowsFonts()
font_import()
loadfonts(device = "win")

#loading necessary packages
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(fcuk)
library(extrafont)



#get the data
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

head(tdf_winners)

#structure of the data
glimpse(tdf_winners)

#winners average speed over time by nationality
tdf_winners %>%
  group_by(nationality)%>%
  mutate(speed = distance / time_overall) %>%
  select(winner_team,start_date,edition,speed) %>%
  arrange(desc(speed)) %>%
print(n = 10)

#average speed per winner over time
theme_set(theme_tufte())

ggplot(tdf_winners, aes(x = start_date, y = (speed = distance / time_overall),
                        color = edition)) +
  geom_point(na.rm = TRUE) +
  geom_label_repel(data = tdf_winners %>% sample_n(20),
                   aes(label = winner_name), size = 3.0, nudge_y = 4,
                   na.rm = TRUE, segment.alpha = 0.5) +
labs(x = "Edition Start Date", y = "Average Speed (Km/h)",
     title = "Average Speed of the Winners Over Time") +
  theme(plot.title = element_text(family = "Times New Roman", size = rel(2),
                                  hjust = 0.5),
        axis.title.x = element_text(family = "Times New Roman", size = rel(1),
                                    hjust = 0.5),
  axis.title.y = element_text(family = "Times New Roman", size = rel(1),
                              hjust = 0.5), legend.background = element_blank(),
  axis.line = element_line(colour = "black", size = 0.5))