#######################################################################
#Purpose: Exploring astronaut database: 1961 - 2019 - Tidytuesday 13/7/2020
#Source: [https://data.mendeley.com/datasets/86tsnnbv2w/1]
#Disclaimer: Data consist of information of astronauts before 15/1/2020.
#Author: LNN
#Date: 15/7/2020
#**********************************************************************

rm(list = ls(all = TRUE))

#package load
suppressMessages({
  library(extrafont)
  library(fcuk)
  library(ggthemes)
  library(tidyverse)
  library(ggdark)
})

#get data
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')
head(astronauts, n = 10)
tail(astronauts, 10)
class(astronauts)

astronauts %>%
glimpse() %>%
  names()

#nationality versus total sum of mission hours
#Ggplot as shown by 
#[https://cedricscherer.netlify.app/2019/05/17/the-evolution-of-a-ggplot-ep.-1/#text]

p1 <- ggplot(astronauts, aes(x = as.factor(nationality), y  = total_hrs_sum, color = nationality)) +
  #to flip the coordinates
  coord_flip() + 
  geom_point(alpha = 0)+
  scale_y_continuous(limits = c(0, 20500), expand = c(0.005, 0.005))+
  labs(x = NULL, y = "Total hours duration of all missions in hours",
       caption = "Data Source: https://data.mendeley.com/datasets/86tsnnbv2w/")

#Create the refrence line; average of all missions(hours) for all nationalities
all_nationalities_avg <- astronauts %>%
  summarise(meanval = mean(total_hrs_sum, na.rm = T)) %>%
  pull(meanval)

#set seed so that, for reproducible points each time we run geom_jitter() 
set.seed(20200715)

p2 <- (p1 + geom_jitter(size = 1.2, alpha = 0.25, width = 0.2) +
         #add the a summary statistic - mean of hours of all missions (different size)
  stat_summary(fun = mean, geom = "point", size = 5) +
    
    #add reference line: the average of all missions(hours) for all nationalities(1961-2019)
  geom_hline(aes(yintercept = all_nationalities_avg), color = "purple", size = 0.6)+
    
    #text - the average of all mission(hours) for all nationalities (1961-2019)
    annotate("text", x = 38, y =6500, family = "Calibri Light",
           size = 5, color = "white", lineheight = 0.6,
           label = glue::glue("Average duration of all missions(hours):
                              \n{round(all_nationalities_avg, 2)}" )))

# create the arrow to point the text to the reference line 
#by providing the start-points and endpoints of the arrows when calling geom_curve()

dat_arrow <- tibble(x1 = 37, x2 =38, y1 = 5500,
                 y2 = all_nationalities_avg)

p2+ geom_curve(data = dat_arrow, aes(x = x1, y = y1, 
                                 xend = x2, yend = y2),
               arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
               color = "white", curvature = -0.2) +
  dark_theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(family = "Calibri Light", size = rel(1.2), 
                                  hjust = 0.5),
        axis.text = element_text(family = "Calibri Light", size = rel(1.0)),
        axis.line.y = element_line(colour = "white", size = 0.5),
        axis.line.x = element_line(colour = "white", size = 0.5))