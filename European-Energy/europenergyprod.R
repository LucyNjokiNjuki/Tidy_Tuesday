#******************************************************************************
#Purpose: TidyTuesday - European energy 
#Date: 8/6/2020
#Author: LNN
#******************************************************************************

#clearing the evironment
rm(list = ls(all = TRUE))

#package load
suppressPackageStartupMessages({
  library(tidyverse)
  library(gganimate)
  library(ggdark)
  library(extrafont)
  library(fcuk)
  library(gifski)
  library(RColorBrewer)
})

#get the data
energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')

#explore data
energy_types %>%
  glimpse() %>%
  names

#merging columns
new_data <- energy_types %>%
  gather("year", "EnergyProduction", "2016", "2017", "2018")

#checcking missing values
sapply(new_data, function(x)sum(is.na(x)))

new_data$year <- as.integer(new_data$year)

#remove nas
new_data2 <- new_data %>%
  filter(country_name != "NA")

sapply(new_data2, function(x)sum(is.na(x)))

#reorder coutry name
new_data3 <- new_data2 %>%
  mutate(country_name = fct_reorder(country_name, desc(EnergyProduction), .fun='sum'))

#plot
file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)

setwd("D:\\NJUKI\\Personal Projects\\Training\\R Training\\Tidyverse_Tuesday_Challenges\\European-Energy")

p1 <- ggplot(new_data3, aes(y = EnergyProduction, x = country_name, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  labs(y = "Energy Production", x = "", title = "Energy Production by Country in the European Region",
       subtitle = "Year: {frame_time}") +
  transition_time(year) +
  ggdark::dark_theme_classic() +
  theme(axis.title = element_text(family = "Calibri Light", size = rel(1.2), 
                                  hjust = 0.5, face = "bold"),
        axis.text = element_text(family = "Calibri Light", size = rel(1.0)),
        axis.line.y = element_line(colour = "white", size = 0.5),
        axis.line.x = element_line(colour = "white", size = 0.5)) +
  scale_fill_brewer(palette = "Set1")

options(scipen = 999) #turn off the scientific numbers

#save the plot
animate(p1, renderer = gifski_renderer()) 

anim_save("EuropeEnergyProductionPlot.gif", animation=last_animation())