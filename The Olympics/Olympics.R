#************************************************************
#Project: EDA - Olympics Games 2021
#Author: Njoki Njuki L
#Date: 30/7/2021
#************************************************************

#clear env
rm(list = ls())

#installing ggpattern packages
#remotes::install_github("coolbutuseless/ggpattern")

#packages load
suppressPackageStartupMessages({
  library(tidyverse)
  library(extrafont)
  library(png)
  library(patchwork)
  library(ggpattern)
  library(viridis)
})

#load data
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')
head(olympics)


#structure of the data
glimpse(olympics)

#for ke, which sex dominates olympics games?

#filter for only ke
olympics$team <- as.factor(olympics$team)

ke_olympics <- olympics %>%
 filter(team == "Kenya")

ke_olympics_sex <- ke_olympics %>%
  group_by(sex) %>%
  summarise(Count = n()) %>%
  ungroup()

p1 <- ggplot(ke_olympics_sex, aes(x = sex, y = Count, fill = sex))+
  geom_bar_pattern(stat = "identity", aes(pattern = sex),
                   size = 0.4) +
  geom_text(aes(label = Count, y = Count, x = sex), size = 4, hjust = 0.5, vjust = -0.25) +
  scale_fill_viridis(discrete = T)+
  labs(title = "Gender Distribution in The Olympics(Kenya)",
        x = "Sex",
       y = "Count", caption = "The Olympics!!! Plot by NjokiNjuki") +
  theme_minimal() +
  annotate("text", x = 1, y = 700, label = "Clearly male partcipation dominates!!!")+
  theme(plot.title = element_text(family = "Calibri Light", face = "bold", size = 12,
                                  hjust = 0.5),
        legend.position = "none",
        axis.title = element_text(family = "Calibri Light", size = 12,
                                  hjust = 0.5))
#loading image
#normalizePath('kenya_flag.jpg')
#ke_flag <- readPNG("./supplement_materials/kenya_flag.png", native = TRUE)

#p2 <- p1+ inset_element(p = ke_flag,
                        #left = 0.5,
                        #bottom = -0.55,
                        #right = 0.95,
                        #top = 1.95)