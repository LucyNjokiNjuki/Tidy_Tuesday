# *************************************************************************** #
# TidyTuesday Contribution
# Week 12: Text Data From Amazon's Annual Reports
# Author: Njoki Njuki
# Date: 29.03.2025
# *************************************************************************** #


# Load libraries ----
pacman::p_load(
  here, # For setting directory
  rio,  # For importing data
  ggplot2, # For exploration and data manipulation
  dplyr,
  tidylog, # for tidyverse functions documentation
  gganimate,
  ggwordcloud,
  stringr,
  RColorBrewer
)

# Data Dictionary ----
# Useful to refer to it, to understand the variables well
amazon_report_data_dict <- import(here("Data Dictionary", "wk_12_amazon_annual_report.xlsx")) |> 
  # Remove leading and trailing white spaces from each column
  mutate(across(everything(), ~ trimws(.)))

# Import Data ----
tuesdata <- tidytuesdayR::tt_load('2025-03-25')

report_words_clean <- tuesdata$report_words_clean

# Top 20 frequent words used ----
word_freqs <- report_words_clean %>%
  filter(year %in% c(2005:2010)) |> 
  group_by(year, word) %>%
  summarise(freq = n(), .groups = "drop") %>%
  arrange(year, desc(freq)) %>%
  mutate(word = str_replace_all(word, "<[^>]+>", "")) %>%  # Remove HTML tags
  filter(!str_detect(word, "http|www|href|script|link")) %>%  # Remove problematic words
  drop_na() %>%
  group_by(year) %>%
  slice_max(order_by = freq, n = 20)  # Keep top 20 words per year

# Define colors
word_colors <- colorRampPalette(brewer.pal(9, "Set1"))(100)

# Create base plot
p <- ggplot(word_freqs, aes(label = word, size = freq, color = freq)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +  
  scale_size_area(max_size = 25) +  
  scale_color_gradientn(colors = word_colors) +  
  theme_minimal(base_size = 20) +  
  theme(
    plot.background = element_rect(fill = "black", color = "black"),  
    panel.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(size = 14, face = "bold", color = "white", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(title = 'Top 20 Words Used in Amazon Reports (2005-2010) - Year: {closest_state}',
       caption = "#TidyTuesday Wk 12: Amazon's Annual Report; \n @njokinjuki")

# Animate the plot
anim <- p + transition_states(year, transition_length = 2, state_length = 1) +
  enter_fade() + exit_fade()

# Save and display animation
animate(anim, width = 600, height = 400, fps = 15, duration = 10, renderer = gifski_renderer(here("Plots", "amazon_report.gif")))

# Display animation
anim
