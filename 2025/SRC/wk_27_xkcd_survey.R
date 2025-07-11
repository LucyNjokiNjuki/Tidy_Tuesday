# *************************************************************************** #
# TidyTuesday Contribution
# Week 25: The xkcd Color Survey Results
# Author: Njoki Njuki
# Date: 11.07.2025
# *************************************************************************** #

# Load libraries ----
pacman::p_load(
  here, # For setting directory
  sysfonts, # For system fonts
  showtext, # For selecting the 
  tidyverse, # For exploration and data manipulation
  ggfx, # For shadows on the plot
  tidylog # For tidyverse functions documentation
)

# Import data ----
tuesdata <- tidytuesdayR::tt_load(2025, week = 27)

color_ranks <- tuesdata$color_ranks

# Add the Patrick Hand font from Google Fonts ----
font_add_google("Patrick Hand", "patrick")
showtext_auto()

# Prepare the data ----
top100_colors <- color_ranks %>%
  filter(rank <= 100) %>%
  arrange(rank) %>%
  mutate(
    id = row_number(),
    row = (id - 1) %/% 5,
    col = (id - 1) %% 5,
    # Calculate brightness for contrast
    brightness = hex2RGB(hex)@coords %>%
      as.data.frame() %>%
      transmute(bright = 0.299 * R + 0.587 * G + 0.114 * B) %>%
      pull(),
    text_color = ifelse(brightness > 0.7, "black", "white"),
    label = paste0(color, "\n", hex)
  )

# Create the plot ----
(wk27_plot <- ggplot(top100_colors, aes(x = col, y = -row, fill = hex)) +
  with_shadow(
    geom_tile(color = "white", width = 0.95, height = 0.95),
    sigma = 5, x_offset = 1, y_offset = 1, colour = "black"
  ) +
  geom_text(aes(label = label, color = text_color), size = 2.5, lineheight = 0.9, show.legend = FALSE) +
  labs(
    title = "Top 100 Ranked Colours",
    subtitle = "From the XKCD colour survey of over 200,000 people",
    caption = "Wk 27 #TidyTuesday \n The xkcd Color Survey Results \n Data: XKCD Colour Survey"
    ) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void(
    base_family = "patrick"
    ) +
  theme(
    plot.background = element_rect(fill = "#fef6e4", color = NA),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#172c66"),
    plot.caption = element_text(size = 12, hjust = 0.5, color = "#8d99ae"),
    plot.title = element_text(
      color = "#001858", size = 26, face = "bold", hjust = 0.5
    )
  ))

ggsave(plot = wk27_plot, 
       here::here("Plots", "Wk27_xkcd_color_survey.png"), 
       width = 4, height = 2.5, 
       bg = "white")

