# *************************************************************************** #
# TidyTuesday Contribution
# Week 18: National Science Foundation Grant Terminations under the Trump Administration
# Author: Njoki Njuki
# Date: 06.05.2025
# *************************************************************************** #

# Load libraries ----
pacman::p_load(
  here, # For setting directory
  rio,  # For importing data
  tidyverse, # For exploration and data manipulation
  tidylog, # for tidyverse functions documentation
  ggimage, # For adding Pokémon images
  showtext, # For cool custom fonts 
  ggtext, # For fancy text formatting
  emo, # Emoji handling
  treemapify,
  networkD3
)


# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-05-06')

nsf_terminations <- tuesdata$nsf_terminations

# Treemap: Grants by Directorate and Division ----
# Aggregate data
domain_summary <- nsf_terminations %>%
  count(directorate, division, name = "grant_count") |> 
  drop_na() |> 
  mutate(directorate = as.factor(directorate),) |> 
  mutate(directorate = fct_recode(directorate,
                                  "Social, Behavioral and Economic Sciences" = "\"Social, Behavioral and Economic Sciences\"",
                                  "Technology, Innovation and Partnerships" = "\"Technology, Innovation and Partnerships\""))


(nsf_terminations_treemap <- ggplot(domain_summary, aes(
  area = grant_count,
  fill = directorate,
  label = ifelse(grant_count > 10, paste(division, "\n(", grant_count, ")"), ""), # Display text for boxes with more than 10 grants,
  subgroup = directorate
)) +
    geom_treemap(color = "white") +
    geom_treemap_subgroup_border(color = "#696969", size = 0.5) +
    geom_treemap_subgroup_text(
      place = "top",
      grow = FALSE,
      alpha = 0.9,
      colour = "#2C3E50",
      fontface = "bold",
      size = 16  # Reduce size for subgroup text
    ) +
    geom_treemap_text(
      colour = "#121212",  # Use black for better contrast
      place = "centre",
      grow = FALSE,
      size = 15,  # Reduce size for main text
      reflow = TRUE,
      fontface = "italic"
    ) +
    # scale_fill_brewer(palette = 5) + # Use the Viridis palette
    labs(
      title = "<span style='color:#8B1C62;'> NSF Grant Terminations by Directorate and Division</span>",
      fill = "NSF Directorate",
      caption = "#TidyTuesday \nWk 18: National Science Foundation Grant Terminations under the Trump Administration\n @njokinjuki"
    ) +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_markdown(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 10)),
      legend.position = "none",
      legend.title = element_text(face = "bold")
    )
)

ggsave(plot = nsf_terminations_treemap, 
       here::here("Plots", "Wk18_nsf_terminations_treemap.png"), 
       width = 14, height = 10, dpi = 300,
       bg = "white")
