# *************************************************************************** #
# TidyTuesday Contribution
# Week 13: Pokemon
# Author: Njoki Njuki
# Date: 01.04.2025
# *************************************************************************** #

# intro ----
# This dataset is sourced from {pokemon} (CRAN | github), an R package which 
# provides Pokemon information in both English and Brazilian Portuguese.
# https://cran.r-project.org/web/packages/pokemon/index.html
# https://github.com/williamorim/pokemon

# Load libraries ----
pacman::p_load(
  here, # For setting directory
  rio,  # For importing data
  tidyverse, # For exploration and data manipulation
  tidylog, # for tidyverse functions documentation
  ggimage, # For adding Pokémon images
  showtext, # For cool custom fonts 
  ggtext, # For fancy text formatting
  emo # Emoji handling
)

# Data Dictionary
# Useful to refer to it, to understand the variables well
pokemon_data_dict <- import(here("Data Dictionary", "wk13_pokemon.xlsx")) |> 
  # Remove leading and trailing white spaces from each column
  mutate(across(everything(), ~ trimws(.)))

# Data ----
tuesdata <- tidytuesdayR::tt_load('2025-04-01')
pokemon_df <- tuesdata$pokemon_df


# Basic Exporation ----

# View structure
glimpse(pokemon_df)

# Summary statistics
summary(pokemon_df)

# Check missing values
colSums(is.na(pokemon_df))

# Add full image URL prefix
pokemon_df <- pokemon_df %>%
  mutate(url_icon = ifelse(is.na(url_icon), NA, paste0("https:", url_icon))) %>%
  drop_na(url_icon)  # Remove rows where url_icon is NA

# Top 10 Fastest Pokémon
top_fastest <- pokemon_df |> 
  arrange(desc(speed)) |> 
  select(pokemon, type_1, speed, url_icon) |> 
  head(10) |> 
  mutate(pokemon = as.factor(pokemon)) |> 
  mutate(pokemon = fct_recode(pokemon, Accelgor = "accelgor",
                              Aerodactyl = "aerodactyl",
                              Crobat = "crobat",
                              `Deoxys-normal` = "deoxys-normal",
                              Electrode = "electrode",
                              Jolteon = "jolteon",
                              Mewtwo = "mewtwo",
                              Ninjask = "ninjask",
                              Swellow = "swellow",
                              Talonflame = "talonflame"))

# Add a custom Pokémon font
showtext_auto()
font_add_google("Press Start 2P", "pokemon_font")


# Pokémon-themed colors
pokemon_colors <- c("electric" = "#FFD700", "fire" = "#FF4500", 
                    "water" = "#1E90FF", "psychic" = "#FF69B4",
                    "poison" = "#B23AEE", "normal" = "#A8A77A",
                    "bug" = "#A8B820")

# Create Pokémon theme plot
(wk13_plot <- ggplot(top_fastest, aes(x = reorder(pokemon, speed), y = speed, fill = type_1)) +
  geom_col(show.legend = TRUE) +  
  geom_text(aes(label = speed), hjust = -0.3, color = "white", size = 7, fontface = "bold") +  
  geom_image(aes(image = url_icon), size = 0.1, by = "width") +  
  coord_flip() +  
  scale_fill_manual(values = pokemon_colors,
                    name = "Type",
                    label = c("Bug", "Electric", "Fire", "Normal", "Poison", "Pyschic")) + 
  labs(title = "**Fastest Pokémon Ever!**",
       subtitle = "<span style='color:#FFD700;'>Can anyone outrun </span><span style='color:#A8A77A;'>Swellow?</span>",
       x = "Pokémon", y = "Speed") +
  theme_minimal(base_family = "pokemon_font") +  
  theme(
    text = element_text(color = "white"),
    plot.title = element_markdown(size = 20, face = "bold", hjust = 0.5, color = "#FFD700"), 
    plot.subtitle = element_markdown(size = 18, hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12, color = "white"),
    legend.position = "bottom",
    panel.background = element_rect(fill = "black", color = NA),  
    plot.background = element_rect(fill = "#222831", color = NA),
    panel.grid.major = element_line(size = 0.2, color = "gray40"),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank()
  ))

# save the plot
ggsave(plot = wk13_plot, 
       file = here::here("Plots", "Wk13_Pokemon.png"), 
       width = 5.5, height = 3.5)
