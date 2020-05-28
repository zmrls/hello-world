library(tidyverse)
library(rvest)
library(stringr)
library(purrr)

#--------------------- Team colors for the pots ------------------------------#

teamColors <- tibble(opp = c("Archers", "Atlas", "Chaos", "Chrome", 
                             "Redwoods", "Whipsnakes"),
                     col = c("#0c2f59", "#009aba", "#be2c49", "#e471a5", 
                             "#005840", "#55d2bd"))

color_map <- set_names(teamColors$col, teamColors$opp)

#--------------------- Grabbing data from files -------------------------------#

# Grabbing the files from the game data folder

file_path <- '~/hello-world/PLL/data/gamedata/'

file_path %>% list.files() -> filenames

# Function to pull the tables from the html 

filenames %>% str_sort(numeric = T) %>% 
  purrr::map(function(file_name){
    read_html(paste0(file_path, file_name)) %>% 
      html_nodes('table') %>% 
      html_table
  }) -> stats