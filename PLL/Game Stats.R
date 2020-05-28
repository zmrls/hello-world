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


#------------------------------------------------------------------------------#

gmstats <- map(stats, 1) %>% map(~rename(.,team=1)) %>% bind_rows() %>% filter(team == 'ATLAS') %>% 
  mutate(wk = row_number(),
         OT = as.numeric(na_if(OT,"-")),
         team = str_to_title(team)) %>%   
  pivot_longer(2:7, names_to = 'quarter', values_to = 'score') %>%
  mutate(quarter = factor(quarter, levels = c('Q1', 'Q2', 'Q3', 'Q4', 'OT', 'F'))) %>% 
  drop_na() -> boxscore

gmstats

map(stats, 1)

stats <- stats[[1]]

stats[[1]] %>% rename(team = 1) %>% 
  mutate(OT = as.numeric(na_if(OT, "-")),
         team = str_to_title(team)) %>%   
  pivot_longer(2:7, names_to = 'quarter', values_to = 'score') %>%
  mutate(quarter = factor(quarter, levels = c('Q1', 'Q2', 'Q3', 'Q4', 'OT', 'F'))) %>% 
  drop_na() -> boxscore
  
ggplot(boxscore, aes(quarter, score, fill = factor(wk))) +
  geom_col(position = 'dodge', width = .5) +
  theme_light() +
  labs(x = NULL,
       y = 'Points',
       fill = 'Team') +
  scale_y_continuous(breaks = 0:max(boxscore$score),
                     minor_breaks = NULL) +
  geom_errorbar(aes(ymin = 10, ymax = 10))


stats[[2]] %>% 
  rename(team = 1) %>%
  mutate_at(vars(contains('%')), ~round(as.numeric(str_replace(., pattern = '%', replacement = "")) / 100, digits = 2)) %>% 
  mutate(team = str_to_title(team)) %>% 
  pivot_longer(2:11, names_to = 'stat', values_to = 'value') -> gmstats
   
ggplot(gmstats, aes(stat, value, fill = team)) +
  geom_col(position = 'dodge', width = .5) +
  theme_light() +
  labs(x = NULL,
       y = 'Value',
       fill = 'Team') +
  scale_y_continuous(breaks = 0:max(gmstats$value),
                     minor_breaks = NULL) +
  scale_fill_manual("Team", values = color_map)

stats[[3]] %>% separate(FO, c('FOW', 'FOTot'), sep = '-', convert = T) %>% 
  mutate(FOL = FOTot - FOW)
