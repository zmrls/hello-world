library(tidyverse)
library(rvest)
library(stringr)
library(purrr)

teamColors <- tibble(opp = c("Archers", "Atlas", "Chaos", "Chrome", 
                             "Redwoods", "Whipsnakes"),
                     col = c("#0c2f59", "#009aba", "#be2c49", "#e471a5", 
                             "#005840", "#55d2bd"))


color_map <- set_names(teamColors$col, teamColors$opp)

statstest <- read_html('~/hello-world/PLL/data/game_data/2019.08.24 - Atlas vs Chaos.html')

teams <- html_nodes(statstest, 'h1') %>% html_text()

box_score <- html_nodes(statstest, 'table')[[1]] %>% 
  html_table() %>% 
  rename(team = 1) %>% 
  mutate(OT = as.numeric(na_if(OT, "-")),
         team = str_to_title(team)) %>%   
  pivot_longer(2:7, names_to = 'quarter', values_to = 'score') %>% 
  drop_na()

box_score$quarter <- factor(box_score$quarter, levels = c('Q1', 'Q2', 'Q3', 'Q4', 'OT', 'F'))

box_score %>% ggplot(aes(quarter, score, fill = team)) +
  geom_col(position = 'dodge', width = .5) +
  theme_light() +
  labs(x = NULL,
       y = 'Points',
       fill = 'Team') +
  scale_y_continuous(breaks = 0:max(box_score$score),
                     minor_breaks = NULL) +
  scale_fill_manual("Team", values = color_map)
  
team_stats <- html_nodes(statstest, 'table')[[2]] %>% 
  html_table() %>% 
  rename(team = 1)%>%
  mutate(`2Pt. Sh. %` = as.numeric(str_replace(`2Pt. Sh. %`, pattern = '%', replacement = "")) / 100)
  pivot_longer(2:10, names_to = 'stat', values_to = 'value')
team_stats
  
fo <- html_nodes(statstest, 'table')[[3]] %>% 
  html_table()
fo

atk <- html_nodes(statstest, 'table')[[4]] %>% 
  html_table()
atk

mid <- html_nodes(statstest, 'table')[[5]] %>% 
  html_table()

def <- html_nodes(statstest, 'table')[[4]] %>% 
  html_table()

library(purrr)

file_path <- '~/hello-world/PLL/data/game_data/'

file_path %>% list.files() -> filenames

filenames %>%
  purrr::map(function(file_name){
    read_html(paste0(file_path, file_name)) %>% 
                html_nodes('table') %>% 
                html_table
  }) -> stats

