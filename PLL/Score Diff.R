library(tidyverse)
library(lubridate)

#Raw Data

raw_data <- read_csv("PLL/data/gameScores2019.csv", 
                     col_types = cols(`2` = col_date(format = "%m/%d/%Y"), 
                                      `3` = col_integer()),
                     na = '-')

#Team colors

teamColors <- tibble(opp = c("Archers", "Atlas", "Chaos", "Chrome", 
                             "Redwoods", "Whipsnakes"),
                     col = c("#0c2f59", "#009aba", "#be2c49", "#e471a5", 
                             "#005840", "#55d2bd"))


color_map <- set_names(teamColors$col, teamColors$opp)

#Column names are all messed up. Vector to rename them.

cols = c("result", "date", "week", "field_name", "location")

game_data <- rename_all(raw_data, ~cols)

#Need to separate the "teamScore" col into teams and scores individually
tms <- str_to_upper(teamColors$opp)

for(tm in tms) {
  tm_data2 <- game_data %>% filter(str_detect(result, tm)) %>% 
    mutate(score = as.integer(str_extract(result, paste0('(?<=', tm, '\\()[:digit:]+(?=\\))'))),
           opp = str_to_title(str_extract(result, paste0('\\b(?!', tm,'|vs)[:alpha:]+\\b'))),
           opp_score = as.integer(str_extract(result, paste0('(?<=', str_to_upper(opp), '\\()[:digit:]+(?=\\))'))),
           score_diff = score - opp_score) %>%
    select(score, opp, opp_score, week, score_diff, location, field_name) %>% 
    arrange(opp, week)
  
  ggplot(tm_data2, aes(week, score_diff,  col = opp)) + 
    geom_point(size = 2) +
    scale_color_manual("Opponent", values = color_map) +
    theme_light() +
    geom_hline(yintercept = 0, size = .25) +
    geom_line(linetype = "twodash") +
    scale_x_continuous(breaks = c(1:10),
                       minor_breaks = NULL,
                       limits = c(1,10)) +
    scale_y_continuous(limits = c(-15, 15),
                       breaks = seq(-15, 15, 5),
                       minor_breaks = c(-15:15))+
    labs(x = "Week", y = "Score Differential", title = paste0(tm, ' Score Differential: Regular Season \'19'))
  
  ggsave(paste0(tm, '_sd_reg2019.png'), path = '~/hello-world/PLL/plots')
}

for(tm in tms) {
  tm_data2 <- game_data %>% filter(str_detect(result, tm)) %>% 
    mutate(score = as.integer(str_extract(result, paste0('(?<=', tm, '\\()[:digit:]+(?=\\))'))),
           opp = str_to_title(str_extract(result, paste0('\\b(?!', tm,'|vs)[:alpha:]+\\b'))),
           opp_score = as.integer(str_extract(result, paste0('(?<=', str_to_upper(opp), '\\()[:digit:]+(?=\\))'))),
           score_diff = score - opp_score) %>%
    select(score, opp, opp_score, week, score_diff, location, field_name) %>% 
    arrange(opp, week)
  
  ggplot(tm_data2, aes(week, score_diff)) + 
    geom_point(size = 2) +
    geom_hline(yintercept = 0, size = .25) +
    geom_line(linetype = "twodash") +
    theme_light() +
    scale_x_continuous(breaks = c(1:10),
                       minor_breaks = NULL,
                       limits = c(1,10)) +
    scale_y_continuous(limits = c(-15, 15),
                       minor_breaks = c(-15:15),
                       breaks = seq(-15, 15, 5))+
    labs(x = "Week", y = "Score Differential", title = paste0(tm, ' Score Differential: Regular Season \'19'))
  
  ggsave(paste0(tm, '_sd_reg2019.png'), path = '~/hello-world/PLL/plots')
}
