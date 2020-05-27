library(tidyverse)
library(lubridate)
library(gganimate)
library(transformr)
library(gifski)

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

teamsandscores

team_game_data <- game_data %>% 
  separate(result, into = c('team1', 'team1_score', NA, 'team2', 'team2_score', 
                            NA), 
           sep = '\\(|\\)|\\svs\\s', convert = T)

tm <- "ATLAS"

tm_data <- game_data %>% filter(str_detect(result, tm)) %>% 
  mutate(!!paste0(str_to_lower(tm), '_score') := as.integer(str_extract(result, paste0('(?<=', tm, '\\()[:digit:]+(?=\\))'))),
         opp = str_to_title(str_extract(result, paste0('\\b(?!', tm,'|vs)[:alpha:]+\\b'))),
         opp_score = as.integer(str_extract(result, paste0('(?<=', str_to_upper(opp), '\\()[:digit:]+(?=\\))'))),
         score_diff = !!paste0(str_to_lower(tm), '_score') - opp_score) %>%
  select(paste0(str_to_lower(tm), '_score'), opp, opp_score, week, score_diff, location, field_name) %>% 
  arrange(opp, week)

tm_data2 <- game_data %>% filter(str_detect(result, tm)) %>% 
  mutate(score = as.integer(str_extract(result, paste0('(?<=', tm, '\\()[:digit:]+(?=\\))'))),
         opp = str_to_title(str_extract(result, paste0('\\b(?!', tm,'|vs)[:alpha:]+\\b'))),
         opp_score = as.integer(str_extract(result, paste0('(?<=', str_to_upper(opp), '\\()[:digit:]+(?=\\))'))),
         score_diff = score - opp_score) %>%
  select(score, opp, opp_score, week, score_diff, location, field_name) %>% 
  arrange(opp, week)

#Team select

team <- "ATLAS"

#ATLAS Tibble
     
teams_results <- team_game_data %>% 
  filter(team1 == team | team2 == team) %>% 
  mutate(opp = if_else(team1 == team, team2, team1), 
         teamScore = if_else(team1 == team, team1_score, team2_score),
         oppScore = if_else(team1 == team, team2_score, team1_score),
         teamW = if_else(teamScore > oppScore, "1", "0"),
         week = as.integer(week)) %>% 
  select(opp, oppScore, teamScore, teamW, week, location) %>% 
  arrange(week) %>% 
  mutate(scoreDiff = teamScore - oppScore,
         opp = str_to_title(opp))

#ATLAS Plot

ggplot(tm_data2, aes(week, score_diff,  col = opp)) + 
  geom_point(size = 2) +
  scale_color_manual("Opponent", values = color_map) +
  theme_light() +
  geom_hline(yintercept = 0, size = .25) +
  geom_line(linetype = "twodash") +
  annotate(geom = "text", x = 1, y = .25, label = "W", col = "darkgreen")  +
  annotate(geom = "text", x = 1, y = -.25, label = "L", col = "darkred" )  +
  scale_x_continuous(breaks = c(1:10), 
                     minor_breaks = c(1,3,5,7,9),
                     limits = c(1,10)) +
  labs(x = "Week", y = "Score Differential", title = 'Score Differential Through 2019 Regular Season: Atlas')

#ATLAS Gif

anim <- ggplot(Atlas, aes(week, scoreDiff,  col = opp)) + 
  geom_point(size = 2) +
  scale_color_manual("Opponent", 
                     values = color_map) +
  theme_linedraw() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, 
             size = .25) +
  geom_line(linetype = "twodash") +
  annotate(geom = "text", 
           x = 1, y = .25, 
           label = "W", 
           col = "darkgreen")  +
  annotate(geom = "text", 
           x = 1, y = -.25, 
           label = "L", 
           col = "darkred" )  +
  scale_x_continuous(breaks = c(1:10), 
                     minor_breaks = c(1,3,5,7,9),
                     limits = c(1,10)) +
  labs(x = "Week", 
       y = "Score Differential") + 
  transition_states(opp,
                    transition_length = 1,
                    state_length = 2) +
  enter_fade() +
  exit_fade() +
  ggtitle('Score Differential Through 2019 Regular Season: Atlas vs {closest_state}')

animate(anim, duration = 15)
