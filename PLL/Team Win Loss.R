library(tidyverse)
library(gganimate)
library(transformr)
library(gifski)

#Raw Data

scores <- read_csv("gameScores.csv")
scores

#Team colors

teamColors <- tibble(opp = c("Archers", "Atlas", "Chaos", "Chrome", "Redwoods", "Whipsnakes"),
                    col = c("#0c2f59", "#009aba", "#be2c49", "#e471a5", "#005840", "#55d2bd"))


color_map <- set_names(teamColors$col, teamColors$opp)

#Column names are all messed up. Vector to rename them.

cols = c("teamScore", "date", "week", "fieldName", "location")

scores <- rename(scores, "teamScore" = X1, "date" = `2`, "week" = `3`, "fieldName" = `4`, "location" = `5`)

#Need to separate the "teamScore" col into teams and scores individually

scores$teamScore <- gsub("[()]"," ", scores$teamScore)
scores$teamScore <- gsub("  vs ", " ", scores$teamScore)
scores$teamScore

#Tidy the data

tidyScores <- scores %>% separate(teamScore, c("team1", "score1", "team2", "score2"), sep = c(" ", " ", " "), convert = T)
tidyScores

#Team select

team <- "ATLAS"

#ATLAS Tibble
     
Atlas <- tidyScores %>% 
  filter(team1 == team | team2 == team) %>% 
  mutate(opp = if_else(team1 == team, team2, team1), 
         teamScore = if_else(team1 == team, score1, score2),
         oppScore = if_else(team1 == team, score2, score1),
         teamW = if_else(teamScore > oppScore, "1", "0"),
         week = as.integer(week)) %>% 
  select(opp, oppScore, teamScore, teamW, week, location) %>% 
  arrange(week) %>% 
  mutate(scoreDiff = teamScore - oppScore,
         opp = str_to_title(opp))

#ATLAS Plot

ggplot(Atlas, aes(week, scoreDiff,  col = opp)) + 
  geom_point(size = 2) +
  scale_color_manual("Opponent", values = color_map) +
  theme_linedraw() +
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
