library(tidyverse)

teamStats <- tibble(team = c("Archers", "Atlas", "Chaos", "Chrome", "Redwoods", "Whipsnakes"),
                    col = c("#0c2f59", "#009aba", "#be2c49", "#e471a5", "#005840", "#55d2bd"),
                    g = c(105, 117, 118, 120, 112, 116),
                    a = c(65, 73, 67, 62, 61, 73),
                    sh = c(419, 403, 383, 416, 440, 457),
                    shPct = c(25, 29, 30.8, 28.8, 25.4, 25.3),
                    gb = c(282, 299, 294, 344, 274, 287),
                    foW = c(112, 167, 133, 142, 106, 133),
                    foTot = c(241, 273, 281, 281, 253, 257),
                    to = c(147, 144, 164, 154, 141, 137),
                    cto = c(55, 51, 64, 67, 67, 59),
                    sv = c(126, 140, 157, 127, 116, 133),
                    svPct = c(55.7, 53.4, 55.4, 50.3, 52.2, 54.9),
                    ga = c(105, 132, 129, 137, 116, 116),
)

color_map <- set_names(teamStats$col, teamStats$team)

ggplot(teamStats, aes(g, shPct, col = team)) +
  geom_point() +
  scale_color_manual("Teams", values = color_map) +
  theme_light()
