setwd("~/Assessment-4/Assessment-4-Reproducible-Data-Analysis-Project")

team_stats <- read.csv("data/processed_data/team_statistics.csv")

ggplot(data = team_stats, aes(x = ASTTOVR, y = (PTS/G))) +
  geom_point(colour = "black") +
  geom_smooth(method= "lm", colour = "red") +
  labs(title = "Relationship between ASTTOVR and Points per game",
       x = "Assist to Turnover Ratio",
       y = "Points scored in a game")
  
