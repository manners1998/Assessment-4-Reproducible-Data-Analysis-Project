library(tidyverse)
library(broom)

player_statistics <- read.csv("data/processed_data/player_statistics.csv")

Reb_to_score <- player_statistics %>%
  select(Player:G, FG, FGp, X2P, X2Pp, X3P, X3Pp, ORB:TRB, PTS) %>%
  mutate(ORB_per_game = (ORB/G),
        X2P_per_game = (X2P/G),
        PTS_per_game = (PTS/G)) 

REB_MLR_dat <- select(Reb_to_score,
                     PTS_per_game, 
                     X2P_per_game, 
                     ORB_per_game)

pairs(REB_MLR_dat, 
      labels = c("Points per game", "2 points per game", "Rebounds per game"), 
      main = "Frequency of 2 points scored from rebounds")
      
Reb_fit <- lm(X2P_per_game ~ ORB_per_game, data =  Reb_to_score)
broom::tidy(Reb_fit, conf.int= TRUE)

ggplot(Reb_to_score, aes(x = X2P_per_game, y = ORB_per_game)) + 
  geom_point(colour = "black") +
  geom_smooth(method = "lm", colour = "red") +
  labs(title = "Relationship between 2-points per game and Offensive Rebounds per game",
       x = "2 Point shots made per game",
       y = "Offensive Rebounds per game")

Reb_std_res <- rstandard(Reb_fit)       #Step to detect outliers
Reb_points <- 1:length(Reb_std_res)
Reb_res_labels <- if_else(abs(Reb_std_res) >= 2.5, paste(Reb_points), "")

ggplot(data = NULL, aes(x = Reb_points, y = Reb_std_res)) +
  geom_point() +
  geom_text(aes(label = Reb_res_labels), nudge_x = 25) + ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3,3), colour = "red", linetype = "dashed")

Reb_plot +
 geom_text(aes(label = Reb_res_labels), nudge_x = 0.8)

Reb_hats <- hatvalues(Reb_fit)
Reb_hats_labels <- if_else(Reb_hats >= 0.02, paste(Reb_points), "")

ggplot(data = NULL, aes(x = Reb_points, y = Reb_hats)) +  #Checking for Leverage points
  geom_point() +
  geom_text(aes(label = Reb_hats_labels), nudge_y = 0.001)

Reb_plot +
  geom_text(aes(label = Reb_hats_labels), nudge_x = -0.8) #Checking for Leverage points

Reb_cook <- cooks.distance(Reb_fit)
Reb_cook_labels <- if_else(Reb_cook >= 0.025, paste(Reb_points), "") 

ggplot(data = NULL, aes(x = Reb_points, y = Reb_cook)) +  #Checking for Influence 
  geom_point() +
  geom_text(aes(label = Reb_cook_labels), nudge_y = 0.003)

Reb_plot +
  geom_text(aes(label = Reb_cook_labels), nudge_x = 0.5)

Reb_res <- residuals(Reb_fit)
Reb_fitted <- predict(Reb_fit)

ggplot(data = NULL, aes(x = Reb_fitted, y = Reb_res)) + 
  geom_point(colour = "black") +
  geom_smooth(se = FALSE, colour = "red")

# No sign of linerarity, therefore Homoscedascity is present
