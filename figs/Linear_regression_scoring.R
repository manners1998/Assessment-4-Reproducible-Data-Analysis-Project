install.packages("SportsAnalytics")
install.packages("plotly")
library(tidyverse)
library(broom)
library(SportsAnalytics)
library(plotly)

player_statistics <- read.csv("data/processed_data/player_statistics.csv")

scoring <- player_statistics %>%
  select(Player:G, FG, FGp, X2P, X2Pp, X3P, X3Pp, PTS_per_game) %>%
  mutate(FG_per_game = (FG/G),
         X3P_per_game = (X3P/G),
         X2P_per_game = (X2P/G), 
         PTS_per_game) %>%
  mutate_at(vars(FG_per_game, X2P_per_game, X3P_per_game), funs(round(., 3)))

scoring_data <- select(scoring,
                       PTS_per_game, 
                       X2P_per_game, 
                       X3P_per_game)

pairs(scoring_data, 
      labels = c("Points per Game", "2 Point Goals per game", "3 Point Goals per game"), 
      main = "Multilinear Regression of Points score per game")

fit <- lm(PTS_per_game ~ X2P_per_game + X3P_per_game, data = scoring_data)
broom::tidy(fit, conf.int = TRUE) #Test for multi-Linear Regression

car::avPlots(fit) #Test for Linerarity

car::vif(fit)

#Linear Regression - Scoring
## The findings show that the X2P_per_game is the more effective mode of scoring 


gg <- ggplot(scoring, aes(x = X2P_per_game, y = PTS_per_game, label = Player)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "red") +
  labs(title = "Relationship between 2pts per game and Total points per game",
       x= "2pts per Game", 
       y= "Total points per game")

ggplotly(gg)
          
std_res <- rstandard(fit)
points <- 1:length(std_res)

ggplot(data= NULL, aes(x = points, y = std_res)) +
  geom_point()+
  ylim(c(-5,5)) +
  geom_smooth(method = "lm", colour = "red")

res_labels <- if_else(abs(std_res) >= 2.5, paste(points), "")

scoringplot + geom_text(aes(label = res_labels), nudge_x = 0.5, nudge_y = 0.5) # visulasing the outliters

hats <- hatvalues(fit)
hat_labels <- if_else(hats >= 0.025, paste(points), "")

ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point()+
  geom_text(aes(label = hat_labels), nudge_x = 20) #Leverage Points

scoringplot + 
  geom_text(aes(label = hat_labels), nudge_x = 0.05)

cook <- cooks.distance(fit)
cook_labels <- if_else(cook >= 0.15, paste(points), "")

ggplot(data = NULL, aes(x = points, y = cook)) + 
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_y = 0.05)

scoringplot +
  geom_text(aes(label = cook_labels), nudge_y = 1)

res <- residuals(fit)
fitted <- predict(fit)

ggplot(data = NULL, aes(x = fitted, y = res)) + 
  geom_point(colour = "black") + 
  geom_smooth(se = FALSE, colour = "red") 