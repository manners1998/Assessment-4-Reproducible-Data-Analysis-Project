library(tidyverse)
library(broom)


player_statistics <- read_csv("data/processed_data/player_statistics.csv")
team_statistics <- read_csv("data/processed_data/team_statistics.csv")

# Now the raw data has been processed, further analysis of the players data can be conducted. In this section the players positions will be broken down and key statistics per position will be analysed.

##Point Guards (PG)

point_guards_data <- player_statistics %>%
  select(Player:G,
         MP,
         FGA,
         FTA,
         AST, 
         TOV, 
         PTS,
         PTS_per_game,
         Salary) %>% 
  filter(Pos == "PG") %>% 
  mutate(ASTTOVR = (AST/TOV)) %>%  #Creating a Assist to turnover variable. This will give a good understanding of how well the PG is able to get the ball to teamates without turning it over. 
  mutate(TSA = (FGA + (0.44 *FTA)))  #Calculating True shooting attempts to then calculate True shooting percentage 

point_guards_data <- point_guards_data %>% #Calculating True shooting percentage 
  mutate(TSp = (PTS/(2 * TSA)))

point_guards_data <- point_guards_data[is.finite(point_guards_data$ASTTOVR),]  # removes any data that contains an infinite value. e.g. Scott Machado

point_guards_data <- point_guards_data %>%
  mutate(mean_ASTTOVR = mean(ASTTOVR))

point_guards_data <- point_guards_data %>%
  mutate(ASTTOVR_rating = if_else(condition = ASTTOVR < mean_ASTTOVR, 
                           true = "below average", false = "above average"))
point_guards_data <- point_guards_data %>%
  mutate_at(vars(PTS_per_game, ASTTOVR, TSA, TSp, mean_ASTTOVR), funs(round(., 3)))

## Shooting Guard (SG)

shooting_guard_data <- player_statistics %>%
  select(Player:G,
         MP,
         FG: X2Pp,
         FT:FTp,
         AST,
         ORB:TRB,
         PTS, 
         Salary) %>%
  filter(Pos %in% c("SG", "SF, SG")) %>%
 mutate(PTS_per_game = (PTS/G), FT_per_game = FT/G)
shooting_guard_data <- shooting_guard_data %>%
  mutate_at(vars(FGp, X2Pp,X3Pp, FTp, PTS_per_game,FT_per_game), funs(round(., 3)))

## Small Forward (SF)

small_forward_data <- player_statistics %>%
  select(Player:G,
         MP,
         FG: X2Pp,
         FT:FTp,
         AST,
         ORB:TRB,
         PTS, 
         Salary) %>%
         filter(Pos %in% c("SF", "SF, SG", "PF, SF")) %>%
  mutate(PTS_per_game = (PTS/G), 
         AST_per_game = (AST/G),)
small_forward_data <- small_forward_data %>%
  mutate_at(vars(FGp, X2Pp,X3Pp, FTp, PTS_per_game,AST_per_game), funs(round(., 3)))

## Power Forward (PF)

power_forward_data <- player_statistics %>%
  select(Player:G, 
         MP,
         FG:FGp,
         X2P:FTp,
         ORB:TRB, 
         PTS,
         PTS_per_game, 
         Salary) %>%
  filter(Pos %in% c("PF", "PF, SF", "PF, SG", "C,PF")) %>%
  mutate(TRB_per_game = TRB/G, 
         ORB_per_game = ORB/G)

power_forward_data <- power_forward_data %>%
mutate_at(vars(FGp,PTS_per_game, X2Pp, FTp, TRB_per_game,ORB_per_game), funs(round(., 3)))


## Centre (C)

centre_data <- player_statistics %>%
  select(Player:G,
         MP:FGp,
         X2P:FTp,
         ORB:TRB, 
         BLK, 
         PTS,
         PTS_per_game, 
         Salary) %>%
  filter(Pos %in% c("C", "C, PF")) %>%
  mutate(TRB_per_game = TRB/G, 
         ORB_per_game = ORB/G, 
         BLK_per_game = BLK/G)
centre_data <- centre_data %>% 
  mutate_at(vars(FGp, X2Pp,eFGp,FTp, PTS_per_game, TRB_per_game,ORB_per_game, BLK_per_game), funs(round(., 3)))



write_csv(point_guards_data, file = "data/processed_data/point_guard_data.csv")
write_csv(shooting_guard_data, file = "data/processed_data/shooting_guard_data.csv")
write_csv(small_forward_data, file = "data/processed_data/small_forward_data.csv")
write_csv(power_forward_data, file = "data/processed_data/power_forward_data.csv")
write_csv(centre_data, file = "data/processed_data/centre_data.csv")
