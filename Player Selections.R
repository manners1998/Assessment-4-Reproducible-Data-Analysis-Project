library(tidyverse)
library(broom)

setwd("~/Assessment-4/Assessment-4-Reproducible-Data-Analysis-Project")

SG <- read.csv("data/processed_data/shooting_guard_data.csv")
PG <- read.csv("data/processed_data/point_guard_data.csv")
SF <- read.csv("data/processed_data/small_forward_data.csv")
PF <- read.csv("data/processed_data/power_forward_data.csv")
C <- read.csv("data/processed_data/centre_data.csv")
Player <- read.csv("data/processed_data/player_statistics.csv")


ASTTOVR <- c(PG$ASTTOVR)
ASTTOVR_mean <- mean(ASTTOVR)
AST_mean <- mean(PG$AST)
TOV_mean <- mean(PG$TOV)

#Point Guard selection
PG <- PG %>%
  filter(ASTTOVR >= ASTTOVR_mean, AST >= AST_mean, TOV <= TOV_mean) %>%
  arrange(desc(ASTTOVR))

Monte_Morris <- PG %>%
  select(Player:G, AST: ASTTOVR) %>%
  filter(Player == "Monte Morris")

.........
#Shooting Guard selection
SG <- SG %>%
  select(Player:FTp, PTS, PTS_per_game, AST, ORB:TRB, Salary) %>%
  mutate(X2P_per_game = X2P/G, 
         X3P_per_game = X3P/G)

PTS_per_game_mean <- mean(SG$PTS_per_game)

SG <- SG %>%
  filter(PTS_per_game >= PTS_per_game_mean)

SG <- SG %>%
  arrange(desc(PTS_per_game)) %>%
  select(Player: G, FG:FTp,PTS, AST, TRB, PTS_per_game, Salary)

Devin_Booker <- SG %>%
  select(Player:Salary) %>%
  filter(Player == "Devin Booker")

......... 
#Small Forward Selection

SF <- SF %>% 
  mutate(ORBX2P = (ORB/X2P),
         ORBX2Pr = (ORBX2P- mean(ORBX2P))/ sd(ORBX2P)) %>%
  mutate_at(vars(ORBX2P, ORBX2Pr) , funs(round(., 3))) %>%
  filter(ORBX2Pr >= 0) %>%
  arrange(desc(ORBX2Pr))

SF <- SF %>%
  mutate(AST_per_game = (AST/G), 
         ASTPGr = (AST_per_game - mean(AST_per_game)) / sd(AST_per_game)) %>%
  mutate_at(vars(AST_per_game, ASTPGr), funs(round(., 3))) %>%
    filter(AST_per_game >= 0) %>%
    arrange(desc(AST_per_game))

Nicolas_Batum <- SF %>%
  select(Player:G, FG:FGp, X3P:X3Pp, X2P:X2Pp, AST, ORB:TRB, ORBX2P, Salary) %>%
  filter(Player == "Nicolas Batum")
  
..............
#Power Forward Selection

PF <- PF %>%
  mutate(ORBX2P = (ORB/X2P), 
         ORBX2Pr = (ORBX2P- mean(ORBX2P))/ sd(ORBX2P)) %>%
  mutate_at(vars(ORBX2Pr) , funs(round(., 3))) %>%
  filter(ORBX2Pr >= 0) %>%
  arrange(desc(ORBX2P))

PJ_Tucker <- PF %>%
  select(Player:G, X2P: X2Pp, ORB:TRB, ORBX2P, ORBX2Pr, Salary) %>%
  filter(Player == "PJ Tucker")

..............
#Center selection
C <- C %>%
  mutate(ORBX2P = (ORB/X2P), 
         ORBX2Pr = (ORBX2P- mean(ORBX2P))/ sd(ORBX2P), 
         BLKr = BLK - mean(BLK)/ sd(BLK)) %>%
  mutate_at(vars(ORBX2Pr) , funs(round(., 3))) %>%
  arrange(desc(TRB_per_game))

C <- C %>%
  filter(BLKr >= 0)


Andre_Drummond <- C %>%
  select(Player:G, X2P, BLK, ORB:TRB, ORBX2P, Salary) %>%
  filter(Player == "Andre Drummond")

#Starting Five 

Starting_Five <- Player %>%
filter(Player %in% c("Monte Morris", "Devin Booker", "Nicolas Batum", "PJ Tucker", "Andre Drummond")) %>%
  select(Player:G, 
         FG:X2Pp, 
         FT:FTp, 
         ORB:AST, 
         BLK:TOV, 
         PTS, 
         PTS_per_game,
         ASTTOVR, 
         Salary) %>%
  mutate(ORBX2P = (ORB/X2P),
         AST_per_game = (AST/G), 
         RPG = (TRB/G)) %>%
  mutate_at(vars(ASTTOVR, ORBX2P, RPG, AST_per_game), funs(round(., 3)))

Starting_Five <- Starting_Five %>%
  select(Player:G,
         Salary, 
         FG:FTp, 
         ORB:TRB, 
         RPG,
         ORBX2P, 
         AST, 
         AST_per_game,
         TOV, 
         ASTTOVR,
         BLK, 
         PTS, 
         PTS_per_game)

sum(Starting_Five$Salary)

