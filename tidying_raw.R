library(tidyverse)
library(broom)

setwd("~/Assessment-4/Assessment-4-Reproducible-Data-Analysis-Project")

## Load the data into the project 

salaries <- read.csv("data/raw_data/2018-19_nba_player-salaries.csv")
statistics <- read.csv("data/raw_data/2018-19_nba_player-statistics.csv")
team_statistics_1 <- read_csv("data/raw_data/2018-19_nba_team-statistics_1.csv")
team_statistics_2 <- read_csv("data/raw_data/2018-19_nba_team-statistics_2.csv")
payroll <- read.csv("data/raw_data/2019-20_nba_team-payroll.csv")

# Tidying the team data provided 

team_statistics_1 <- team_statistics_1 %>%
  rename(Rk = 'Rk', 
         eFGp = 'eFG.',
         TOVp = 'TOV.',
         ORBp = 'ORB.',
         DRBp = 'DRB.')

team_statistics_1[,c("X","X.1","X.2")] <- list(NULL) ## This step drops the blank variables at the end of the dataset

team_statistics <- bind_cols(team_statistics_2, team_statistics_1[c(4:5)]) # This step combines the teams data and there overall win lose record                                  

team_statistics <- team_statistics %>% 
  rename(FGp = 'FG%',
         X3Pp = '3P%',
         X2Pp = '2P%',
         FTp = 'FT%') %>%
  mutate(ASTTOVR = (AST/TOV)) %>%
  select(Rk:Team, G, W:L, FG:PTS, ASTTOVR)

# Tidying the individual players data provided 

statistics <- rename(statistics, 
                     FGp = 'FG.',
                     X3Pp = 'X3P.',
                     X2Pp = 'X2P.',
                     eFGp = 'eFG.',
                     FTp = 'FT.',
                     Player = 'player_name')

salaries <- salaries %>%
  rename(Player = 'player_name', 
         Salary = 'salary') %>%
  select(Player:Salary)

salaries[,c("X", "X.1", "X.2", "X.3")] <- list(NULL) # This step drops the blank variables at the end of the dataset

statistics <- filter(statistics, Tm != "TOT") # This step removes when Tm = TOT, because it means total for players that are in more then 1 team 

statistics <- data.frame(stringsAsFactors = FALSE, statistics)

statistics <- statistics %>%          # data set contains the same players playing for multiple teams and positions. This step combines those players statistics. 
  group_by(Player, Age) %>% 
  summarise(Pos = paste(Pos, collapse = ", "),
            Tm = paste(Tm, collapse = ", "),
            G = sum(G), 
            GS = sum(GS),
            MP = sum(MP), 
            FG = sum(FG),
            FGA = sum(FGA),
            FGp = (FG/FGA), 
            X3P = sum (X3P),
            X3PA = sum(X3PA), 
            X3Pp = (X3P/X3PA),
            X2P = sum(X2P),
            X2PA = sum(X2PA), 
            X2Pp = (X2P/X2PA), 
            FT = sum(FT),
            FTA = sum(FTA),
            FTp = (FT/FTA),
            ORB = sum(ORB), 
            DRB = sum(DRB), 
            TRB = sum(TRB), 
            AST = sum(AST), 
            STL = sum(STL), 
            BLK = sum(BLK), 
            TOV = sum(TOV), 
            PF = sum(PF), 
            PTS = sum(PTS),
            FTF = (FTA/FGA), 
            PTS_per_game = (PTS / G), 
            PTS_per_min = (PTS/MP),
            ASTTOVR = (AST/TOV))

statistics <- full_join(x = statistics, y = salaries) %>%
  drop_na()

player_statistics <- statistics

write_csv(player_statistics, file = "data/processed_data/player_statistics.csv")
write_csv(team_statistics, file = "data/processed_data/team_statistics.csv")
