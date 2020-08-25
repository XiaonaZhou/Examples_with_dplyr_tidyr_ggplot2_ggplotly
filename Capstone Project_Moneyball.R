library(dplyr)
batting <- read.csv('Batting.csv')
head(batting)
str(batting)
head(batting$AB)
head(batting$X2B)
batting$BA <- batting$H / batting$AB
tail(batting$BA,5)
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

batting$x1B <- (batting$H - batting$X2B - batting$X3B - batting$HR)
batting$SLG <- (batting$x1B + 2*batting$X2B + 3*batting$X3B + 4*batting$HR)/batting$AB

str(batting)

salaries <- read.csv('Salaries.csv')
batting_af_1985 <- subset(batting, batting$yearID >= 1985)
summary(batting_af_1985)
bat_sal <- merge(batting_af_1985,salaries, by = c('playerID','yearID'))
summary(bat_sal)

lost_players <- subset(bat_sal, bat_sal$playerID %in% c('giambja01','damonjo01','saenzol01')) 


lost_player_2001 <- subset(lost_players, yearID == 2001)

reduced_lost_player <- lost_player_2001 %>% 
  select(playerID, H, X2B,X3B, HR, OBP, SLG, BA, AB)

possible.players <- bat_sal %>%  
  filter(salary<8000000,OBP>0, yearID == 2001, AB>500) %>% 
  arrange(desc(OBP)) %>% 
  select(playerID, OBP, AB, salary)
sum(lost_player_2001$AB)
sum(lost_player_2001$OBP)
sum(possible.players$AB[2:4])
sum(possible.players$OBP[2:4])
