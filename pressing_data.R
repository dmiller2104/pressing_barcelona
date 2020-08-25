## necssary libraries

library(ggplot2)
library(dplyr)
library(tidyverse)
library(StatsBombR)
library(FC.rSTATS)
library(soccermatics)
library(ggsoccer)
library(SBpitch)
library(formattable)
library(RColorBrewer)
library(png)
library(ggrepel)
library(extrafont)
font_import()
loadfonts(device = "win")
# fonts() to check fonts available

## get statsbomb logo ##
img <- grid::rasterGrob(png::readPNG("statsbomb_logo.png"), interpolate = TRUE)

## la liga datasets - 13 seasons worth ##

la_liga_06_07 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2006/2007')

la_liga_07_08 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2007/2008')

la_liga_08_09 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2008/2009')

la_liga_09_10 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2009/2010')

la_liga_10_11 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2010/2011')

la_liga_11_12 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2011/2012')

la_liga_12_13 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2012/2013')

la_liga_13_14 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2013/2014')

la_liga_14_15 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2014/2015')

la_liga_15_16 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2015/2016')

la_liga_16_17 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2016/2017')

la_liga_17_18 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2017/2018')

la_liga_18_19 <- FreeCompetitions() %>% 
  filter(competition_id == 11 & season_name == '2018/2019')

# data preparation, getting match data, cleaning and getting minutes for games
# la liga gives us the league and match level data
# minutes gives us the minutes played per season per player

la_liga_06_07 <- FreeMatches(la_liga_06_07)
la_liga_06_07 <- StatsBombFreeEvents(MatchesDF = la_liga_06_07, Parallel = T)
la_liga_06_07 <- allclean(la_liga_06_07)
minutes_06_07 <- get.minutesplayed(la_liga_06_07)
minutes_06_07 <- minutes_06_07 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

la_liga_07_08 <- FreeMatches(la_liga_07_08)
la_liga_07_08 <- StatsBombFreeEvents(MatchesDF = la_liga_07_08, Parallel = T)
la_liga_07_08 <- allclean(la_liga_07_08)
minutes_07_08 <- get.minutesplayed(la_liga_07_08)
minutes_07_08 <- minutes_07_08 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

la_liga_08_09 <- FreeMatches(la_liga_08_09)
la_liga_08_09 <- StatsBombFreeEvents(MatchesDF = la_liga_08_09, Parallel = T)
la_liga_08_09 <- allclean(la_liga_08_09)
minutes_08_09 <- get.minutesplayed(la_liga_08_09)
minutes_08_09 <- minutes_08_09 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

la_liga_09_10 <- FreeMatches(la_liga_09_10)
la_liga_09_10 <- StatsBombFreeEvents(MatchesDF = la_liga_09_10, Parallel = T)
la_liga_09_10 <- allclean(la_liga_09_10)
minutes_09_10 <- get.minutesplayed(la_liga_09_10)
minutes_09_10 <- minutes_09_10 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

la_liga_10_11 <- FreeMatches(la_liga_10_11)
la_liga_10_11 <- StatsBombFreeEvents(MatchesDF = la_liga_10_11, Parallel = T)
la_liga_10_11 <- allclean(la_liga_10_11)
minutes_10_11 <- get.minutesplayed(la_liga_10_11)
minutes_10_11 <- minutes_10_11 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

la_liga_11_12 <- FreeMatches(la_liga_11_12)
la_liga_11_12 <- StatsBombFreeEvents(MatchesDF = la_liga_11_12, Parallel = T)
la_liga_11_12 <- allclean(la_liga_11_12)
minutes_11_12 <- get.minutesplayed(la_liga_11_12)
minutes_11_12 <- minutes_11_12 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

la_liga_12_13 <- FreeMatches(la_liga_12_13)
la_liga_12_13 <- StatsBombFreeEvents(MatchesDF = la_liga_12_13, Parallel = T)
la_liga_12_13 <- allclean(la_liga_12_13)
minutes_12_13 <- get.minutesplayed(la_liga_12_13)
minutes_12_13 <- minutes_12_13 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

la_liga_13_14 <- FreeMatches(la_liga_13_14)
la_liga_13_14 <- StatsBombFreeEvents(MatchesDF = la_liga_13_14, Parallel = T)
la_liga_13_14 <- allclean(la_liga_13_14)
minutes_13_14 <- get.minutesplayed(la_liga_13_14)
minutes_13_14 <- minutes_13_14 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

la_liga_14_15 <- FreeMatches(la_liga_14_15)
la_liga_14_15 <- StatsBombFreeEvents(MatchesDF = la_liga_14_15, Parallel = T)
la_liga_14_15 <- allclean(la_liga_14_15)
minutes_14_15 <- get.minutesplayed(la_liga_14_15)
minutes_14_15 <- minutes_14_15 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

la_liga_15_16 <- FreeMatches(la_liga_15_16)
la_liga_15_16 <- StatsBombFreeEvents(MatchesDF = la_liga_15_16, Parallel = T)
la_liga_15_16 <- allclean(la_liga_15_16)
minutes_15_16 <- get.minutesplayed(la_liga_15_16)
minutes_15_16 <- minutes_15_16 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

## 2016/17 data appears to have been removed ##

la_liga_16_17 <- FreeMatches(la_liga_16_17)
la_liga_16_17 <- StatsBombFreeEvents(MatchesDF = la_liga_16_17, Parallel = T)
la_liga_16_17 <- allclean(la_liga_16_17)
minutes_16_17 <- get.minutesplayed(la_liga_16_17)
minutes_16_17 <- minutes_16_17 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

la_liga_17_18 <- FreeMatches(la_liga_17_18)
la_liga_17_18 <- StatsBombFreeEvents(MatchesDF = la_liga_17_18, Parallel = T)
la_liga_17_18 <- allclean(la_liga_17_18)
minutes_17_18 <- get.minutesplayed(la_liga_17_18)
minutes_17_18 <- minutes_17_18 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

la_liga_18_19 <- FreeMatches(la_liga_18_19)
la_liga_18_19 <- StatsBombFreeEvents(MatchesDF = la_liga_18_19, Parallel = T)
la_liga_18_19 <- allclean(la_liga_18_19)
minutes_18_19 <- get.minutesplayed(la_liga_18_19)
minutes_18_19 <- minutes_18_19 %>% 
  group_by(player.id) %>% 
  summarise(minutes = sum(MinutesPlayed))

## barcelona specific data

# this function pulls together barcelona specific data, by player, including chances created, assists, Xg, and details on goal data like how it was scored

target <- c('Pressure','Pass','Carry','Ball Receipt*','Ball Recovery','Duel','Interception')

barca_get_data <- function(la_liga_season){
  barca_df <- la_liga_season %>% filter(team.name == "Barcelona") %>% 
  filter(type.name %in% target) %>% 
    select(id, type.name, related_events,location, player.name, position.name, location.x, location.y, pass.length, pass.end_location, 
    pass.end_location.x, pass.end_location.y, pass.recipient.name, pass.outcome.name, interception.outcome.name, foul_committed.type.name,
    pass.miscommunication, ball_receipt.outcome.name, dribble.outcome.name, carry.end_location.x, carry.end_location.y)
}

# the function is then applied to all previous la liga seasons

barca_06_07 <- barca_get_data(la_liga_06_07)
barca_06_07$season <- "06/07"
#barca_06_07 <- left_join(barca_06_07, minutes_06_07)
#barca_06_07 <- barca_06_07 %>% mutate(nineties = minutes/90)

barca_07_08 <- barca_get_data(la_liga_07_08)
barca_07_08$season <- "07/08"
#barca_07_08 <- left_join(barca_07_08, minutes_07_08)
#barca_07_08 <- barca_07_08 %>% mutate(nineties = minutes/90)

barca_08_09 <- barca_get_data(la_liga_08_09)
barca_08_09$season <- "08/09"
#barca_08_09 <- left_join(barca_08_09, minutes_08_09)
#barca_08_09 <- barca_08_09 %>% mutate(nineties = minutes/90)

barca_09_10 <- barca_get_data(la_liga_09_10)
barca_09_10$season <- "09/10"
#barca_09_10 <- left_join(barca_09_10, minutes_09_10)
#barca_09_10 <- barca_09_10 %>% mutate(nineties = minutes/90)

barca_10_11 <- barca_get_data(la_liga_10_11)
barca_10_11$season <- "10/11"
#barca_10_11 <- left_join(barca_10_11, minutes_10_11)
#barca_10_11 <- barca_10_11 %>% mutate(nineties = minutes/90)

barca_11_12 <- barca_get_data(la_liga_11_12)
barca_11_12$season <- "11/12"
#barca_11_12 <- left_join(barca_11_12, minutes_11_12)
#barca_11_12 <- barca_11_12 %>% mutate(nineties = minutes/90)

barca_12_13 <- barca_get_data(la_liga_12_13)
barca_12_13$season <- "12/13"
#barca_12_13 <- left_join(barca_12_13, minutes_12_13)
#barca_12_13 <- barca_12_13 %>% mutate(nineties = minutes/90)

barca_13_14 <- barca_get_data(la_liga_13_14)
barca_13_14$season <- "13/14"
barca_13_14 <- left_join(barca_13_14, minutes_13_14)
barca_13_14 <- barca_13_14 %>% mutate(nineties = minutes/90)

barca_14_15 <- barca_get_data(la_liga_14_15)
barca_14_15$season <- "14/15"
#barca_14_15 <- left_join(barca_14_15, minutes_14_15)
#barca_14_15 <- barca_14_15 %>% mutate(nineties = minutes/90)

barca_15_16 <- barca_get_data(la_liga_15_16)
barca_15_16$season <- "15/16"
#barca_15_16 <- left_join(barca_15_16, minutes_15_16)
#barca_15_16 <- barca_15_16 %>% mutate(nineties = minutes/90)

barca_16_17 <- barca_get_data(la_liga_16_17)
barca_16_17$season <- "16/17"
#barca_16_17 <- left_join(barca_16_17, minutes_16_17)
#barca_16_17 <- barca_16_17 %>% mutate(nineties = minutes/90)

barca_17_18 <- barca_get_data(la_liga_17_18)
barca_17_18$season <- "17/18"
#barca_17_18 <- left_join(barca_17_18, minutes_17_18)
#barca_17_18 <- barca_17_18 %>% mutate(nineties = minutes/90)

barca_18_19 <- barca_get_data(la_liga_18_19)
barca_18_19$season <- "18/19"
#barca_18_19 <- left_join(barca_18_19, minutes_18_19)
#barca_18_19 <- barca_18_19 %>% mutate(nineties = minutes/90)


## rowbind all seasons together

all_seasons <- rbind(barca_06_07, barca_07_08, barca_08_09, barca_09_10, barca_10_11,
                     barca_11_12, barca_12_13, barca_13_14, barca_14_15, barca_15_16,
                     barca_17_18, barca_18_19)

# pressure events "events to close down an opponent in a way that forces them to make a decision"

all_seasons %>% group_by(season) %>% 
  summarise(average_passes_per_game = sum(is.na(pass.outcome.name), na.rm = TRUE) / 38,
            average_pass_completion = (sum(is.na(pass.outcome.name), na.rm = TRUE) / length(pass.outcome.name)),
            avearge_pressure_events_per_game = round(sum(type.name == 'Pressure') / 38),
            average_pass_length = mean(pass.length * (is.na(pass.outcome.name)),na.rm = TRUE),
            interceptions_per_game = (sum(interception.outcome.name == "Won", na.rm = TRUE)/38),
            ball_recoveries_per_game = (sum(type.name == 'Ball Recovery')/38))