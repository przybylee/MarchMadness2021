#Lee Przybylski 3/10/2021
library(ggplot2)
#library(plyr)
library(tidyverse)
options(stringsAsFactors = F)

#Read in data with scores for each team
scores <- read.csv("Scores2020-2021.csv", header = TRUE, sep = ",")
head(scores)
#read in bartorvik data for the same year
teams <- read.csv("cbb21_no_results.csv", header = TRUE, sep = ",")
head(teams)

teams_bartorvik <- as.character(unique(teams$team))
teams_scores <- as.character(unique(scores$Team))
char_min <- stringdist::stringdist(team_names[1],team_ids)

#read in team ids from Kaggle data
MTeams <- read.csv("Kaggle Data/MTeams.csv")
head(MTeams)

#For each team in Mteams, identify the team id from the scores data and the team data 
#Here is a function for identifying the closest match between two vectors of strings
name_match <- function(x,y){
  char_min <- stringdist::stringdist(x, y)
  y[which.min(char_min)]
}
name_match(MTeams$TeamName[1], teams_scores)
name_match(MTeams$TeamName[1], teams_bartorvik)

#Create a column in Mteams for the bartorvik ids
MTeams$bartorvik <- as.character(0)
L <- length(MTeams$TeamID)
for (j in 1:L){
  name <- MTeams$TeamName[j]
  b_name <- name_match(name, teams_bartorvik)
  MTeams$bartorvik[j] <- b_name
}
head(MTeams)
tail(MTeams)

#Create a cloumn in Mteams for the ids in the scores data
MTeams$scores <- as.character(0)
for (j in 1:L){
  name <- MTeams$TeamName[j]
  s_name <- name_match(name, teams_scores)
  MTeams$scores[j] <- s_name
}
head(MTeams)
tail(MTeams)

write.table(MTeams, file = "TeamList.csv", col.names = T, row.names = F, sep = ",")
#There are still some serious problems with the matching.