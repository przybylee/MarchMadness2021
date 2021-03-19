#Lee Przybylski 3/10/2021
library(ggplot2)
#library(plyr)
library(tidyverse)
options(stringsAsFactors = F)

#First we will load the team list from 2021 as a point of reference
yr <- 2021
source("sort_functions.R")
#Choose Year for validation:
yr <- 2019
yr2000 <- yr - 2000

#read in bartorvik data for the same year
file <- paste("cbb", yr2000, ".csv", sep = "")
teams <- read.csv(file, header = TRUE, sep = ",")
head(teams)

teams_bartorvik <- unique(teams$team)
#teams_scores <- unique(scores$Team)

#read in team ids from Kaggle data
MTeams <- read.csv("Kaggle Data/MDataFiles_Stage2/MTeams.csv")
head(MTeams)

#For each team in Mteams, identify the team id from thethe team data 
#Functions for looking up name mathces have been loaded from sort_functions.R
teams_bartorvik <- teams$TEAM
name_list("Gonzaga", teams_bartorvik, 11)
name_match(MTeams$TeamName[1], teams_bartorvik)

head(MTeams)
#Create a column in Mteams for the bartorvik ids
MTeams$bartorvik <- as.character(0)
L <- length(MTeams$TeamID)
for (j in 1:L){
  ID <- MTeams$TeamID[j]
  b_name <- ID_to_bar(ID, error = FALSE)
  MTeams$bartorvik[j] <- b_name
}
head(MTeams)
tail(MTeams)
#How many exact matches:
sum(MTeams$bartorvik == "None")/371
matched <- MTeams$bartorvik != "None"
MTeams_keep <- MTeams[matched,]
MTeams_sort <- MTeams[!matched,]
#Remove team names that found an exact match
teams_bartorvik1 <- teams_bartorvik[!(teams_bartorvik %in% MTeams_keep$TeamName)]
head(teams_bartorvik1)
head(MTeams_sort)
#Repeat matching, but only look at bartrovik1 for matches
L <- length(MTeams_sort$TeamID)
for (j in 1:L){
  name <- MTeams_sort$TeamName[j]
  b_name <- name_match(name, teams_bartorvik1)
  MTeams_sort$bartorvik[j] <- b_name
}
MTeams_sort
write.table(MTeams_sort, "bartorvik_sort.csv", col.names = T, row.names = F, sep = ",")
#Use name search to find best remaining names
name_search("Birmingham", teams_bartorvik1)
#Read back in sorted teams
MTeams_sorted <- read.csv("bartorvik_sort.csv", header = T, sep = ",")
head(MTeams_sorted)
New <- MTeams_sorted[MTeams_sorted$match == 1, 1:5]
MTeams <- rbind(MTeams_keep, New)
Unmatched <- MTeams_sorted[MTeams_sorted$match == 0, 1:5]

#For validation, we will not add scores data, and only use regular games from the challenge dataset.
file <- paste("TeamList", yr, ".csv", sep = "")
write.table(MTeams, file, row.names = FALSE, col.names = TRUE, sep = ",")


#Below we save the code from the first ListTeams script in case scores data should be 
#incorporated into validation sets

#Create a cloumn in Mteams for the ids in the scores data
MTeams$scores <- as.character(0)
L <- length(MTeams$TeamID)
for (j in 1:L){
  name <- MTeams$TeamName[j]
  s_name <- name_match(name, teams_scores)
  MTeams$scores[j] <- s_name
}
head(MTeams)
tail(MTeams)
#Remove exact matches from consideration
exact <- MTeams$TeamName == MTeams$scores
v <- ifelse(exact, 1,0)
matched <- MTeams$TeamName[exact]
teams_scores1 <- teams_scores[!(teams_scores %in% matched)]
MTeams_sort <- cbind(MTeams, v)
head(MTeams_sort)
#write.table(MTeams_sort, "score_teams_sort.csv", col.names = T, row.names = F, sep = ",")
#Use name_search to confirm exact matches
name_search("Loyola", teams_scores1)
name_search("Miami", teams_scores1)

#Read back in sorted data
MTeams_sorted <- read.csv("score_teams_sort.csv", header = T, sep = ",")
head(MTeams_sorted)
#One row was not matched (Brooklyn)
MTeams_sorted[MTeams_sorted$v2 == 0,]
teams_scores2 <- teams_scores1[!(teams_scores1 %in% MTeams_sorted$scores)]

#Save the 3 lists
write.table(MTeams_sorted, "TeamList0.csv", col.names = T, row.names = F, sep = ",")
write.table(bartorvik2, "Bartorvik_nomatch.csv", col.names = T, row.names = F, sep = ",")
write.table(teams_scores2, "scores_nomatch.csv", col.names = T, row.names = F, sep = ",")
write.table(Unmatched, "Unmatched_ids.csv", col.names = T, row.names = F, sep = ",")


#We try to put the unmatched teams in teams_scores2 with a team id
#MTeams_sorted <- read.csv("TeamList0.csv", header = T, sep = ",")
#bartorvik2 <- read.csv("TeamList0.csv", header = T, sep = ",")
#MTeams_sorted <- read.csv("TeamList0.csv", header = T, sep = ",")

#Start with the 58 teams in teams_scores2
L <- length(teams_scores2)
v <- as.character(rep(0,L))
for (j in 1:L){
  name <- teams_scores2[j]
  id_name <- name_match(name, MTeams_sorted$TeamName)
  v[j] <- id_name
}
d_names <- data.frame(cbind(teams_scores2, v))
d_names
d_names$check <- 0
d_names[1:5,]
d_names$check[1:5] <- c(1,0,1,1,1)
m <- 5
d_names[(1+m):(5+m),]
d_names$check[(1+m):(5+m)] <- c(0,1,0,1,1)
m <- m+5
d_names[(1+m):(5+m),]
d_names$check[(1+m):(5+m)] <- c(1,0,1,0,0)
m <- m+5
d_names[(1+m):(5+m),]
d_names$check[(1+m):(5+m)] <- c(1,0,1,0,0)
m <- m+5
d_names[(1+m):(5+m),]
d_names$check[(1+m):(5+m)] <- c(0,0,0,0,1)
m <- m+5
d_names[(1+m):(5+m),]
d_names$check[(1+m):(5+m)] <- c(1,0,1,1,0)
m <- m+5
d_names[(1+m):(5+m),]
d_names$check[(1+m):(5+m)] <- c(0,0,0,1,0)
m <- m+5
d_names[(1+m):(5+m),]
d_names$check[(1+m):(5+m)] <- c(0,1,1,1,1)
m <- m+5
d_names[(1+m):(5+m),]
d_names$check[(1+m):(5+m)] <- c(0,1,1,1,1)
m <- m+5
d_names[(1+m):(5+m),]
d_names$check[(1+m):(5+m)] <- c(0,1,1,0,1)
m <- m+5
d_names[(1+m):(5+m),]
d_names$check[(1+m):(5+m)] <- c(1,1,1,0,1)
m <- m+5
d_names[(1+m):(3+m),]
d_names$check[(1+m):(3+m)] <- c(0,1,1)
sum(d_names$check)
# We added 33 names matched
#rows without matches in teams_scores2
d_names[d_names$check == 0,]
list <- MTeams_sorted$TeamName
lst <- name_list("NorthernIllinois", list )
print(lst)
d_names$v[2] <- lst[3,1]
d_names$check[2] <- 1
d_names[d_names$check == 0,]
lst <- name_list("SouthCarolinaState", list, n =5 )
print(lst)
list[grepl("Kentucky", list)]
d_names$v[14] <- lst[3,1]
d_names$check[14] <- 1
d_names[d_names$check == 0,]
lst <- name_list("NorthCarolinaCentral", list, n =10 )
print(lst)
list[grepl("Carolina", list)]
d_names$v[17] <- lst[4,1]
d_names$check[17] <- 1
d_names[d_names$check == 0,]
lst <- name_list("FloridaAM", list, n =5 )
print(lst)
list[grepl("Kentucky", list)]
d_names$v[20] <- lst[2,1]
d_names$check[20] <- 1
d_names[d_names$check == 0,]
lst <- name_list("MississippiValleyState", list, n =5 )
print(lst)
list[grepl("Purdue", list)]
d_names$v[22] <- lst[3,1]
d_names$check[22] <- 1
d_names[d_names$check == 0,]
lst <- name_list("CentralConnecticut", list, n =5 )
print(lst)
d_names$v[23] <- lst[2,1]
d_names$check[23] <- 1
d_names[d_names$check == 0,]
lst <- name_list("UMKC", list, n =10 )
print(lst)
list[grepl("Missouri", list)]
d_names$v[27] <- "Missouri KC"
d_names$check[27] <- -1
d_names[d_names$check == 0,]
lst <- name_list("TexasAM", list, n =10 )
print(lst)
list[grepl("Corpus", list)]
d_names$v[31] <- lst[2,1]
d_names$check[31] <- 1
d_names[d_names$check == 0,]
lst <- name_list("AlabamaAM", list, n =10 )
print(lst)
list[grepl("Corpus", list)]
d_names$v[32] <- lst[2,1]
d_names$check[32] <- 1
d_names[d_names$check == 0,]
lst <- name_list("TennesseeMartin", list, n =10 )
print(lst)
list[grepl("Corpus", list)]
d_names$v[35] <- lst[4,1]
d_names$check[35] <- 1
d_names[d_names$check == 0,]
lst <- name_list("CentralArkansas", list, n =10 )
print(lst)
list[grepl("Corpus", list)]
d_names$v[36] <- "Cent Arkansas"
d_names$check[36] <- -1
d_names[d_names$check == 0,]
d_names$v[33] <- "Florida Intl"
d_names$check[33] <- -1
d_names[d_names$check == 0,]
d_names$v[41] <- "TAM C. Christi"
d_names$check[41] <- -1
d_names[d_names$check == 0,]
lst <- name_list("Northwestern", list, n =5 )
print(lst)
list[grepl("LA", list)]
d_names$v[49] <- "Northwestern LA"
d_names$check[49] <- -1
d_names[d_names$check == 0,]
list[grepl("Little Rock", list)]
d_names$v[56] <- "Ark Little Rock"
d_names$check[56] <- 1

d_names <- d_names[1:58,]
d_names$check[10] <-0 
length(unique(d_names$v[d_names$check == 1]))
sum(d_names$check == 1)
#The names in d_names with check = 1 get assigned as a second scores name
MTeams_sorted$scores2 <- NA
d_names1 <- d_names[d_names$check ==1,]
L <- length(d_names1$v)
for (j in 1:L){
  team_row <- d_names1$v[[j]]
  MTeams_sorted[MTeams_sorted$TeamName == team_row, "scores2"] <- d_names1$teams_scores2[j]
}
#The names in d_names with check = 1- get matched with the correct row from Unmatched
head(MTeams_sorted)
head(Unmatched2)
d_names2 <- d_names[d_names$check < 0,]
Unmatched2 <- Unmatched[Unmatched$TeamName %in% d_names2$v,]
Unmatched2$v <- 0
Unmatched2$v2 <- 0
Unmatched2$scores <- NA
Unmatched2$scores2 <- NA
L <- length(d_names2$v)
for (j in 1:L){
  team_row <- d_names2$v[[j]]
  Unmatched2[Unmatched2$TeamName == team_row, "scores"] <- d_names2$teams_scores2[j]
}
head(MTeams_sorted)
MTeams_sorted <- rbind(MTeams_sorted, Unmatched2)
tail(MTeams_sorted)

#Try to match unclaimed names in bartorvik2 to the sorted team names:
names <- as.character(rep(0, 9))
for (j in 1:9){
  bar <- bartorvik2[j]
  names[j] <- name_match(bar, MTeams$scores)
}
d_bar <- data.frame(cbind(bartorvik2, names))
d_bar
MTeams_sorted$bartorvik[MTeams_sorted$scores == "UMKC"] <- "UMKC"
MTeams_sorted$bartorvik[MTeams_sorted$scores == "TexasA&MCorpus"] <- "Texas A&M Corpus Chris"
MTeams_sorted$scores2[MTeams_sorted$scores == "TexasA&MCorpus"] <- "TexasAMCorpus"
MTeams_sorted$bartorvik[MTeams_sorted$scores == "FloridaIntl"] <- "FIU"
d <- Unmatched[Unmatched$TeamName == "Cent Arkansas",]
d$scores[1] <- "CentralArkansas"
d$scores2[1] <- NA
d$v <- 0
d$v2 <- 0
MTeams_sorted <- rbind(MTeams_sorted, d)
#How many bartorvik ids still need matching
bartorvik3 <- bartorvik2[!(bartorvik2 %in% MTeams_sorted$bartorvik)]
teams_scores3 <- teams_scores2[!(teams_scores2 %in% MTeams_sorted$scores)]
teams_scores3 <- teams_scores3[!(teams_scores3 %in% MTeams_sorted$scores2)]
bartorvik3
teams_scores3
#We have 4 from bartorvik and 11 from scores
MTeams_sorted$scores2[MTeams_sorted$scores == "NorthwesternSt"] <- "NorthwesternState"
#Found SIUE
MTeams_sorted[MTeams_sorted$TeamName == "SIUE",]
MTeams_sorted[MTeams_sorted$TeamName == "S Illinois",]
MTeams_sorted[MTeams_sorted$TeamName == "S Illinois","scores2"] <- "SouthernIllinois"
MTeams_sorted[MTeams_sorted$TeamName == "SIUE",c("bartorvik", "scores")] <- c("SIU Edwardsville", "SIUEdwardsville")
d <- Unmatched[Unmatched$TeamName %in% c("MTSU", "UTRGV", "PFW"),]
d$scores <- c("IPFW", "MiddleTennSt", "UTRioGrandeValley")
d$bartorvik <- c("Fort Wayne", "Middle Tennessee", "UT Rio Grande Valley")
d$v <- 0
d$v2 <- 0
d$scores2 <- NA
MTeams_sorted <- rbind(MTeams_sorted, d)
bartorvik4 <- bartorvik3[!(bartorvik3 %in% MTeams_sorted$bartorvik)]
teams_scores4 <- teams_scores3[!(teams_scores3 %in% MTeams_sorted$scores)]
teams_scores4 <- teams_scores4[!(teams_scores4 %in% MTeams_sorted$scores2)]
bartorvik4
teams_scores4
#We still have 5 score team names to sort
list <- MTeams_sorted$TeamName
name_search("ETSU", list)
MTeams_sorted[MTeams_sorted$TeamName == "ETSU","scores2"] <- "EastTennState"
name_search("WK", list)
MTeams_sorted[MTeams_sorted$TeamName == "WKU", "scores"] <- "WesternKentucky"
name_search("NC", list)
MTeams_sorted[MTeams_sorted$TeamName == "NC A&T", "scores3"] <- "NorthCarolinaAT"
MTeams_sorted$scores3 <- NA
MTeams_sorted[MTeams_sorted$TeamName == "NC A&T", "scores2"] <- "N.CarolinaA&T"
name_search("LI", MTeams_sorted$bartorvik)
MTeams_sorted[MTeams_sorted$bartorvik == "LIU Brooklyn", "scores"] <- "LongIsland"
teams_scores5 <- teams_scores4[!(teams_scores4 %in% MTeams_sorted$scores)]
teams_scores5 <- teams_scores5[!(teams_scores5 %in% MTeams_sorted$scores2)]
teams_scores5 <- teams_scores5[!(teams_scores5 %in% MTeams_sorted$scores3)]
teams_scores5

#We finish with 348 teams with IDs from the sorted data
MTeams_final <- MTeams_sorted[,c(1:6, 9,10)]
head(MTeams_final)
#Make sure each column has no duplicates:
sum(duplicated(MTeams_final$TeamName))
MTeams_final[duplicated(MTeams_final$TeamName),]
MTeams_final <- MTeams_final[!duplicated(MTeams_final$TeamName),]

sum(duplicated(MTeams_final$bartorvik))

sum(duplicated(MTeams_final$scores))
dup_scores <- MTeams_final[duplicated(MTeams_final$scores),"scores"]
MTeams_final[MTeams_final$scores %in% dup_scores,]
MTeams_final <- MTeams_final[!duplicated(MTeams_final$TeamName),]
MTeams_final[c("241", "304"), "scores"] <- c("DelawareState", "SanDiegoState")
MTeams_final[c("241", "304"), "scores2"] <- c(NA, NA)

D <- MTeams_final[!is.na(MTeams_final$scores2),]
sum(duplicated(D$scores2))
D <- MTeams_final[!is.na(MTeams_final$scores3),]
sum(duplicated(D$scores3))
#Remove NA
head(MTeams_final)
summary(MTeams_final)
indx <- is.na(MTeams_final$scores2)
sum(indx)
MTeams_final$scores2[indx] <- "none"
indx <- is.na(MTeams_final$scores3)
sum(indx)
MTeams_final$scores3[indx] <- "none"

#Now that duplicates are removed, we can save the file
write.table(MTeams_final, file = "TeamList2021.csv", col.names = T, row.names = F, sep = ",")
