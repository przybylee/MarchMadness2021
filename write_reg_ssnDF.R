#Clean the scores data
options(stringsAsFactors = FALSE)

yr <- 2021
source("sort_functions.R")
scores <- read.csv("Scores2020-2021.csv", header = TRUE, sep = ",")
path <- "Kaggle Data/MDataFiles_Stage2/MRegularSeasonCompactResults.csv"
Kscores0 <- read.csv(path, header = TRUE, sep = ",")
Kscores <- Kscores0[Kscores0$Season == 2021,]
head(Kscores)
summary(Kscores)

L <- length(scores$Team)
C_ids <- rep(0,L)
t0 <- Sys.time()
for (j in 1:L){
  string <- scores$Team[j]
  C_ids[j] <- scores_to_ID(string)
  print(paste(j, "of", L, "IDs found"))
}
tf <- Sys.time()
print(tf -t0)
head(scores)
scores$TeamID <-C_ids
ID_to_name(1139)

#We have more games in the scores data than in Kscores
#We will clean up the scores data and add games from Kscores that are not
#included in scores
dates <- scores$Date
dates[dates <1000] <- dates[dates <1000] +1200
udates <- sort(unique(c(dates, 1224)))
scores$Day <- 0
L <- length(udates)       
for (j in 1:L){
  indx <- dates == udates[j]
  scores$Day[indx] <- j
}
head(scores)

#Index each obsereved game in scores with "HID,VID,Hscore,Vscore"
L<- length(scores$TeamID)
games_scores <- as.character(rep(0, L/2))
date_scores <- rep(0, L/2)
for (j in seq(2,L, by = 2)){
  HID <- scores$TeamID[j]
  VID <- scores$TeamID[j-1]
  Hscore <- scores$Final[j]
  Vscore <- scores$Final[j-1]
  games_scores[j/2] <- paste(HID, VID, Hscore, Vscore, sep = ",")
  date_scores[j/2] <- scores$Day[j]
}
d_scores <- data.frame(gameID = games_scores, day = date_scores)
tail(d_scores)
tail(scores)

#Now index each observed game in Kscores the same way
head(Kscores)
L <- length(Kscores$WTeamID)
games_K <- as.character(rep(0,L))
date_K <- rep(0,L)
for (j in 1:L){
  date_K[j] <- Kscores$DayNum[j]
  if (Kscores$WLoc[j] %in% c("H", "N")){
    HID <- Kscores$WTeamID[j]
    VID <- Kscores$LTeamID[j]
    Hscore <- Kscores$WScore[j]
    Vscore <- Kscores$LScore[j]
    games_K[j] <- paste(HID, VID, Hscore, Vscore, sep = ",")
  }else{
    HID <- Kscores$LTeamID[j]
    VID <- Kscores$WTeamID[j]
    Hscore <- Kscores$LScore[j]
    Vscore <- Kscores$WScore[j]
    games_K[j] <- paste(HID, VID, Hscore, Vscore, sep = ",")
  }
}
d_K <- data.frame(gameID= games_K, day = date_K)
tail(d_K)
tail(Kscores)
#Check how many in games_K are in games_scores
sum(!(games_K %in% games_scores))
#There are 795 extra games from the Kaggle data.  Most of them are from conf tourneys
#For the games listed in both data sets, compare the date systems:
overlp <- games_K %in% games_scores
d_overlp <- d_K[overlp,]
head(d_overlp)
d_overlp$day_score <- 0
L <- length(d_overlp$day_score)
for (j in 1:L){
  game <- d_overlp$gameID[j]
  indx <- which(d_scores$gameID == game)[1]
  d_overlp$day_score[j] <- d_scores$day[indx]
}
head(d_overlp)
#The labels for date in Kscores are 22 days ahead of the lables in scores
head(Kscores)
head(scores)
Kscores$Day <- Kscores$DayNum - 22

#Construct a data frame of regular season games with games from scores and Kscores
#First collect the games from scores
L<- length(scores$TeamID)
reg_ssn <- data.frame(matrix(nrow = L/2,ncol = 6, data = 0))
names(reg_ssn) <- c("Day", "Home", "Visitor", "Hscore", "Vscore", "Neutral")
head(scores)
for (j in seq(2,L, by = 2)){
  HID <- scores$TeamID[j]
  VID <- scores$TeamID[j-1]
  Hscore <- scores$Final[j]
  Vscore <- scores$Final[j-1]
  day <- scores$Day[j]
  neut <- ifelse(scores$VH[j] == "N", 1, 0)
  reg_ssn[j/2,] <- c(day, HID, VID, Hscore, Vscore, neut)
}
head(reg_ssn)
head(scores)

#Collect games from Kscores that were not identified in scores:
Kscores1 <- Kscores[!overlp,]
L <- length(Kscores1$Season)
reg_ssn1 <- data.frame(matrix(nrow = L,ncol = 6, data = 0))
names(reg_ssn1) <- c("Day", "Home", "Visitor", "Hscore", "Vscore", "Neutral")
head(Kscores1)
for (j in 1:L){
  date <- Kscores1$Day[j]
  if (Kscores1$WLoc[j] %in% c("H", "N")){
    HID <- Kscores1$WTeamID[j]
    VID <- Kscores1$LTeamID[j]
    Hscore <- Kscores1$WScore[j]
    Vscore <- Kscores1$LScore[j]
  }else{
    HID <- Kscores1$LTeamID[j]
    VID <- Kscores1$WTeamID[j]
    Hscore <- Kscores1$LScore[j]
    Vscore <- Kscores1$WScore[j]
  }
  neut <- ifelse(Kscores1$WLoc[j] == "N", 1, 0)
  reg_ssn1[j,] <- c(date, HID, VID, Hscore, Vscore, neut)
}
head(reg_ssn1)
#Combine games from the two data sets
reg_ssn <- rbind(reg_ssn, reg_ssn1)
reg_ssn$Diff <- reg_ssn$Hscore - reg_ssn$Vscore
head(reg_ssn)
summary(reg_ssn)

file <- paste("reg_ssn", yr, ".csv", sep = "")
write.table(reg_ssn, file, col.names = TRUE, row.names = FALSE, sep = ",")
