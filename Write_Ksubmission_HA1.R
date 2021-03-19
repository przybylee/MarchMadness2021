#Lee Przybylski
#3/18/2021
options(stringsAsFactors = FALSE)
#Write a submission for the Kaggle competition with 68*67/2 win probabilities
yr <- 2021
file <- paste("TourneyTeams", yr, ".csv", sep = "")
d_teams <- read.csv(file, header = FALSE, sep = ",")
head(d_teams)
names(d_teams) <- c("Team")
d_teams$TeamName <- NA 
d_teams$TeamID <- 0

#d_teams$Team[1]
#ID_search("Texas Sout")
#name_search("Texas", DTeams$scores)
#DTeams[DTeams$scores == "TexasSouthern",]
L <- length(d_teams$Team)
for (j in 1:L){
  id <- d_teams$Team[j]
  d_teams[j,2:3] <- ID_search(id)
} 
d_teams

IDascd <- sort(d_teams$TeamID)

n <- length(IDascd)
N <- n*(n-1)/2
Ksub <- data.frame(matrix(nrow = N, ncol =2, data = NA))
names(Ksub) <- c("ID", "Pred")
i <- 1
for (j in 1:(n-1)){
  for (k in (j+1):n){
    id1 <- IDascd[j]
    id2 <- IDascd[k]
    label <- paste(yr, id1, id2, sep = "_")
    wprob <- ha1_wprob(id1, id2, neutral = TRUE)
    Ksub[i,] <- data.frame(ID = label, Pred = wprob)
    i <- i+1
  }
  print(paste(N-(i-1), "rows remain"))
}
head(Ksub, n = 10)
tail(Ksub, n = 20)

#Verify that the labels in my submission match the sample submission
path <- "Kaggle Data/MDataFiles_Stage2/MSampleSubmissionStage2.csv"
sample <-read.csv(path, header = TRUE, sep = ",")
head(sample)
sum(sample$ID!= Ksub$ID)
#IF the sum is 0, we are good

file <- paste("KsubmissionHA1", yr, ".csv", sep = "")
write.table(Ksub, file = file, row.names = FALSE, col.names = TRUE, sep = ",")
