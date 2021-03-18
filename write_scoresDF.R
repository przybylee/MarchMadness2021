#Clean the scores data

scores <- read.csv("Scores2020-2021.csv", header = TRUE, sep = ",")

L <- length(scores$Team)
C_ids <- as.character(rep(0,L))
t0 <- Sys.time()
for (j in 1:L){
  string <- scores$Team[j]
  C_ids[j] <- scores_to_ID(string)
  print(paste(j, "of", L, "IDs found"))
}
tf <- Sys.time()

print(tf -t0)