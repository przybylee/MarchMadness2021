#Lee Przybylski
#3/18/2021
library(ggplot2)
library(MASS)

#This is a script for validation of the harville method using the last few days of the season
#Fit a model for the Harville method using data from a regular ssn
#Enter tournament year
yr <- 2021
file <- paste("reg_ssn", yr, ".csv", sep = "")
#Read in regular ssn data
reg_ssn <- read.csv(file, header = TRUE, sep = ",")
head(reg_ssn)
tail(reg_ssn)

#Fill in a matrix X to perform ols regression
#For each row, we put a 1 under home team, -1 under visitor, and 1 under homecourt
#if game was not played at a neutral site

IDs <- unique(c(reg_ssn$Home, reg_ssn$Visitor))
N <- length(reg_ssn$Day)
M <- length(IDs)
X = matrix(data = 0, nrow = N, ncol = M+1)
colnames(X) <- c(IDs, "HCA")
Y <- rep(0, N/2)

for(n in 1:N){
  home <- as.character(reg_ssn$Home[n])
  away <- as.character(reg_ssn$Visitor[n])
  hca <- 1- reg_ssn$Neutral[n] 
  X[n,"HCA"] <- hca
  X[n, away] <- -1
  X[n, home] <- 1
}
Y <- reg_ssn$Diff

ha_reg1 <- lm(Y ~ X+0)
summary(ha_reg1)

reg_ssn$Residuals <- residuals(ha_reg1)
head(reg_ssn)
plot(reg_ssn$Day, reg_ssn$Residuals)
#No obvios pattern for these residuals over time


#This function extracts a team effect given the ID
ha1_team_eff <- function(tmid){
  c <- rep(0, M+1)
  indx <- colnames(X) == as.character(tmid)
  c[indx] <- 1
  X.X <- t(X)%*%X
  cb <- t(c)%*%ginv(X.X)%*%t(X)%*%Y
  cb[1,1]
}

#Predict the difference in team effects with or without homecourt
#When neutral is false id1 gets homecourt advantage
ha1_team_diff <- function(id1,id2,neutral = TRUE){
  if(neutral){
    ha1_team_eff(id1) - ha1_team_eff(id2)
  }else{
    cvec <- rep(0, M)
    cvec <- c(cvec,1)
    X.X <- t(X)%*%X
    cb <- t(cvec)%*%ginv(X.X)%*%t(X)%*%Y
    ha1_team_eff(id1) - ha1_team_eff(id2)+cb[1,1]
  }
}

#Requires model matrix X, observations y, estimated line d, and confidence level a
conf_int=function(X,y,c,d=0,a=0.05){
  reg <- lm(y ~ X+0)
  c = as.matrix(c)
  X.X <- t(X)%*%X
  df = nrow(X) - anova(reg)[1,1]
  #df= nrow(X) - rankMatrix(X)[1]
  cb.d=t(c)%*%ginv(X.X)%*%t(X)%*%y-d
  sigsq <- anova(reg)[2,3]
  var.cb <- sigsq*t(c)%*%ginv(X.X)%*%c
  tquant <- qt(1-a/2,df)
  std.dev = sqrt(var.cb)
  lower = cb.d - tquant%*%std.dev
  upper = cb.d + tquant%*%std.dev
  data.frame(est = cb.d, std.dev = std.dev, lower = lower, upper = upper)
}

#Predict the win probability of id1 beating id2.  
#When neutral is False, assume id1 is home.
ha1_wprob <- function(id1,id2, neutral = TRUE){
  residuals <- reg_ssn$Residuals
  wins <- sum(residuals + ha1_team_diff(id1,id2, neutral)>0)
  prob <- wins/length(residuals)
  prob
}
head(reg_ssn)
head(scores)
ha1_team_eff(1139)
ha1_team_eff(1)
ha1_team_diff(1444,1439, neutral = FALSE)
ha1_wprob(1444,1439, neutral = FALSE)
head(reg_ssn)
