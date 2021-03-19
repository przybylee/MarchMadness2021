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

#Enter a string name ID closest to the string
ID_search<- function(string, incl.name = TRUE){
  char_min <- stringdist::stringdist(string, DTeams$TeamName)
  indx <- which.min(char_min)
  tm <- DTeams$TeamName[indx][1]
  tmid <- DTeams$TeamID[indx][1]
  if(incl.name){
    data.frame(Team = tm, TeamID = tmid)
  }else{
    tmid
  }
}

HAWP_str <- function(string1, string2){
  d1 <- ID_search(string1)
  d2 <- ID_search(string2)
  id1 <- as.numeric(d1$TeamID[1])
  id2 <- as.numeric(d2$TeamID[1])
  d3 <- rbind(d1, d2)
  wp1 <- ha1_wprob(id1,id2)
  wp2 <- 1-wp1
  d3$Wprob <- c(wp1, wp2)
  d3
}

