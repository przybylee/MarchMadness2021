#Lee Przybylski 3/17/2021
#This script provides functions for associating the various team names with the TeamID 
#provided by Kaggle
#library(ggplot2)
#library(plyr)
library(tidyverse)
options(stringsAsFactors = F)

#Enter the year of the tournament for fitting the model
yr <- 2021
file <- paste("TeamList", yr, ".csv", sep = "")
DTeams <- read.csv(file, header = TRUE, sep = ",")
#head(DTeams)
#"Drake" %in% DTeams[1,6:8]

scores_to_ID <- function(str){
  indx <- DTeams$scores == str
  if (sum(indx) == 1){
    ID <- DTeams$TeamID[indx][1]
  }else {
    indx <- DTeams$scores2 == str
    if (sum(indx) == 1){
      ID <- DTeams$TeamID[1]
    }else {
      indx <- DTeams$scores3 == str
      if (sum(indx) == 1){
        ID <- DTeams$TeamID[1]
      }else{
        stop("Name not found")
      }
    }
  }
  ID    
}
scores_to_ID("AlabamaAM")
head(DTeams)

#Convert Bartorvik name to ID
bar_to_ID <- function(str){
  indx <- DTeams$bartorvik == str
  if(sum(indx) == 1){
    ID <- DTeams$TeamID[indx][1]
    ID
  }else{
    stop("Name not found")
}}
bar_to_ID("Arizona")

#Convert Name to ID
name_to_ID <- function(str){
  indx <- DTeams$TeamName == str
  if(sum(indx) == 1){
    ID <- DTeams$TeamID[indx][1]
    ID
  }else{
    stop("Name not found")
  }}
#name_to_ID("Alabama A&M")

#Convert ID to team
ID_to_name <- function(id){
  indx <- DTeams$TeamID == id
  if(sum(indx) == 1){
    name <- DTeams$TeamName[indx][1]
    name
  }else{
    stop("ID not found")
  }}
ID_to_name(1326)

#Here is a function for identifying the closest match between two vectors of strings
name_match <- function(x,y){
  char_min <- stringdist::stringdist(x, y)
  indx <- which.min(char_min)
  y[indx]
}
name_search <- function(x,y){
  y[grepl(x,y)]
}

#This functions lists the n best matches according to string dist
name_list <- function(name, list, n = 5){
  char_min <- stringdist::stringdist(name,list)
  results <- data.frame(cbind(list, char_min))
  results[order(char_min)[1:n],]
}