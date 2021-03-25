#Lee Przybylski
#3/18/2021
library(ggplot2)
library(MASS)

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

source("HA1_functions.R")
#To get functions for looking up teams load sourt functions
source("sort_functions.R")
head(reg_ssn)
head(scores)
ha1_team_eff(1139)
ha1_team_eff(1)
ha1_team_diff(1444,1439, neutral = FALSE)
ha1_wprob(1444,1439, neutral = FALSE)
head(reg_ssn)

#ID_search("Drake")
#ID_search("Witchita State")
#ha1_wprob(1179, 1455)
