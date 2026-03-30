#Packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)
library(MASS) 


source("scripts/ABM/R/activation_function.R")

#Simulate colony
colony <- matrix(1:23)
#Simulate their pre states
colony <- cbind(colony,rbinom(n = 23, size = 1, prob = 0.7))
#Simulate their positions
colony <- cbind(colony,runif(23,0,10))
colony <- cbind(colony,runif(23,0,10))
#Calculate weights

#Add headers
colnames(colony) <- c("individual_ID","state","x","y")


#Simulate the ball landing at a random position

#Draw first responder using the parameter from the likelihood of being a first responder with distance from ball (logistic regression 1 or 0 first responder with distance from ball)

#For each ms after first responder, compute the dose experience by each individual. 
#Set a dose threshold (can simulate across) and if inidividual dose exceeds this, it is activated - flees

#if flees, this acts to affect the dose of the individuals around it.

#compute cascade size across many simulations of group size and dose threshold

#see how these cascade size distributions compare to real data