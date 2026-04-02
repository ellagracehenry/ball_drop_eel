#Packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)
library(MASS) 
library(readxl)
library(lme4)
library(glmmLasso)

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


#Fit a model for initator responder pairs
setwd("~/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/Shared drives/Field Research Videos/Gil Lab/Raw_Data/Curacao_2024/garden_eels/position_drop_experiment")
data <- read_excel("master_ball_drop_data_3D_0216.xlsx")

data$distance_to_ball <- sqrt((data$base_X - data$ball_hit_X)^2 + (data$base_Y - data$ball_hit_Y)^2 + (data$base_Z - data$ball_hit_Z)^2)

#Summarise data by initiator-responder pairs
initator_responder <- data %>%
  group_by(drop_ID) %>%
  mutate(responses = sum(full_partial_none > 0, na.rm=TRUE)) %>%
  filter(responses > 0 & !is.na(full_partial_none)) %>%
  filter(!trial_ID == 11) %>%
  mutate(first_index = if (any(full_partial_none != 0 & !is.na(full_partial_none)))
    which.min(ifelse(full_partial_none != 0 & !is.na(full_partial_none),
                     response_frame_cam1, Inf))
    else NA_integer_) %>%
  mutate(
    rank_order = rank(response_frame_cam1, na.last = "keep", ties.method = "first"),
    initator = as.integer(rank_order == 1),
    second_responder = as.integer(rank_order == 2),
    first_x = base_X[first_index],
    first_y = base_Y[first_index],
    first_z = base_Z[first_index],
    # Compute distance only for responders
    dist_from_first_resp = ifelse(
    !is.na(full_partial_none),
    sqrt((base_X - first_x)^2 + (base_Y - first_y)^2 + (base_Z - first_z)^2),
    NA_real_
    )
  ) %>%
  filter(!is.na(dist_from_first_resp) & dist_from_first_resp > 0)%>%
  ungroup()

initator_responder$colony_eel_ID <- paste(initator_responder$colony, initator_responder$eel_ID, sep = "_")

initator_responder <- initator_responder[
  complete.cases(initator_responder[,c(
    "second_responder",
    "dist_from_first_resp",
    "trial_ID",
    "distance_to_ball"
  )]),
]

initator_responder$trial_ID <- as.factor(initator_responder$trial_ID)

initator_responder$colony <- as.factor(initator_responder$colony)

initator_responder$colony_eel_ID <- as.factor(initator_responder$colony_eel_ID)

# testing random effects structure
intercepts_random_model <- glmer(second_responder ~ 1 + (1 | colony) + (1 | colony_eel_ID), family = binomial, data = initator_responder)
summary(intercepts_random_model)

#test model
test_model <- glmmLasso(
  fix = second_responder ~ dist_from_first_resp + distance_to_ball,
  rnd = list(colony = ~1, colony_eel_ID = ~1),
  family = binomial(),
  data = initator_responder,
  lambda = 2
  # no control/start - uses defaults
)

summary(test_model)

#Scan over a range of lambdas
lambda_grid <- seq(1,3,by=1)
#Scan over a range of lambdas
lambda_grid <- seq(0.5,10,by=0.5)

#Fit model for lambda using default starting values
models <- lapply(lambda_grid, function(lambda) {
       glmmLasso(
             fix = second_responder ~ dist_from_first_resp + distance_to_ball,  # full model formula
             rnd = list(colony = ~1, colony_eel_ID = ~1),
             family = binomial(),
             data = initator_responder,
             lambda = lambda
         )
   })


#extract deviance:
deviances <- sapply(models, function(m) {m$deviance})

best_lambda <- lambda_grid[which.min(deviances)]
best_model <- models[[which.min(deviances)]]
