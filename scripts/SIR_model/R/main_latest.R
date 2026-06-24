#Set up
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
library(tibble)
library(stringr)
library(pracma)
library(spatstat)
library(deldir)
library(MuMIn) #https://ecologyforacrowdedplanet.wordpress.com/2013/08/27/r-squared-in-mixed-models-the-easy-way/
library(brms)
library(loo)
library(pROC)
library(rstanarm)
library(report)
library(tidybayes)

#Scripts
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/sharededge.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/models/private_model.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/models/social_model.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/models/social_private_model.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/manipulate_data.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/generate_initator_responder_data.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/statistical_analysis/bayes_main.R")

#load in data
setwd("/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/My Drive/Colorado/PhD/PROJECTS/ball_drop_garden_eel/triangulation/final_triangulation")
data <- read_excel("final_master_ball_drop_3D.xlsx") %>%
  filter(drop_ID != 152) %>%
  filter (drop_ID != 169) %>%
  filter (drop_ID != 146) %>%
  filter(drop_ID != 176) %>%
  filter(drop_ID != 157) %>%
  filter(drop_ID != 147) %>%
  filter(drop_ID != 180) %>%
  filter(drop_ID != 179) %>%
  #filter(trial_ID != 5) %>%
  filter(drop_ID != 173) %>%
  filter(trial_ID != 17) %>% #needs correcting annotations %>%
  filter(drop_ID != 149)  #two 156s?

#Check how many unique drops per colony
data %>%
  group_by(colony) %>%
  summarise(n_drop_ids = n_distinct(drop_ID))

#manipulate data
data_clean <- manipulate_data(data)
initator_responder <- generate_initator_responder_data(data_clean)

#Statistical model fit - choose model, get parameters for first and second responder model
coefs <- get_coefs(data_clean)

#Define parameter sets
#Frame range of cascades
ranges <- data_clean %>%
  group_by(drop_ID) %>%
  summarise(range = max(response_frame_cam1, na.rm=TRUE) - min(response_frame_cam1, na.rm=TRUE))

#Constants
max_rate <- 1
dt <- 1
da <- 1
n_sims <- 10

#Parameter grid
param_grids <- expand.grid(range = max(ranges$range), ball_decay_time_coef = c(-10,0,10), social_decay_time_coef = c(-10,0, 10), private_threshold = c(0,5,10), social_threshold = c(0,5,10), tr = c(1,5,10), tm = c(1,5,10), fractional_contagion_first = c(TRUE,FALSE), fractional_contagion_subs = c(TRUE,FALSE))
param_list <- split(param_grids, seq(nrow(param_grids)))

#For each parameter set

#For each model type

#Simulate 10000 times

#Calculate log probabilities of cascade order

#Sum the log probabilities across trials

