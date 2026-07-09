## Set up ##
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
library("optimParallel")

#Scripts
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/sharededge.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/models/private_model.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/models/social_model.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/models/social_private_model.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/manipulate_data.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/generate_initator_responder_data.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/statistical_analysis/bayes_main.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/cascade_size_nll.R")

##Load in data
setwd("/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/My Drive/Colorado/PhD/PROJECTS/ball_drop_garden_eel/triangulation/final_triangulation")
data <- read_excel("final_final_master_ball_drop_3D.xlsx") %>%
  filter (drop_ID != 146) %>%
  filter(drop_ID != 176) %>%
  filter(drop_ID != 157) %>%
  filter(drop_ID != 147) %>%
  filter(drop_ID != 180) %>%
  filter(drop_ID != 149) #%>%
  #filter(trial_ID != 5) %>%
  #filter(trial_ID != 17) #adding in seems to completely reverse the results. Check why. 

#Check how many unique drops per colony
data %>%
  group_by(colony) %>%
  summarise(n_drop_ids = n_distinct(drop_ID))

##Manipulate data
data_clean <- manipulate_data(data)
initator_responder <- generate_initator_responder_data(data_clean)

data_clean$log_distance_to_ball_sc <- scale(data_clean$log_distance_to_ball)
data_clean$log_inst_topo_dist_from_first_sc <- scale(data_clean$log_inst_topo_dist_from_first)
orig_topo_mean <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:center")
orig_topo_sd   <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:scale")

data <- data_clean
##Statistical model fit - choose model, get parameters for first and second responder model
coefs <- get_coefs(data_clean)
coefs <- list() #(all on scaled!)
coefs[1] <- -2.92 #fr intercept
coefs[2] <- -0.75 #fr log distance from ball
coefs[3] <- -3.71 #sr intercept
coefs[4] <- -0.58 #sr log inst topo dist
coefs[5] <- -0.54 #sr log distance from ball

##Define parameter sets
#Frame range of cascades
ranges <- data_clean %>%
  group_by(drop_ID) %>%
  summarise(range = max(response_frame_cam1, na.rm=TRUE) - min(response_frame_cam1, na.rm=TRUE))

#Constants
max_rate <- 1
dt <- 1
da <- 1
n_sims <- 10
eligible_drops <- 1

#Parameter grid
param_grids <- expand.grid(ball_decay_time_coef = c(0.2), social_decay_time_coef = c(0.2), private_threshold = c(0.01), social_threshold = c(0.7))
fixed <- expand.grid(range = max(ranges$range), tr = c(5), tm = c(4), fractional_contagion_first = c(TRUE), fractional_contagion_subs = c(TRUE))
param_list <- split(param_grids, seq(nrow(param_grids)))
  
starting_values <- param_list[[i]]
starting_values <- unlist(starting_values)
  
#Fit each model type from same starting vals with maximum likelihood
cl <- makeCluster(5)     # set the number of processor cores
setDefaultCluster(cl=cl) # set 'cl' as default cluster
parallel::clusterEvalQ(cl, {
  library(dplyr)
  library(magrittr)
})
sp_fit <- optimParallel(par = starting_values, fn = cascade_size_nll, model = social_private_model, data_clean = data_clean, coefs = coefs, n_sims = n_sims, fixed = fixed)

sp_fit <- optim(par = starting_values, fn = cascade_size_nll, model = social_private_model, data_clean = data_clean, coefs = coefs, n_sims = n_sims, fixed = fixed)


p_fit <- optim(par = starting_values, fn = cascade_size_nll, model = private_model, data_clean = data_clean, coefs = coefs, n_sims = n_sims, fixed = fixed)
s_fit <- optim(par = starting_values, fn = cascade_size_nll, model = social_model, data_clean = data_clean, coefs = coefs, n_sims = n_sims, fixed = fixed)

#Likelihood inference
#For each pair of models in a set
#Calculate the likelihood ratio
exp(-model1_nll)/exp(-model2_nll)
#result - how many times more likely is model 1 compared to model 2
  
#Could even do likelihood profiling across parameter values.... for values of the focal parameter, train the model for other parameters, caclulate the likelihood ratio for each, plot the profile of likelihood ratio




