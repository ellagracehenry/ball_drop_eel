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
library(bayesplot)
library(foreach)
library(doParallel)
library(EasyABC)
library("coda")
library(bayesplot)

#Scripts
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/sharededge.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/models/private_model.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/models/social_model.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/models/social_private_model.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/manipulate_data.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/generate_initator_responder_data.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/statistical_analysis/bayes_main.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/cascade_size_nll.R")
source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/msnipulate_data_raw.R")

##Load in data
setwd("/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/My Drive/Colorado/PhD/PROJECTS/ball_drop_garden_eel/triangulation/final_triangulation")
data <- read_excel("final_final_master_ball_drop_3D.xlsx") %>%
  filter (drop_ID != 146) %>% #fish in frame
  filter(drop_ID != 176) %>% #pre hide
  filter(drop_ID != 157) %>% #tangled line
  filter(drop_ID != 147) %>% #pre hide
  filter(drop_ID != 180) %>% #ball goes in leaves
  filter(drop_ID != 149) %>% #pre hide
  filter(drop_ID != 41) %>% #pre hide
  filter(drop_ID != 42) %>% #pre hide
  filter(trial_ID != 12) #%>% #terrible retriangulation of ball
  #filter(trial_ID != 5) #%>%
  #filter(trial_ID != 17) #adding in seems to completely reverse the results. Check why. 

##Manipulate data
data_clean <- manipulate_data(data)
data_clean <- manipulate_data_raw(data)
initator_responder <- generate_initator_responder_data(data_clean)

#Check how many unique drops per colony
data_clean %>%
  group_by(colony, drop_ID) %>%
  summarise(any_response_drop = any(any_response), .groups = "drop_last") %>%
  group_by(colony) %>%
  summarise(n_drop_ids = n(), n_responses = sum(any_response_drop))

data_clean$log_distance_to_ball_sc <- scale(data_clean$log_distance_to_ball)
data_clean$log_inst_topo_dist_from_first_sc <- scale(data_clean$log_inst_topo_dist_from_first)
orig_topo_mean <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:center")
orig_topo_sd   <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:scale")

data <- data_clean
##Statistical model fit - choose model, get parameters for first and second responder model
coefs <- get_coefs(data_clean)
coefs <- list() #(all on scaled!)
coefs[1] <- -2.93 #fr intercept
coefs[2] <- -0.73 #fr log distance from ball
coefs[3] <- -4.11 #sr intercept
coefs[4] <- -0.64 #sr log inst topo dist
coefs[5] <- -0.64 #sr log distance from ball


##Define parameter sets
#Frame range of cascades
ranges <- data_clean %>%
  group_by(drop_ID) %>%
  summarise(range = max(response_frame_cam1, na.rm=TRUE) - min(response_frame_cam1, na.rm=TRUE))

#Constants
max_rate <- 1
dt <- 1
da <- 1
n_sims <- 2000
eligible_drops <- 1
n_time <- 100

#METHOD 1: NLL GRID SEARCH
#Parameter grid 
param_grids <- expand.grid(social_threshold = seq(0,5,0.1))
fixed <- expand.grid(tr = c(5), tm = c(4), fractional_contagion_first = c(TRUE), fractional_contagion_subs = c(TRUE), max_rate = max_rate, dt = dt, da = da)
param_list <- split(param_grids, seq(nrow(param_grids)))
starting_values <- param_list[[1]]
starting_values <- unlist(starting_values)

data_clean <- data_clean %>% filter(drop_ID %in% c(2))

drop1_initator_responder <- initator_responder %>% filter(trial_ID %in% c(1))

#Fit each model type from same starting vals with maximum likelihood
cl <- makeCluster(7, outfile = "parallel_log_1.txt")     # set the number of processor cores
setDefaultCluster(cl=cl) # set 'cl' as default cluster
registerDoParallel(cl)
clusterSetRNGStream(cl, iseed = 12345)
parallel::clusterEvalQ(cl, {
  library(dplyr)
  library(magrittr)
})
clusterExport(cl, varlist = c(
  "social_private_model", 
  "cascade_size_nll", 
  "data_clean", 
  "initator_responder", 
  "coefs", 
  "fixed",
  "param_list",
  "drop1_clean",
  "drop1_initator_responder"
))

out_nll <- foreach (i = 1:length(param_list), .combine = 'c') %dopar% {
  starting_values <- param_list[[i]]
  starting_values <- unlist(starting_values)
  
  nll <- cascade_size_nll(starting_values, social_private_model, drop1_clean, initator_responder, coefs, n_sims, fixed, n_time)
  
  nll

}

plot(out_nll)

social_private_param_results2 <- do.call(rbind.data.frame, param_list)
colnames(social_private_param_results2) <- c("ball_decay_time_coef", "social_decay_time_coef", "private_threshold", "social_threshold")
social_private_param_results2 <- cbind(social_private_param_results2, out_nll)
plot(social_private_param_results$ball_decay_time_coef, social_private_param_results$out_nll)

social_private_param_results <- social_private_param_results %>% filter(out_nll  < 4.605170)
ggplot(social_private_param_results2, aes(ball_decay_time_coef, social_decay_time_coef, fill = out_nll)) +
  geom_tile()
ggplot(social_private_param_results, aes(ball_decay_time_coef, social_threshold, fill = out_nll)) +
  geom_tile()
ggplot(social_private_param_results, aes(ball_decay_time_coef, private_threshold, fill = out_nll)) +
  geom_tile()

ggplot(social_private_param_results, aes(private_threshold, social_decay_time_coef, fill = out_nll)) +
  geom_tile()
ggplot(social_private_param_results, aes(private_threshold, social_threshold, fill = out_nll)) +
  geom_tile()

ggplot(social_private_param_results, aes(social_threshold, social_decay_time_coef, fill = out_nll)) +
  geom_tile()


param_grids <- expand.grid(ball_decay_time_coef = c(2), social_decay_time_coef = c(3), private_threshold = c(0.02), social_threshold = c(3))
fixed <- expand.grid(tr = c(5), tm = c(4), fractional_contagion_first = c(TRUE), fractional_contagion_subs = c(TRUE), max_rate = max_rate, dt = dt, da = da)
param_list <- split(param_grids, seq(nrow(param_grids)))
starting_values <- param_list[[1]]
starting_values <- unlist(starting_values)

sp_fit1 <- optim(par = starting_values, fn = cascade_size_nll, model = social_private_model, data_clean = drop1_clean, initator_responder = drop1_initator_responder, coefs = coefs, n_sims = n_sims, fixed = fixed, n_time = n_time)

starting_values <- unlist(sp_fit1$par)
sp_fit1 <- optim(par = starting_values, fn = cascade_size_nll, model = social_private_model, data_clean = drop1_clean, initator_responder = drop1_initator_responder, coefs = coefs, n_sims = n_sims, fixed = fixed, n_time = n_time)

sp_fit <- optimParallel(par = starting_values, fn = cascade_size_nll, model = social_private_model, data_clean = data_clean, initator_responder = initator_responder, coefs = coefs, n_sims = n_sims, fixed = fixed, n_time = n_time, control = list(
  trace = 1, ndeps = rep(0.02, 4)))

sp_fit <- optim(par = starting_values, fn = cascade_size_nll, model = social_private_model, data_clean = data_clean, initator_responder = initator_responder, coefs = coefs, n_sims = n_sims, fixed = fixed, n_time = n_time)

estimates <- sp_fit$par
names(estimates) <- names(starting_values)
print(estimates)

#NLL mod selection
k <- length(sp_fit$par)
nll <- sp_fit$value

AIC_val <- 2 * k + 2 * nll
cat("AIC:", AIC_val)

p_fit <- optim(par = starting_values, fn = cascade_size_nll, model = private_model, data_clean = data_clean, coefs = coefs, n_sims = n_sims, fixed = fixed)
s_fit <- optim(par = starting_values, fn = cascade_size_nll, model = social_model, data_clean = data_clean, coefs = coefs, n_sims = n_sims, fixed = fixed)

#Likelihood inference
#For each pair of models in a set
#Calculate the likelihood ratio
exp(-model1_nll)/exp(-model2_nll)
#result - how many times more likely is model 1 compared to model 2

#Comparing null/simpler model to full model - Likelihood ratio test with test statistic D (deviance)
# Extract NLL values
nll_null <- null_fit$value
nll_full <- full_fit$value

# Calculate the Deviance statistic (D)
deviance_stat <- 2 * (nll_null - nll_full)

# Calculate Degrees of Freedom (difference in number of parameters estimated)
df_diff <- length(null_fit$par) - length(full_fit$par)
df_diff <- abs(df_diff) # Ensure positive integer

# Calculate the p-value using the Chi-squared distribution
p_val <- pchisq(deviance_stat, df = df_diff, lower.tail = FALSE)

# Display the comparison table
cat("--- Model Comparison via Likelihood Ratio Test ---\n")
cat("Null Model NLL: ", nll_null, "\n")
cat("Full Model NLL: ", nll_full, "\n")
cat("Test Statistic (LRT):", deviance_stat, "\n")
cat("Degrees of Freedom:  ", df_diff, "\n")
cat("p-value:             ", p_val, "\n")

#If p <0.05, adding social significantly improves model fit.
#Could even do likelihood profiling across parameter values.... for values of the focal parameter, train the model for other parameters, caclulate the likelihood ratio for each, plot the profile of likelihood ratio



#METHOD 2: Approximate bayesian computing
#Summary stat target is the experimental outcome
experimental_counts <- drop1_clean %>%
  group_by(drop_ID) %>%
  summarise(n_responders = first(n_responders)) %>%
  pull(n_responders)

#SIR cascade count wrapper
sim_cascade_count_wrapper <- function(prior) {
  
  model_result <- social_private_model(drop1_clean, drop1_initator_responder, prior, coefs, n_sims, fixed, n_time)
  
  experimental_cascade_size <- drop1_clean %>%
    group_by(drop_ID) %>%
    summarise(n_responders = first(n_responders))
  
  sim_sums_all<- NULL
  iii <- 1
  
  for (ii in experimental_cascade_size$drop_ID) {
    # Use sapply instead of an explicit loop to calculate matches in one go
    #Extract the cascade size in the simulations
    sim_sums <- sapply(model_result[[ii]], function(sim_res) sum(!is.na(sim_res)))
    sim_sums_all[iii] <- sim_sums
    iii <- iii + 1
  }
  return(sim_sums_all)
}

model_result <- social_private_model(drop1_clean, drop1_initator_responder, starting_values, coefs, n_sims, fixed, n_time)

sim_cascade_count_wrapper(par = starting_values, model = social_private_model, data_clean = data_clean, initator_responder = initator_responder, coefs = coefs, n_sims = n_sims, fixed = fixed, n_time = n_time)

priors_list <- list(
  c("unif",0,15),
  c("unif",0,15),
  c("unif",0,1),
  c("unif",0,10)
)

mcmc_results <- ABC_mcmc(
  method = "Marjoram_original",
  model = sim_cascade_count_wrapper,
  prior = priors_list,
  summary_stat_target = experimental_counts,
  n_rec = 1000,
  dist_max = 2,
  progress_bar = TRUE
)

#accepted params (throw away first 20%)
param_1_post_burn_in <- mcmc_results$param[,1]
quantile(param_1_post_burn_in, probs = c(0.025, 0.975)) #extract final 95% thresholds for each parameter


clean_samples <- mcmc_results$param[1:200, ]

# Convert the EasyABC matrix into an official MCMC object
mcmc_chain <- as.mcmc(clean_samples )

# This single command generates both trace and marginal plots for all parameters
plot(mcmc_chain)




colnames(clean_samples) <-c("ball_decay", "social_decay", "private_thresh", "social_thresh")
  
# 1. Marginal Density Plots (with 50% and 95% shaded intervals)
mcmc_areas(clean_samples, 
           pars = c("ball_decay", "social_decay", "private_thresh", "social_thresh"),
           prob = 0.95)

# 2. Publication-grade Trace Plots
mcmc_trace(clean_samples, 
           pars = c("ball_decay", "social_decay", "private_thresh", "social_thresh"))

         