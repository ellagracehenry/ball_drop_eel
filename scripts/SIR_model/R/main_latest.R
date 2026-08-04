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
library(ggformula)

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
coefs[1] <- -2.56 #fr intercept NA est
coefs[2] <- -2.06 #fr log distance from ball NA est
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
n_sims <- 3000
eligible_drops <- 1
n_time <- 200

#METHOD 1: NLL GRID SEARCH
#Parameter grid 
param_grids <- expand.grid(social_threshold = seq(0,10,0.5))
fixed <- expand.grid(tr = c(5), tm = c(4), fractional_contagion_first = c(TRUE), fractional_contagion_subs = c(TRUE), max_rate = max_rate, dt = dt, da = da)
param_list <- split(param_grids, seq(nrow(param_grids)))
starting_values <- param_list[[1]]
starting_values <- unlist(starting_values)

ID <- unique(data$drop_ID)

sample_frac(as.data.frame(unique(data$drop_ID)), 0.8)

data_clean_f <- data_clean %>% filter(drop_ID %in% sample(unique(data$drop_ID), size = length(unique(data$drop_ID))*0.8))

#reloading
data_clean_f <- data_clean %>% filter(drop_ID %in% unique(data_clean_f1$drop_ID))

drop1_initator_responder <- initator_responder %>% filter(trial_ID %in% c(1))

#Save alp inputs
write.csv(param_grids, "social_threshold_param_6000sim_frNA_noK_0-10.csv")
save(data_clean_f, initator_responder, coefs,
     n_sims, fixed, n_time, file = "SIR_inputs_6000sim_frNA_noK_0-10.RData")


#Fit each model type from same starting vals with maximum likelihood
cl <- makeCluster(6, outfile = "parallel_log_frNA_noK_fine.txt")     # set the number of processor cores
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
  "data_clean_f",
  "initator_responder", 
  "coefs", 
  "n_sims",
  "n_time",
  "fixed",
  "param_list",
  "drop1_clean",
  "drop1_initator_responder"
))

out_nll <- foreach (i = 1:length(param_list), .combine = 'c') %dopar% {
  starting_values <- param_list[[i]]
  starting_values <- unlist(starting_values)
  
  res <- cascade_size_nll(starting_values, social_private_model, data_clean_f, initator_responder, coefs, n_sims, fixed, n_time)
  
  res

}


#Saving data
saveRDS(out_nll, file = "sp_coarse_frNA_noK_s3000.rds")
write.csv(param_grids, file = "sp_coarse_frNA_noK_s3000_params.csv")
write.csv(data_clean_f[,1:33], file = "sp_coarse_frNA_noK_training_data.csv")
nll_vector <- unlist(out_nll[seq(1, length(out_nll), by = 2)])
write.csv(nll_vector, file = "sp_coarse_sp_coarse_frNA_noK_s3000_nll.csv")

#reading in Alp data
#Method 1
result_files <- list.files("~/Desktop/PhD/academic_projects/ball_drop_eel/data/social_threshold_param_frNA_noK-5-9/output_1", pattern = "^result_\\d+\\.rds$", full.names = TRUE)
# Sort by task_id numerically so the list is in a sensible order
task_ids <- as.integer(gsub(".*result_(\\d+)\\.rds", "\\1", result_files))
result_files <- result_files[order(task_ids)]
all_results <- lapply(result_files, readRDS)
names(all_results) <- paste0("task_", sort(task_ids))
saveRDS(all_results, "all_sweep_results.rds")

#Method 2
flat_results <- unlist(all_results, recursive = FALSE)

# Save the combined summary
write.csv(results_table, "combined_sweep_results.csv", row.names = FALSE)

# Best-fitting parameter combination:
results_table %>% filter(nll == min(nll, na.rm = TRUE))

#Reading in previous full RDS
out_nll <- readRDS(file = "/Users/ellag/Desktop/PhD/academic_projects/ball_drop_eel/data/output_10000sim_frNA_noK_3-5_SocialPrivate/result_23_254.076843223296.rds")

##Saving cascade size
# 1. Identify indices for all model_result objects (2, 4, 6, ...)
model_res_indices <- seq(2, length(out_nll), by = 2)

# 2. Extract simulations safely
all_params_results <- map_df(seq_along(model_res_indices), function(p_idx) {
  
  # Grab model_result for parameter set p
  mod_res <- out_nll[[model_res_indices[p_idx]]]
  
  # Loop over all trials within this parameter set
  map_df(seq_along(mod_res), function(t_idx) {
    
    # Safely get trial ID (handles either named lists or numeric indices)
    #trial_id <- names(mod_res)[t_idx] %||% experimental_cascade_size$drop_ID[t_idx] %||% experimental_cascade_size$drop_ID[[t_idx]]
    trial_id <- as.character(experimental_cascade_size$drop_ID[t_idx])
    
    # Calculate cascade sizes for all 3,000 sims in this trial
    sim_sizes <- map_dbl(mod_res[[trial_id]], ~ sum(!is.na(.x[,1])))
    
    # Safely pull a SINGLE experimental value matching this drop_ID
    exp_val_matches <- experimental_cascade_size$n_responders[experimental_cascade_size$drop_ID == trial_id]
    
    exp_val <- if (length(exp_val_matches) > 0) exp_val_matches[1] else NA_real_
    
    tibble(
      param_set = p_idx,
      trial_id = trial_id,
      sim_idx = seq_along(sim_sizes),
      sim_cascade_size = sim_sizes,
      exp_cascade_size = exp_val,
      cascade_diff = sim_sizes - exp_val
    )
    
  })
})

# Save output
write.csv(all_params_results, "sp_coarse_frNA_noK_s3000_cascade_size_sims.csv", row.names = FALSE)


#distribution check
out_nll[["task_24.nll"]]
mod_res_2 <- out_nll[["model_result"]]

mod_res_2 <- out_nll[["model_result"]]
# Choose your trial (change 1 to whichever trial index you want to inspect)
trial_idx <- 228
# Extract the cacade size (number of responding eels) for all 3,000 sims
cascade_sizes <- sapply(mod_res_2[[as.character(trial_idx)]], function(sim_res) {
  sum(!is.na(sim_res[,1]))
})
cascade_diff <- cascade_sizes - experimental_cascade_size$n_responders[experimental_cascade_size$drop_ID == trial_idx]
hist(cascade_diff)

chosen_param_results <- all_params_results %>% filter(param_set == 1)

gf_histogram(~cascade_diff, data = chosen_param_results) %>%
  gf_facet_grid(trial_id ~ .)


avg_sim_cascade <- chosen_param_results %>% 
  group_by(trial_id) %>%
  summarise(avg_cascade_diff = mean(cascade_diff), experimental_cascade_size = first(exp_cascade_size), avg_cascade_size = mean(sim_cascade_size))

plot(avg_sim_cascade$experimental_cascade_size, avg_sim_cascade$avg_cascade_diff, xlab = "Actual cascade size", ylab = "Average simulated cascade size - actual cascade size")

plot(avg_sim_cascade$experimental_cascade_size, avg_sim_cascade$avg_cascade_size, xlab = "Actual cascade size", ylab = "Average simulated cascade size")

#who responded the most
drop2modres <- mod_res_2[[2]]
d <- sapply(drop2modres, function(sim_identity) {
  #getting proportion of hides
  #!is.na(sim_identity[,1])
  #order of hides
  sim_identity[,1]
})

#sum of hides
d <- as.data.frame(d)
d$j <- rowSums(d[2:10000], na.rm=TRUE)/10000
d[1:10000] <- NULL

#Order of hides
d <- as.data.frame(d)
d$j <- rowMeans(d[2:10000], na.rm=TRUE)
d[1:10000] <- NULL

d2 <- data.frame(drop2$eel_ID, drop2$rank_order)

group_cascade_size <- data_clean %>%
  group_by(drop_ID) %>%
  summarise(n_emerged = first(inst_emerged), n_response = first(n_responders)/first(inst_emerged))

plot(group_cascade_size$n_emerged, group_cascade_size$n_response)

plot(nll_vector)

social_private_param_results2 <- do.call(rbind.data.frame, param_list)
colnames(social_private_param_results2) <- c("social_threshold")
social_private_param_results2 <- cbind(social_private_param_results2, nll_vector)
plot(social_private_param_results2$social_threshold, social_private_param_results2$nll_vector)

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

model_result <- social_private_model(data_clean_f, initator_responder, starting_values, coefs, n_sims, fixed, n_time)

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

         