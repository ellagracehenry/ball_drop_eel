library(brms)
library(loo)
library(pROC)
library(rstanarm)
library(report)
library(tidybayes)

hpdi <- function (samp, prob = 0.95) {
  vals <- sort(samp)
  nsamp <- length(vals)
  gap <- max(1, min(nsamp - 1, round(nsamp * prob)))
  init <- 1:(nsamp - gap)
  inds <- which.min(vals[init + gap,drop=FALSE] - vals[init, drop=FALSE])
  ans <- cbind(lower=vals[inds], upper=vals[inds + gap])
  return(ans)
}

data_no_first <- data %>% filter(rank_order != 1)
data_no_first$rank_order <- data_no_first$rank_order - 1

data_no_first$rank_prop <- data_no_first$rank_order/data_no_first$n_responders

data_no_first <- data_no_first |>
  tidyr::drop_na(rank_prop, dist_from_first, distance_to_ball)

data_no_first$dist_from_first_sc <- scale(data_no_first$dist_from_first)
data_no_first$distance_to_ball_sc <- scale(data_no_first$distance_to_ball)

### Spread of responses within a group

# Model with interaction
full_interaction_spread_model <- brm(formula = rank_prop ~ 
                                       dist_from_first_sc + 
                                       distance_to_ball_sc + 
                           dist_from_first_sc:distance_to_ball_sc + 
                           (1|colony/colony_eel_ID) + 
                           (1|drop_ID) + 
                           (1|date), 
                         data = data_no_first, 
                         chains = 4,
                         cores = 4,
                         iter = 4000,
                         warmup = 2000,
                         save_pars = save_pars(all = TRUE),
                         family = Beta(),
                         #family = cumulative("logit"), works with prior(normal(0, 10), class = "Intercept")
                         #sample_prior = "only",
                         prior = c(#prior(normal(0, 1), class = "b"),
                                   #prior(normal(0, 10), class = "Intercept"),
                                   #prior(exponential(1), class = "sd")),
                                  # prior(normal(0, 1), class = "b"),          # fixed effects
                                  # #prior(normal(0, 1), class = "Intercept"),  # mean rank_prop on logit scale
                                  # #prior(exponential(1), class = "sd"),         # random effect SDs
                                  # prior(student_t(3, 0, 2.5),  class = "Intercept"),  # keep brms default
                                  # prior(student_t(3, 0, 2.5),  class = "sd"),          # keep brms default
                                  # prior(gamma(2, 0.1), class = "phi")),          # precision parameter
                         prior(normal(0, 1),         class = "b"),
                         prior(normal(0, 1),         class = "Intercept"),
                         prior(exponential(1),       class = "sd"),
                         prior(gamma(10, 0.5),       class = "phi")),
                         control = list(adapt_delta = 0.99)
)

#Checks
plot(full_interaction_spread_model)
summary(full_interaction_spread_model) #rhat < 1.01, bulk ess and tail ess > 400
pp_check(full_interaction_spread_model, type="dens_overlay",ndraws=500)
pp_check(full_interaction_spread_model, type = "stat", stat = "sd")
pp_check(full_interaction_spread_model, type = "scatter_avg")
pp_check(full_interaction_spread_model, ndraws = 100)

# outlier detection
loo_result <- loo(full_interaction_spread_model)
print(loo_result) #check all less than 0,7, if more they are surprising rows, check if usual
plot(loo_result)

# residuals
res <- residuals(full_interaction_spread_model)
plot(res[,1] ~ fitted(full_interaction_spread_model)[,1])
abline(h = 0, col = "red") #want random scatter around 0

#prior checks if sample_prior = "only"
pp_draws <- posterior_predict(full_interaction_spread_model, ndraws = 200)
                              
pp_check(full_interaction_spread_model,          # good for ordinal outcomes
         ndraws  = 200) +
  labs(title    = "Prior Predictive Check — Rank Order",
       subtitle = "Bars = observed  |  Points/lines = prior predictive draws",
       x        = "Rank order category",
       y        = "Count") +
  theme_bw()

# Model with both
full_both_spread_model <- brm(formula = rank_prop ~ 
      dist_from_first_sc + 
      distance_to_ball_sc + 
      (1|colony/colony_eel_ID) + 
      (1|drop_ID) + 
      (1|date), 
    data = data_no_first, 
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 2000,
    family = Beta(),
    save_pars = save_pars(all = TRUE),
    #family = cumulative("logit"), works with prior(normal(0, 10), class = "Intercept")
    #sample_prior = "only",
    prior = c(#prior(normal(0, 1), class = "b"),
      #prior(normal(0, 10), class = "Intercept"),
      #prior(exponential(1), class = "sd")),
      # prior(normal(0, 1.5), class = "b"),          # fixed effects
      # prior(logistic(0, 1), class = "Intercept"),  # mean rank_prop on logit scale
      # prior(exponential(1), class = "sd"),         # random effect SDs
      # prior(gamma(2, 0.1), class = "phi")),          # precision parameter
    prior(normal(0, 1),         class = "b"),
    prior(normal(0, 1),         class = "Intercept"),
    prior(exponential(1),       class = "sd"),
    prior(gamma(10, 0.5),       class = "phi")),
    control = list(adapt_delta = 0.99)
)

#Checks
plot(full_both_spread_model)
summary(full_both_spread_model) #rhat < 1.01, bulk ess and tail ess > 400
pp_check(full_both_spread_model, type="hist",ndraws=200)

# Model without dist_from_first
no_social_spread_model <- brm(formula = rank_prop ~ 
                                distance_to_ball_sc + 
                                (1|colony/colony_eel_ID) + 
                                (1|drop_ID) + 
                                (1|date), 
                              data = data_no_first, 
                              chains = 4,
                              cores = 4,
                              iter = 4000,
                              warmup = 2000,
                              family = Beta(),
                              save_pars = save_pars(all = TRUE),
                              #family = cumulative("logit"), works with prior(normal(0, 10), class = "Intercept")
                              #sample_prior = "only",
                              prior = c(#prior(normal(0, 1), class = "b"),
                                #prior(normal(0, 10), class = "Intercept"),
                                #prior(exponential(1), class = "sd")),
                                # prior(normal(0, 1.5), class = "b"),          # fixed effects
                                # prior(logistic(0, 1), class = "Intercept"),  # mean rank_prop on logit scale
                                # prior(exponential(1), class = "sd"),         # random effect SDs
                                # prior(gamma(2, 0.1), class = "phi")),          # precision parameter
                              prior(normal(0, 1),         class = "b"),
                              prior(normal(0, 1),         class = "Intercept"),
                              prior(exponential(1),       class = "sd"),
                              prior(gamma(10, 0.5),       class = "phi")),
                              control = list(adapt_delta = 0.99)
)

#Checks
plot(no_social_spread_model)
summary(no_social_spread_model) #rhat < 1.01, bulk ess and tail ess > 400
pp_check(no_social_spread_model, type="dens_overlay",ndraws=200)

# Model without distance_to_ball  
no_private_spread_model <- brm(formula = rank_prop ~ 
      dist_from_first_sc + 
      (1|colony/colony_eel_ID) + 
      (1|drop_ID) + 
      (1|date), 
    data = data_no_first, 
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 2000,
    family = Beta(),
    save_pars = save_pars(all = TRUE),
    #family = cumulative("logit"), works with prior(normal(0, 10), class = "Intercept")
    #sample_prior = "only",
    prior = c(#prior(normal(0, 1), class = "b"),
      #prior(normal(0, 10), class = "Intercept"),
      #prior(exponential(1), class = "sd")),
      # prior(normal(0, 1.5), class = "b"),          # fixed effects
      # prior(logistic(0, 1), class = "Intercept"),  # mean rank_prop on logit scale
      # prior(exponential(1), class = "sd"),         # random effect SDs
      # prior(gamma(2, 0.1), class = "phi")),          # precision parameter
      prior(normal(0, 1),         class = "b"),
      prior(normal(0, 1),         class = "Intercept"),
      prior(exponential(1),       class = "sd"),
      prior(gamma(10, 0.5),       class = "phi")),
    control = list(adapt_delta = 0.99)
)

#Checks
plot(no_private_spread_model)
summary(no_private_spread_model) #rhat < 1.01, bulk ess and tail ess > 400
pp_check(no_private_spread_model, type="dens_overlay",ndraws=200)


summary()

# Then compare
modcompare <- loo_compare(
  loo(full_interaction_spread_model),
  loo(full_both_spread_model),
  loo(no_social_spread_model),
  loo(no_private_spread_model)
)

loofull <- loo(full_interaction_spread_model)
loosocial <- loo(no_private_spread_model)
  
modcompare <- cbind(modcompare, -2*modcompare[,1], 2*modcompare[,2]) #calc LOOIC difference
colnames(modcompare)[9:10] <- c("looic_diff","se_looic_diff")
print(modcompare[,c("looic","looic_diff","se_looic_diff")], simplify=FALSE, digits=4)

x <- suppressWarnings(brms::loo_compare(
  brms::add_criterion(full_interaction_spread_model, "loo"),
  brms::add_criterion(full_both_spread_model, "loo"),
  brms::add_criterion(no_social_spread_model, "loo"),
  brms::add_criterion(no_private_spread_model, "loo"),
  model_names = c("full_interaction_spread_model", "full_both_spread_model", "no_social_spread_model","no_private_spread_model")
))

report(x)
#rule of thumb is that a ΔLOOIC > 4 with SE < ΔLOOIC/2 signals a meaningful difference. 
#A LOOIC difference of about 2 indicates a fairly negligible difference between models. 
#LOOIC can favour a simpler model even if a predictor's posterior doesn't cross zero — because the predictor might add noise elsewhere

bayes_R2(full_both_spread_model)
bayes_R2(no_private_spread_model)

posterior <- as_draws_array(full_both_spread_model)
mcmc_hist(full_both_spread_model,pars = c("b_dist_from_first_sc","b_distance_to_ball_sc"))
mcmc_dense(full_both_spread_model,pars = c("b_dist_from_first_sc:distance_to_ball_sc","b_dist_from_first_sc","b_distance_to_ball_sc"))
mcmc_combo(full_both_spread_model,pars = c("b_dist_from_first_sc","b_distance_to_ball_sc"))
mcmc_areas(full_both_spread_model, pars = c("b_dist_from_first_sc","b_distance_to_ball_sc"),prob=0.89)
ppd <- posterior_predict(model, newdata = data)




newd <- data.frame(dist_from_first_sc = seq(min(data_no_first$dist_from_first_sc), max(data_no_first$dist_from_first_sc), length.out = 100), distance_to_ball_sc = 0)
pmu <- posterior_epred(full_both_spread_model, newdata = newd, re_formula=NA)
ppd <- posterior_predict(full_both_spread_model, newdata = newd, re_formula=NA)
mnmu <- colMeans(pmu)
n <- ncol(pmu)
mean_intervals <- data.frame(mulo95=rep(NA,n), muhi95=rep(NA,n))
for ( i in 1:n ) {
  mean_intervals[i,] <- hpdi(pmu[,i], prob=0.95)
}
prediction_intervals <- predictive_interval(ppd, prob=0.95)
prediction_intervals <- data.frame(prediction_intervals)
names(prediction_intervals) <- c("ppdlow95", "ppdhi95")

preds <- cbind(newd, mnmu, mean_intervals, prediction_intervals)

orig_mean <- attr(data_no_first$dist_from_first_sc, "scaled:center")
orig_sd   <- attr(data_no_first$dist_from_first_sc, "scaled:scale")

preds$dist_from_first <- preds$dist_from_first_sc * orig_sd + orig_mean

orig_mean <- attr(data_no_first$distance_to_ball_sc, "scaled:center")
orig_sd   <- attr(data_no_first$distance_to_ball_sc, "scaled:scale")

preds$distance_to_ball <- preds$distance_to_ball_sc * orig_sd + orig_mean

#the visualisation is where you "undo" the standardisation for the reader by putting the x-axis back in original units
preds |> 
  ggplot() +
  geom_point(data = data_no_first, 
             aes(x = dist_from_first, y = rank_prop), 
             alpha = 0.3, size = 1) +  # fade points back
  geom_ribbon(aes(x = dist_from_first, ymin = mulo95, ymax = muhi95),  #95% posterior credible intervals (specifically a HPDI), uncertainty around mean 
              alpha = 0.4, fill = "steelblue") +
  geom_line(aes(x = dist_from_first, y = mnmu), 
            linewidth = 1, colour = "steelblue") +
  geom_line(aes(x = dist_from_first, y = ppdlow95), lty = 2, colour = "grey40") + #prediction intervals, uncertainty aboyt a new observed Y at that X
  geom_line(aes(x = dist_from_first, y = ppdhi95), lty = 2, colour = "grey40") + #prediction intervals
  labs(x = "Distance from first eel", 
       y = "Percentile rank response") +
  theme_bw()

#reporting coefs unscaled. Spanning 1 is null. 
full_both_spread_model |>
  gather_draws(b_dist_from_first_sc, b_distance_to_ball_sc) |>
  mutate(.value = exp(.value)) |>
  median_hdi(.width = 0.95)




### Spread of responses within a group




#1 - log_inst_top
sr_bayes_inst_topo <- brm(
  formula = second_responder ~ 
    log_distance_to_ball +
    log_inst_topo_dist_from_first +
    (1 | colony/colony_eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli(link = "logit"),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 2), class = "Intercept"),
    
    prior(exponential(1), class = "sd")
  ),
  
  control = list(adapt_delta = 0.95)
)


sr_bayes_metric_dist <- brm(
  formula = second_responder ~ 
    log_distance_to_ball +
    log_dist_from_first +
    (1 | colony/colony_eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli(link = "logit"),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 2), class = "Intercept"),
    
    prior(exponential(1), class = "sd")
  ),
  
  control = list(adapt_delta = 0.95)
)

sr_bayes_metric_dist <- brm(
  formula = second_responder ~ 
    log_distance_to_ball +
    log_dist_from_first +
    (1 | colony/colony_eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli(link = "logit"),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 2), class = "Intercept"),
    
    prior(exponential(1), class = "sd")
  ),
  
  control = list(adapt_delta = 0.95)
)







summary(sr_bayes)
posterior_summary(sr_bayes)
VarCorr(sr_bayes)
bayes_R2(sr_bayes)
loo(sr_bayes)

# Visual check — bars for binary outcome
pp_check(sr_bayes, type = "bars", ndraws = 100)
# Does model recover observed response rate?
pp_check(sr_bayes, type = "stat", stat = "mean")
loo_sr <- loo(sr_bayes)
print(loo_sr)
# Pareto k plot — flag influential observations
plot(loo_sr, diagnostic = "k")
bayes_R2(sr_bayes)
performance(sr_bayes)
# Posterior mean predicted probabilities
pred_probs <- posterior_epred(sr_bayes) |> colMeans()
# Tjur R² — mean prob separation between 1s and 0s
tjur_r2 <- mean(pred_probs[initator_responder$second_responder == 1]) -
  mean(pred_probs[initator_responder$second_responder == 0])
tjur_r2
# AUC — discrimination across full ranking
roc(initator_responder$second_responder, pred_probs)

