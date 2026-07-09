get_coefs <- function(data) {

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

data_no_first$rank_prop <- data_no_first$rank_order/(data_no_first$n_responders) #added 1 to deal with when all respond, is this okay?

data_no_first <- data_no_first |>
  tidyr::drop_na(rank_prop, dist_from_first, distance_to_ball)

data_no_first$dist_from_first_sc <- scale(data_no_first$dist_from_first)
data_no_first$distance_to_ball_sc <- scale(data_no_first$distance_to_ball)

data_no_first <- data_no_first %>% filter(rank_prop != 1) #%>% filter(rank_prop != 1)

### Spread of responses within a group

# Model with interaction
full_interaction_spread_model <- brm(formula = rank_prop ~ 
                                       dist_from_first_sc + 
                                       distance_to_ball_sc + 
                           dist_from_first_sc:distance_to_ball_sc + 
                           (1|colony/eel_ID) + 
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
      (1|colony/eel_ID) + 
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
pp_check(full_both_spread_model, type="dens_overlay")
pp_check(full_both_spread_model, type="dens_overlay",ndraws=500)
pp_check(full_both_spread_model, type = "stat", stat = "sd")
pp_check(full_both_spread_model, type = "scatter_avg")
pp_check(full_both_spread_model, ndraws = 100)

# Model without dist_from_first
no_social_spread_model <- brm(formula = rank_prop ~ 
                                distance_to_ball_sc + 
                                (1|colony/eel_ID) + 
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
      (1|colony/eel_ID) + 
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

# Then compare
modcompare <- loo_compare(
  loo(full_interaction_spread_model, k_threshold = 0.7),
  loo(full_both_spread_model, k_threshold = 0.7),
  loo(no_social_spread_model, k_threshold = 0.7),
  loo(no_private_spread_model, k_threshold = 0.7)
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

## NON SOCIAL MODELS ##

#3 - dist to first
sr_bayes_B <- brm(
  formula = second_responder ~ 
    ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  save_pars = save_pars(all = TRUE),
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#4 - log distance to first
sr_bayes_lB <- brm(
  formula = second_responder ~ 
    log_ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

## SOCIAL MODELS ##

#3 - dist to first
sr_bayes_M <- brm(
  formula = second_responder ~ 
    metric_dist_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)


#4 - log distance to first
sr_bayes_lM <- brm(
  formula = second_responder ~ 
    log_metric_dist_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#5 - topo dist global
sr_bayes_gT <- brm(
  formula = second_responder ~ 
    global_topo_dist_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#6 - log topo dist global
sr_bayes_lgT <- brm(
  formula = second_responder ~ 
    log_global_topo_dist_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#7 -topo dist inst
sr_bayes_iT <- brm(
  formula = second_responder ~ 
    inst_topo_dist_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)


#8 - log topo dist inst
sr_bayes_liT <- brm(
  formula = second_responder ~ 
    log_inst_topo_dist_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#9 - inst vor
sr_bayes_iV <- brm(
  formula = second_responder ~ 
    first_in_inst_voronoi +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#10 - global vor
sr_bayes_gV <- brm(
  formula = second_responder ~ 
    first_in_global_voronoi +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)


#SOCIAL AND PRIVATE
#11
sr_bayes_MB <- brm(
  formula = second_responder ~ 
    metric_dist_sc + ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#12
sr_bayes_MlB <- brm(
  formula = second_responder ~ 
    metric_dist_sc + log_ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#27
sr_bayes_MxB <- brm(
  formula = second_responder ~ 
    metric_dist_sc*ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#28
sr_bayes_MxlB <- brm(
  formula = second_responder ~ 
    metric_dist_sc*log_ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#13
sr_bayes_lMB <- brm(
  formula = second_responder ~ 
    log_metric_dist_sc + ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#14
sr_bayes_lMlB <- brm(
  formula = second_responder ~ 
    log_metric_dist_sc + log_ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#29
sr_bayes_lMxB <- brm(
  formula = second_responder ~ 
    log_metric_dist_sc*ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#30
sr_bayes_lMxlB <- brm(
  formula = second_responder ~ 
    log_metric_dist_sc*log_ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#15 
sr_bayes_gTB <- brm(
  formula = second_responder ~ 
    global_topo_dist_sc + ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#16 
sr_bayes_gTlB <- brm(
  formula = second_responder ~ 
    global_topo_dist_sc + log_ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#31 
sr_bayes_gTxB <- brm(
  formula = second_responder ~ 
    global_topo_dist_sc*ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#32
sr_bayes_gTxlB <- brm(
  formula = second_responder ~ 
    global_topo_dist_sc*log_ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#17
sr_bayes_lgTB <- brm(
  formula = second_responder ~ 
    log_global_topo_dist_sc + ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#18
sr_bayes_lgTlB <- brm(
  formula = second_responder ~ 
    log_global_topo_dist_sc + log_ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#33
sr_bayes_lgTxB <- brm(
  formula = second_responder ~ 
    log_global_topo_dist_sc*ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#34
sr_bayes_lgTxlB <- brm(
  formula = second_responder ~ 
    log_global_topo_dist_sc + log_ball_sc + 
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)


#19
sr_bayes_iTB <- brm(
  formula = second_responder ~ 
    inst_topo_dist_sc + ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#35
sr_bayes_iTxB <- brm(
  formula = second_responder ~ 
    inst_topo_dist_sc*ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#36
sr_bayes_iTxlB <- brm(
  formula = second_responder ~ 
    inst_topo_dist_sc*log_ball_sc +
  (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)


#21
sr_bayes_liTB <- brm(
  formula = second_responder ~ 
    log_inst_topo_dist_sc + ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#22
sr_bayes_liTlB <- brm(
  formula = second_responder ~ 
    log_inst_topo_dist_sc + log_ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#37
sr_bayes_liTxB <- brm(
  formula = second_responder ~ 
    log_inst_topo_dist_sc*ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = brms::bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#38
sr_bayes_liTxlB <- brm(
  formula = second_responder ~ 
    log_inst_topo_dist_sc*log_ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)


#23
sr_bayes_iVB <- brm(
  formula = second_responder ~ 
    first_in_inst_voronoi + ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  save_pars = save_pars(all = TRUE),
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#24
sr_bayes_iVlB <- brm(
  formula = second_responder ~ 
    first_in_inst_voronoi + log_ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#39
sr_bayes_iVxB <- brm(
  formula = second_responder ~ 
    first_in_inst_voronoi*ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#40
sr_bayes_iVxlB <- brm(
  formula = second_responder ~ 
    first_in_inst_voronoi*log_ball_sc +
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#25
sr_bayes_gVB <- brm(
  formula = second_responder ~ 
    first_in_global_voronoi + ball_sc + 
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#26
sr_bayes_gVlB <- brm(
  formula = second_responder ~ 
    first_in_global_voronoi + log_ball_sc + 
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#41
sr_bayes_gVxB <- brm(
  formula = second_responder ~ 
    first_in_global_voronoi*ball_sc + 
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

#42
sr_bayes_gVxlB <- brm(
  formula = second_responder ~ 
    first_in_global_voronoi*log_ball_sc + 
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = initator_responder,
  family = bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)


#model comparison
modcompare <- loo_compare(
  loo(sr_bayes_gT, k_threshold = 0.7),
  loo(sr_bayes_gV, k_threshold = 0.7),
  loo(sr_bayes_iV, k_threshold = 0.7),
  loo(sr_bayes_iT, k_threshold = 0.7),
  loo(sr_bayes_lgT, k_threshold = 0.7),
  loo(sr_bayes_liT, k_threshold = 0.7, moment_match=TRUE),
  loo(sr_bayes_lM, k_threshold = 0.7),
  loo(sr_bayes_M, k_threshold = 0.7),
  loo(sr_bayes_B, k_threshold = 0.7),
  loo(sr_bayes_lB, k_threshold = 0.7),
  loo(sr_bayes_gTB, k_threshold = 0.7),
  loo(sr_bayes_gTlB, k_threshold = 0.7),
  loo(sr_bayes_gTxB, k_threshold = 0.7),
  loo(sr_bayes_gTxlB, k_threshold = 0.7),
  loo(sr_bayes_gVB, k_threshold = 0.7),
  loo(sr_bayes_gVlB, k_threshold = 0.7),
  loo(sr_bayes_gVxB, k_threshold = 0.7),
  loo(sr_bayes_gVxlB, k_threshold = 0.7),
  loo(sr_bayes_iTB, k_threshold = 0.7),
  loo(sr_bayes_iTxB, k_threshold = 0.7),
  loo(sr_bayes_iTxlB, k_threshold = 0.7),
  loo(sr_bayes_iVB, k_threshold = 0.7),
  loo(sr_bayes_iVlB, k_threshold = 0.7),
  loo(sr_bayes_iVxB, k_threshold = 0.7),
  loo(sr_bayes_iVxlB, k_threshold = 0.7),
  loo(sr_bayes_lgTB, k_threshold = 0.7, moment_match=TRUE),
  loo(sr_bayes_lgTlB, k_threshold = 0.7),
  loo(sr_bayes_lgTxB, k_threshold = 0.7),
  loo(sr_bayes_lgTxlB, k_threshold = 0.7),
  loo(sr_bayes_liTB, k_threshold = 0.7),
  loo(sr_bayes_liTlB, k_threshold = 0.7),
  loo(sr_bayes_liTxB, k_threshold = 0.7, moment_match=TRUE),
  loo(sr_bayes_liTxlB, k_threshold = 0.7),
  loo(sr_bayes_lMB, k_threshold = 0.7),
  loo(sr_bayes_lMlB, k_threshold = 0.7),
  loo(sr_bayes_lMxB, k_threshold = 0.7),
  loo(sr_bayes_lMxlB, k_threshold = 0.7),
  loo(sr_bayes_MB, k_threshold = 0.7),
  loo(sr_bayes_MlB, k_threshold = 0.7),
  loo(sr_bayes_MxB, k_threshold = 0.7),
  loo(sr_bayes_MxlB, k_threshold = 0.7),
  loo(sr_bayes_B, k_threshold = 0.7)
)

modcompare <- cbind(modcompare, -2*modcompare[,1], 2*modcompare[,2]) #calc LOOIC difference
colnames(modcompare)[9:10] <- c("looic_diff","se_looic_diff")
print(modcompare[,c("looic","looic_diff","se_looic_diff")], simplify=FALSE, digits=4)
write.csv(modcompare, path = "modcompare.csv")
summary(sr_bayes_liTlB)

pp_check(sr_bayes_liTlB, type="dens_overlay",ndraws=100)
#Data density curve fits within simulations

pp_check(sr_bayes_liTlB, type="hist",ndraws=100, vars = c("b_log_inst_topo_dist_sc","b_log_ball_sc"))


pp_check(sr_bayes_liTlB, type="stat",stat = "mean",ndraws=100, bins = 10)
#data fits within distribution mean test statistic

pp_check(sr_bayes_liTlB, type="stat",stat = "sd",ndraws=100, bins = 10)
#data fits within distribution sd test statistic

pp_check(sr_bayes_liTlB, type="stat",stat = "max",ndraws=100, bins = 10) #doesn't really work for binomial, alternatives?
pp_check(sr_bayes_liTlB, type="stat",stat = "min",ndraws=100, bins = 10) #doesn't really work for binomial, alternatives?

pp_check(sr_bayes_liTlB, type = "scatter_avg") #doesn't really work for binomial, alternatives?

pp_check(sr_bayes_liTlB, type = "error_hist", nreps = 6)

#Looking at samples to judge shape and noisiness of posterior
cols <- sample(seq_len(ncol(pmu)), 6)

pmu_sub <- pmu[, cols, drop = FALSE]
colnames(pmu_sub) <- paste0("pred_", cols)

mcmc_hist(pmu_sub, bins = 75)


mcmc_combo(sr_bayes_liTlB,pars = c("b_log_inst_topo_dist_sc","b_log_ball_sc"))
mcmc_areas(sr_bayes_liTlB, pars = c("b_log_inst_topo_dist_sc","b_log_ball_sc"),prob=0.89)

#BROWSE DIAGNOSTICS
launch_shinystan(sr_bayes_liTlB)

plot(sr_bayes_liTlB, "neff",variable = c("b_log_inst_topo_dist_sc","b_log_ball_sc"))

newd <- data.frame(log_inst_topo_dist_sc = seq(min(initator_responder$log_inst_topo_dist_sc), max(initator_responder$log_inst_topo_dist_sc), length.out = 100), log_ball_sc = 0)
pmu <- posterior_epred(sr_bayes_liTlB, newdata = newd, re_formula=NA)
ppd <- posterior_predict(sr_bayes_liTlB, newdata = newd, re_formula=NA)

mnmu <- colMeans(pmu)
n <- ncol(pmu)
n <- nrow(newd)
mean_intervals <- data.frame(mulo95=rep(NA,n), muhi95=rep(NA,n))
for ( i in 1:n ) {
  mean_intervals[i,] <- hpdi(pmu[,i], prob=0.95)
}
prediction_intervals <- predictive_interval(ppd, prob=0.95)
prediction_intervals <- data.frame(prediction_intervals)
names(prediction_intervals) <- c("ppdlow95", "ppdhi95")

preds <- cbind(newd, mnmu, mean_intervals, prediction_intervals)

orig_topo_mean <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:center")
orig_topo_sd   <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:scale")

preds$log_inst_topo_dist <- preds$log_inst_topo_dist_sc * orig_sd + orig_mean

orig_ball_mean <- attr(data_no_first$log_distance_to_ball_sc, "scaled:center")
orig_ball_sd   <- attr(data_no_first$log_distance_to_ball_sc, "scaled:scale")

preds$log_distance_to_ball <- preds$log_distance_to_ball_sc * orig_sd + orig_mean

# Back-transform: undo standardisation AND the log transform
preds$inst_topo_dist <- exp(preds$log_inst_topo_dist)  # exp() undoes log()

# Similarly for your raw data points
initator_responder$inst_topo_dist_from_first <- exp(initator_responder$log_inst_topo_dist_from_first)

#the visualisation is where you "undo" the standardisation for the reader by putting the x-axis back in original units
preds |> 
  ggplot() +
  geom_point(data = initator_responder, 
             aes(x = log_inst_topo_dist_from_first, y = second_responder), 
             alpha = 0.2, size = 1, height = 0.01, width = 0.2) +  # fade points back
  geom_ribbon(aes(x = log_inst_topo_dist, ymin = mulo95, ymax = muhi95),  #95% posterior credible intervals (specifically a HPDI), uncertainty around mean 
              alpha = 0.4, fill = "steelblue") +
  geom_line(aes(x = log_inst_topo_dist, y = mnmu), 
            linewidth = 1, colour = "steelblue") +
  #geom_line(aes(x = inst_topo_dist, y = ppdlow95), lty = 2, colour = "grey40") + #prediction intervals, uncertainty aboyt a new observed Y at that X
  #geom_line(aes(x = inst_topo_dist, y = ppdhi95), lty = 2, colour = "grey40") + #prediction intervals
  labs(x = "Topological distance from first eel", 
       y = "Probability of being a second responder") +
  theme_bw() 

#uncertainty is small relative to prediction!

#reporting coefs unscaled. Spanning 1 is null. 
sr_bayes_liTlB |>
  gather_draws(b_dist_from_first_sc, b_distance_to_ball_sc) |>
  mutate(.value = exp(.value)) |>
  median_hdi(.width = 0.95)

summary(sr_bayes_liTlB)

models <- ls(pattern = "sr_bayes")

for (m in models) {
  saveRDS(get(m), file = paste0(m, ".rds"))
}

data$log_distance_to_ball_sc <- scale(data$log_distance_to_ball)
#First responder
fr_bayes_lB <- brm(
  formula = first_responder ~ 
    log_distance_to_ball_sc + 
    (1 | colony/eel_ID) +
    (1 | drop_ID) +
    (1 | date),
  
  data = data,
  family = brms::bernoulli,
  save_pars = save_pars(all = TRUE),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  
  #sample_prior = TRUE,
  
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

summary(fr_bayes_lB)
pp_check(fr_bayes_lB, type="dens_overlay",ndraws=100)

summary(sr_bayes_liTlB)

#options
# 1. Run with the posterior mean to get a reference trajectory
# 2. Draw ~500–1000 samples from your posterior, run the SIR for each, and plot the credible interval band around your trajectories

#Params (all on scaled!)
coefs <- list()
coefs[1] <- -2.92 #fr intercept
coefs[2] <- -0.75 #fr log distance from ball
coefs[3] <- -3.71 #sr intercept
coefs[4] <- -0.58 #sr log inst topo dist
coefs[5] <- -0.54 #sr log distance from ball


#-0.58
#-0.54




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

return(coefs)

}
