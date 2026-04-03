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

#Data manipulation
setwd("~/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/Shared drives/Field Research Videos/Gil Lab/Raw_Data/Curacao_2024/garden_eels/position_drop_experiment")
data <- read_excel("master_ball_drop_data_3D_0216.xlsx")

data$colony_drop_ID <- paste(data$drop_ID,":",data$colony,sep="")
data$colony_eel_ID <- paste(data$eel_ID,data$colony,sep = "_")

data$distance_to_ball <- sqrt((data$base_X - data$ball_hit_X)^2 + (data$base_Y - data$ball_hit_Y)^2 + (data$base_Z - data$ball_hit_Z)^2)

data$colony_size[data$colony == "S5"] <- 34
data$colony_size[data$colony == "S9"] <- 59
data$colony_size[data$colony == "S15"] <- 67
data$colony_size[data$colony == "S12"] <- 47
data$colony_size[data$colony == "S7"] <- 116

data <- data %>%
  group_by(drop_ID) %>%
  mutate(inst_emerged = sum(!is.na(full_partial_none))) %>%
  ungroup()

data<- data %>%
  group_by(drop_ID) %>%
  mutate(first_responder = as.integer(!is.na(response_frame_cam1) & response_frame_cam1 == min(response_frame_cam1, na.rm = TRUE))) %>%ungroup()

  
#Fit a model for initator responder pairs
#Summarise data by initiator-responder pairs
initator_responder <- data %>%
  group_by(drop_ID) %>%
  mutate(responses = sum(full_partial_none > 0, na.rm=TRUE)) %>%
  filter(responses > 0 & !is.na(full_partial_none)) %>%
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
  filter(!is.na(dist_from_first_resp) & dist_from_first_resp > 0) %>%
  ungroup()

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

initator_responder$drop_ID <- as.factor(initator_responder$drop_ID)

initator_responder$date <- as.factor(initator_responder$date)

initator_responder$re_colony_eel_ID <- interaction(
  initator_responder$colony,
  initator_responder$eel_ID,
  drop=TRUE
)

initator_responder <- as.data.frame(initator_responder)

# Create all scaled versions first, OUTSIDE the loop
initator_responder$dist_sc        <- scale(initator_responder$dist_from_first_resp)
initator_responder$ball_sc        <- scale(initator_responder$distance_to_ball)
initator_responder$log_dist_sc    <- scale(log(initator_responder$dist_from_first_resp))
initator_responder$log_ball_sc    <- scale(log(initator_responder$distance_to_ball))

# testing random effects structure
intercepts_random_model <- glmer(second_responder ~ 1 + (1 | colony/eel_ID) + (1|drop_ID) + (1|date), family = binomial, data = initator_responder)
summary(intercepts_random_model)

#test model
test_model <- glmmLasso(
  fix = second_responder ~ dist_sc + ball_sc + log_dist_sc + log_ball_sc,
  rnd = list(re_colony_eel_ID = ~1, colony = ~1, drop_ID = ~1, date = ~1),
  family = binomial(),
  data = initator_responder,
  lambda = 2
  # no control/start - uses defaults
)

summary(test_model)


####
lambda      <- 10^seq(-4, 2, length = 10)
devianz_vec <- rep(Inf, length(lambda))
coeff_ma    <- NULL

binom_deviance <- function(y, y_hat) {
  y_hat <- pmin(pmax(y_hat, 1e-6), 1 - 1e-6)
  -2 * sum(y * log(y_hat) + (1 - y) * log(1 - y_hat))
}

for (j in 1:length(lambda)) {
  
  glm1 <- try(glmmLasso(
    fix = second_responder ~ dist_sc + ball_sc + log_dist_sc + log_ball_sc,  # full model formula
    rnd = list(re_colony_eel_ID = ~1, colony = ~1, drop_ID = ~1, date = ~1),
    family = binomial(),
    data = initator_responder,
    lambda = lambda[j])
  )
  
  if (!inherits(glm1, "try-error") & !is.null(glm1$coefficients)) {
    y_hat          <- predict(glm1, type = "response")
    devianz_vec[j] <- binom_deviance(initator_responder$second_responder, y_hat)
    coeff_ma       <- cbind(coeff_ma, glm1$coefficients)
    cat("lambda:", round(lambda[j], 5),
        "| dev:", round(devianz_vec[j], 2),
        "| coefs:", round(glm1$coefficients[-1], 3), "\n")
  }

}

# Exclude null model (any lambda where ALL non-intercept coefs are zero)
nonzero_mask <- apply(coeff_ma[-1, , drop = FALSE], 2, function(x) any(x != 0))

final_lambda <- lambda[which.min(ifelse(nonzero_mask, devianz_vec, Inf))]
cat("lambda_min:", final_lambda, "\n")

# Replot with null region greyed out
plot(log10(lambda), devianz_vec, type = "b", pch = 19,
     xlab = "log10(lambda)", ylab = "Binomial deviance",
     main = "Lambda selection: min binomial deviance")
points(log10(lambda)[!nonzero_mask], devianz_vec[!nonzero_mask],
       col = "grey70", pch = 19)
abline(v = log10(final_lambda), lty = 2, col = "red")

#Fit final model to the lambda with the lowest deviance to find the top-ranked predictors
models_fine  <- glmmLasso(
  fix = second_responder ~ dist_sc + ball_sc + log_dist_sc + log_ball_sc,  # full model formula
  rnd = list(re_colony_eel_ID = ~1, colony = ~1, drop_ID = ~1, date = ~1),
  family = binomial(),
  data = initator_responder,
  lambda = final_lambda
)
summary(models_fine)

# Check what's actually happening across lambda
matplot(log10(lambda), t(coeff_ma), type = "l", lty = 1,
        xlab = "log10(lambda)", ylab = "Coefficients",
        main = "glmmLasso coefficient paths")
abline(v = log10(final_lambda), lty = 2, col = "red")
legend("topright", rownames(coeff_ma), col = 1:nrow(coeff_ma), lty = 1, cex = 0.7)


#Collinearity... drop one of the logs or non logs and see what happens

#model coefficients are weights

#################################################################################
#Simulate the ball landing at a random position
#Draw first responder using the parameter from the likelihood of being a first responder with distance from ball (logistic regression 1 or 0 first responder with distance from ball)
#For each ms after first responder, compute the dose experience by each individual. 
#Set a dose threshold (can simulate across) and if inidividual dose exceeds this, it is activated - flees
#if flees, this acts to affect the dose of the individuals around it.
#compute cascade size across many simulations of group size and dose threshold
#see how these cascade size distributions compare to real data
#For each trial, get the ball position and the eel positions, compute an interaction network
#Weights - probability individual i startles given that individual j has startle. The logistic regression gives you w_ij, and you're justified in using it as p_ij in the contagion model because of the proportionality argument. 

#1 - First responder model
fr_model <- glmer(first_responder ~ distance_to_ball + (1|colony/colony_eel_ID) + (1|drop_ID) + (1|date), family = binomial, data = data)
summary(fr_model)
# Fixed effects
intercept <- fixef(fr_model)[1]  # gives β₀ and β_distance
b_dist <- fixef(fr_model)[2] 

# Random effects
re_colony_colony_eel_ID <- ranef(fr_model)$'colony_eel_ID:colony'
re_colony_colony_eel_ID$combo <- rownames(re_colony_colony_eel_ID)

re_drop_ID <- ranef(fr_model)$drop_ID    # u_drop for each drop nested in colony
re_drop_ID$combo <- rownames(re_drop_ID)

re_date <- ranef(fr_model)$date
re_date$combo <- rownames(re_date)

re_colony <- ranef(fr_model)$colony
re_colony$combo <- rownames(re_colony)

#Optional - placing in df first then just pulling from? Not correct random effects below.... 
re_drop <- ranef(fr_model)$`drop_ID:colony` %>%
  rownames_to_column("colony_drop_ID")

re_eel <- ranef(fr_model)$colony_eel_ID %>%
  rownames_to_column("colony_eel_ID")

data2 <- data %>%
  mutate(colony_drop_ID = paste0(drop_ID, ":", colony)) %>%
  left_join(re_drop, by = "colony_drop_ID") %>%
  left_join(re_eel, by = "colony_eel_ID", suffix = c("_drop", "_eel"))

data2$eta <- intercept +
  b_dist * data2$distance_to_ball +
  data2$`(Intercept)_drop` +
  data2$`(Intercept)_eel`


n_drops <- length(unique(data$drop_ID))

#weight strengths... come back to this
for (c in 1:length(unique(data$colony))) {
  data %>%
    filter(unique(data$colony)[1] == colony) %>%
    group_by(colony_eel_ID) %>%
    mutate()
} 

#2 - simulating the cascade at each drop 
for (i in 1:n_drops) {
  print(i)
  #Calculate which individuals are emerged 
  drop_data <-data %>%
    filter(drop_ID == i & !is.na(full_partial_none))
  
  drop_eel_IDs <- unique(drop_data$colony_eel_ID)
  
  
  
  #determine first responder
  for (j in 1:drop_eel_IDs) {
    #for each eel i in drop j nested in colony k, compute the linear predictor
   eta_j <- intercept + b_dist*(drop_data$distance_to_ball[drop_data$colony_eel_ID == j]) + re_drop_ID$"(Intercept)"[re_drop_ID$combo == "colony_drop_ID"]() + re_colony_eel_ID$"(Intercept)"[re_colony_ID$combo == j]
    1/(1+exp(-eta_j))
  }
  #convert this to a standard logistic transform - gives probability per eel
  #sample the first responder (p_eel_i = p_i / sum of p_i)
  winner <- sample(eel_ids, size=1, prob=p_normalized (vector of normalized probabilities))
  
}


p_ij <- 1/(1+exp(-b1-b2*LMD))
