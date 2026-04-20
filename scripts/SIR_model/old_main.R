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

#Some drops the first response frame is before the ball enters view, filter these. - 176
#Fish in frame - 146
#Some have far apart second and first because eel across the group started to hide but it seems like the others don't see - 168, 40
#Tangled line - 157

#Data manipulation
setwd("~/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/Shared drives/Field Research Videos/Gil Lab/Raw_Data/Curacao_2024/garden_eels/position_drop_experiment")
data <- read_excel("master_ball_drop_data_3D_0216.xlsx") %>%
  filter(drop_ID != 152) %>%
  filter (drop_ID != 169) %>%
  filter (drop_ID != 146) %>%
  filter(drop_ID != 176) %>%
  filter(drop_ID != 157)


data$colony_drop_ID <- paste(data$drop_ID,":",data$colony,sep="")
data$colony_eel_ID <- paste(data$eel_ID,data$colony,sep = "_")

data$distance_to_ball <- sqrt((data$base_X - data$ball_hit_X)^2 + (data$base_Y - data$ball_hit_Y)^2 + (data$base_Z - data$ball_hit_Z)^2)

data <- data %>%
  mutate(full_partial_none = case_when(
    full_partial_none == 2 ~ 2,
    full_partial_none == 1 ~ 1,
    full_partial_none == 0 ~ 0,
    TRUE ~ NA_real_
  ))

data$colony_size[data$colony == "S5"] <- 34
data$colony_size[data$colony == "S9"] <- 59
data$colony_size[data$colony == "S15"] <- 67
data$colony_size[data$colony == "S12"] <- 47
data$colony_size[data$colony == "S7"] <- 116

data <- data %>%
  group_by(drop_ID) %>%
  mutate(inst_emerged = sum(!is.na(full_partial_none))) %>%
  ungroup()

data <- data %>%
  group_by(drop_ID) %>%
  mutate(
    rank_order = {
      r <- rep(NA_real_, n())
      responders <- !is.na(full_partial_none) & full_partial_none == 1 & !is.na(response_frame_cam1)
      r[responders] <- rank(response_frame_cam1[responders], ties.method = "first")
      r
    },
    initator = as.integer(rank_order == 1),
    first_index = match(1, rank_order),
    second_responder = as.integer(rank_order == 2),
    subsequent_responder = ifelse(is.na(full_partial_none), NA, as.integer(!is.na(rank_order) & rank_order !=1)),
    first_x = base_X[first_index],
    first_y = base_Y[first_index],
    first_z = base_Z[first_index],
    # Compute distance only for responders
    dist_from_first_resp = ifelse(
      full_partial_none == 1,
      sqrt((base_X - first_x)^2 + (base_Y - first_y)^2 + (base_Z - first_z)^2),
      NA_real_
    )) %>%
  ungroup()


#Fit a model for initator responder pairs
#Summarise data by initiator-responder pairs
# Fit a model for initiator-responder pairs
# Summarise data by initiator-responder pairs
initator_responder <- data %>%
  group_by(drop_ID) %>%
  mutate(
    responses = sum(full_partial_none > 0, na.rm = TRUE)
  ) %>%
  filter(responses > 0 & !is.na(full_partial_none)) %>%
  mutate(
    first_index = if (any(full_partial_none != 0 & !is.na(full_partial_none))) {
      which.min(
        ifelse(
          full_partial_none != 0 & !is.na(full_partial_none),
          response_frame_cam1,
          Inf
        )
      )
    } else {
      NA_integer_
    }
  ) %>%
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
      sqrt(
        (base_X - first_x)^2 +
          (base_Y - first_y)^2 +
          (base_Z - first_z)^2
      ),
      NA_real_
    ),
    time_lag_since_first =
      response_frame_cam1 - response_frame_cam1[first_index]
  ) %>%
  filter(!is.na(dist_from_first_resp) & dist_from_first_resp > 0) %>%
  ungroup()



#For histogram
initator_responder <- data %>%
  group_by(drop_ID) %>%
  filter(full_partial_none == 1) %>%   # only responders
  mutate(
    rank_order = rank(response_frame_cam1, ties.method = "first"),
    initator = as.integer(rank_order == 1),
    second_responder = as.integer(rank_order == 2),
    first_response_time = min(response_frame_cam1, na.rm = TRUE),
    time_lag_since_first = response_frame_cam1 - first_response_time
  ) %>%
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

initator_responder_only <- initator_responder %>%
  filter(second_responder == 1)

initator_responder_only %>%
  ggplot(aes(x=time_lag_since_first))+
  geom_histogram(binwidth = 3,color="black", fill="lightblue")

initator_responder_only %>%
  ggplot(aes(x=dist_from_first_resp, y = time_lag_since_first)) +
  geom_point()

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
  fix = second_responder ~ dist_sc + ball_sc,
  rnd = list(re_colony_eel_ID = ~1, colony = ~1, drop_ID = ~1, date = ~1),
  family = binomial(),
  data = initator_responder,
  lambda = 2
  # no control/start - uses defaults
)

summary(test_model)


####
lambda      <- 10^seq(-3, 3, length = 50)
devianz_vec <- rep(Inf, length(lambda))
coeff_ma    <- NULL

binom_deviance <- function(y, y_hat) {
  y_hat <- pmin(pmax(y_hat, 1e-6), 1 - 1e-6)
  -2 * sum(y * log(y_hat) + (1 - y) * log(1 - y_hat))
}

for (j in 1:length(lambda)) {
  
  glm1 <- try(glmmLasso(
    fix = second_responder ~ dist_sc + ball_sc,  # full model formula
    rnd = list(re_colony_eel_ID = ~1, colony = ~1, drop_ID = ~1, date = ~1),
    family = binomial(),
    data = initator_responder,
    lambda = lambda[j])
  )
  
  if (!inherits(glm1, "try-error") & !is.null(glm1$coefficients)) {
    y_hat          <- predict(glm1, type = "response")
    devianz_vec[j] <- binom_deviance(initator_responder$second_responder, y_hat)
    coeff_ma       <- cbind(coeff_ma, glm1$coefficients)
    #cat("lambda:", round(lambda[j], 5),
    #    "| dev:", round(devianz_vec[j], 2),
    #    "| coefs:", round(glm1$coefficients[-1], 3), "\n")
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
  fix = second_responder ~ dist_sc + ball_sc,  # full model formula
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

cv1(second_responder, initator_responder, lambda1=1,fold=10)

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
fr_intercept <- as.numeric(fixef(fr_model)[1])  # gives β₀ and β_distance
fr_b_dist <- as.numeric(fixef(fr_model)[2]) 
# Random effects
fr_re_colony_colony_eel_ID <- ranef(fr_model)$'colony_eel_ID:colony'
fr_re_colony_colony_eel_ID$combo <- rownames(fr_re_colony_colony_eel_ID)
fr_re_colony_colony_eel_ID$name <- str_extract(fr_re_colony_colony_eel_ID$combo, "^[^:]+")
fr_re_drop_ID <- ranef(fr_model)$drop_ID    # u_drop for each drop nested in colony
fr_re_drop_ID$combo <- rownames(fr_re_drop_ID)
fr_re_date <- ranef(fr_model)$date
fr_re_date$combo <- rownames(fr_re_date)
fr_re_colony <- ranef(fr_model)$colony
fr_re_colony$combo <- rownames(fr_re_colony)

#2 - Second responder model
sr_model <- glmer(second_responder ~ distance_to_ball + dist_from_first_resp + (1 | colony/colony_eel_ID) + (1|drop_ID) + (1|date), family = binomial, data = initator_responder)
summary(sr_model)
#Fixed effects
sr_intercept <- as.numeric(fixef(sr_model)[1])
sr_b_dist_first <- as.numeric(fixef(sr_model)[3])
#Random effects
sr_re_colony_colony_eel_ID <- ranef(sr_model)$'colony_eel_ID:colony'
sr_re_colony_colony_eel_ID$combo <- rownames(sr_re_colony_colony_eel_ID)
sr_re_colony_colony_eel_ID$name <- str_extract(sr_re_colony_colony_eel_ID$combo, "^[^:]+")
sr_re_drop_ID <- ranef(sr_model)$drop_ID    # u_drop for each drop nested in colony
sr_re_drop_ID$combo <- rownames(sr_re_drop_ID)
sr_re_date <- ranef(sr_model)$date
sr_re_date$combo <- rownames(sr_re_date)
sr_re_colony <- ranef(sr_model)$colony
sr_re_colony$combo <- rownames(sr_re_colony)

#3 - Subsequent responder model
subs_model <- glmer(subsequent_responder ~ distance_to_ball + (1|colony/colony_eel_ID) + (1|drop_ID) + (1|date), family = binomial, data = data)
summary(subs_model)
#Fixed effects
subs_intercept <- as.numeric(fixef(subs_model)[1])
subs_b_dist_ball <- as.numeric(fixef(subs_model)[2])
#Random effects
subs_re_colony_colony_eel_ID <- ranef(subs_model)$'colony_eel_ID:colony'
subs_re_colony_colony_eel_ID$combo <- rownames(subs_re_colony_colony_eel_ID)
subs_re_colony_colony_eel_ID$name <- str_extract(subs_re_colony_colony_eel_ID$combo, "^[^:]+")
subs_re_drop_ID <- ranef(subs_model)$drop_ID    # u_drop for each drop nested in colony
subs_re_drop_ID$combo <- rownames(subs_re_drop_ID)
subs_re_date <- ranef(subs_model)$date
subs_re_date$combo <- rownames(subs_re_date)
subs_re_colony <- ranef(subs_model)$colony
subs_re_colony$combo <- rownames(subs_re_colony)

n_drops <- length(unique(data$drop_ID))

#weight strengths... come back to this
weight_strengths <- vector(mode="list", length = length(unique(data$colony)))

for (c in 1:length(unique(data$colony))) {
  
  #filter out that colony data
  data_colony <- data %>%
    filter(colony == unique(data$colony)[c])
  
  # get the full set of eel IDs for this colony
  all_eels <- unique(data_colony$colony_eel_ID)
  
  trial_distances <- vector(mode="list", length = length(unique(data_colony$trial_ID)))
  
  #for each trial
  for (t in 1:length(unique(data_colony$trial_ID))) {
    
    #average eel position per trial
    data_coords <- data_colony %>%
      filter(trial_ID == unique(data_colony$trial_ID)[t]) %>%
      group_by(colony_eel_ID) %>%
      summarise(avg_x = mean(base_X, na.rm=TRUE), avg_y = mean(base_Y, na.rm=TRUE), avg_z = mean(base_Z, na.rm=TRUE))
    
    #name the rownames the eel_ID
    coords <- data_coords[, c("avg_x", "avg_y", "avg_z")]
    rownames(coords) <- data_coords$colony_eel_ID
    
    #transform into distance matrix
    dist_mat <- as.data.frame(as.matrix(dist(coords)))
    
    # pad with NA rows/cols for eels absent in this trial
    missing_eels <- setdiff(all_eels, rownames(dist_mat))
    
    if (length(missing_eels) > 0) {
      # add NA rows for missing eels
      na_rows <- as.data.frame(matrix(NA, nrow = length(missing_eels), ncol = ncol(dist_mat),
                                      dimnames = list(missing_eels, colnames(dist_mat))))
      dist_mat <- rbind(dist_mat, na_rows)
      
      # add NA cols for missing eels
      na_cols <- as.data.frame(matrix(NA, nrow = nrow(dist_mat), ncol = length(missing_eels),
                                      dimnames = list(rownames(dist_mat), missing_eels)))
      dist_mat <- cbind(dist_mat, na_cols)
    }
    
    # reorder rows and cols to consistent order across trials
    dist_mat <- dist_mat[all_eels, all_eels]
    
    trial_distances[[t]] <- dist_mat
  }
  
  # sum up distances across trials
  trial_distances_sum <- Reduce(
    function(x, y) {
      res <- x
      res[!is.na(y)] <- ifelse(is.na(x[!is.na(y)]), y[!is.na(y)], x[!is.na(y)] + y[!is.na(y)])
      res
    },
    trial_distances
  )
  
  trial_distances_count <- Reduce(
    function(x, y) x + (!is.na(y)),
    trial_distances,
    accumulate = FALSE,
    init = matrix(0, nrow = nrow(trial_distances[[1]]), ncol = ncol(trial_distances[[1]]),
                  dimnames = dimnames(trial_distances[[1]]))
  )
  
  # pairs that were NA in *all* trials stay NA
  trial_distances_mean <- trial_distances_sum / trial_distances_count
  trial_distances_mean[trial_distances_count == 0] <- NA
  
  trial_distances_mean <- apply(trial_distances_mean, 
                                c(1,2), 
                                function(x) {
                                  if (!is.na(x)) {
                                    1/(1+exp(-sr_intercept-(sr_b_dist_first*x))) #add in random effects!! but that would give a per drop weighting... 
                                  } else { 
                                    NA }
                                })
  
  weight_strengths[[c]] <- trial_distances_mean
}


#Threshold strengths
theta <- runif(n_eels) #not using right now, fixed

#Frame range of cascades
ranges <- data %>%
  group_by(drop_ID) %>%
  summarise(range = max(response_frame_cam1, na.rm=TRUE) - min(response_frame_cam1, na.rm=TRUE))

max(ranges$range)

max_rate <- 1
dt <- 1
da <- 1
threshold <- 0.01
tm <- 10
tr <- 5

n_sims <- 200
social_frame_recorder_list <- vector(mode="list", length = length(unique(data$drop_ID)))
names(social_frame_recorder_list) <- unique(data$drop_ID)


#2 - simulating the cascade at each drop 
for (i in unique(data$drop_ID)) {
  
  print(i)
  
  social_frame_recorder_list[[i]] <- vector(mode = "list", length = n_sims)
  
  for (sim in 1:n_sims) {
    
    #Calculate which individuals are emerged 
    drop_data <- data %>%
      filter(drop_ID == i & !is.na(full_partial_none) & !is.na(base_x_cam1) & !is.na(base_x_cam2)) # & !is.na(dist_from_first_resp)
    
    drop_eel_IDs <- unique(drop_data$colony_eel_ID)
    
    #create a frame recorder matrix
    social_frame_recorder_matrix <- matrix(nrow=length(drop_eel_IDs), dimnames=list(drop_eel_IDs, NULL))
    
    resp_data <- as.data.frame(matrix(nrow=length(drop_eel_IDs),ncol=3))
    
    colony_idx <- which(unique(data$colony) == first(drop_data$colony))
    
    
    #determine first responder
    for (h in 1:length(drop_eel_IDs)) {
      l_drop_ID <- first(drop_data$drop_ID)
      l_colony_eel_ID <- drop_data$colony_eel_ID[h]
      l_date <- first(drop_data$date)
      l_colony <- first(drop_data$colony)
      #for each eel i in drop j nested in colony k, compute the linear predictor
      eta_j <- fr_intercept + fr_b_dist*(drop_data$distance_to_ball[h]) + fr_re_drop_ID$"(Intercept)"[fr_re_drop_ID$combo == l_drop_ID] + fr_re_colony_colony_eel_ID$"(Intercept)"[as.character(fr_re_colony_colony_eel_ID$name) == l_colony_eel_ID] + fr_re_date$"(Intercept)"[fr_re_date$combo == l_date] + fr_re_colony$"(Intercept)"[fr_re_colony$combo == l_colony]
      #convert this to a standard logistic transform - gives probability per eel
      p_respond <- 1/(1+exp(-eta_j))
      resp_data[h,1] <- l_colony_eel_ID
      resp_data[h,2] <- p_respond
      resp_data[h,3] <- rbinom(n = 1, size = 1, prob = p_respond)
    }
    
    #if there is a first responder
    if (sum(resp_data[,3], na.rm = TRUE) > 0) {
      
      #find IDs of first responder
      fr_ID <- resp_data$V1[resp_data$V3 == 1]
      
      #find index of first responder
      fr_idx <- which(drop_eel_IDs %in% fr_ID)
      
      social_frame_recorder_matrix[fr_idx] <- 1
      
      #create state matrix 
      state_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = 200)
      state_matrix[fr_idx,1] <- "i"
      state_matrix[-fr_idx,1] <- "s"
      
      #create dosage matrix 
      dosage_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = 200)
      dosage_matrix[fr_idx,] <- NA
      dosage_matrix[-fr_idx,] <- 0
      
      #calculate dosage from first responders
      for (z in fr_ID) {
        for (j in 1:length(drop_eel_IDs)) {
          focal_eel_ID <- drop_eel_IDs[j]
          if (state_matrix[j,1] == "i") {
            dosage_matrix[j,1] <- NA
          } else {
            wij <- weight_strengths[[colony_idx]][focal_eel_ID, z]
            if (rbinom(1,1,wij*max_rate*dt)== 1) {
              dosage_matrix[j,1] <- dosage_matrix[j,1] + da
            } else {
              
            }
          }
        }
      }
      
      #for each time step 
      for (k in 2:20) {
        K <- sum(state_matrix[,k-1] == "s")
        for (j in 1:length(drop_eel_IDs)) {
          focal_eel_ID <- drop_eel_IDs[j]
          
          #Assigning states
          if (state_matrix[j,k-1] == "r") { #if eel is recovered
            state_matrix[j,k] <- "r"
            dosage_matrix[j,k] <- NA
          }
          else if (state_matrix[j,k-1] == "i") { #if eel is infected
            dosage_matrix[j,k] <- NA #state and frame recorder matrices stay the same
            if (k-tr <= 0) {
              state_matrix[j,k] <- "i"
              
              #dose everyone
              for (jj in 1:length(drop_eel_IDs)) {
                buddy_eel_ID <- drop_eel_IDs[jj]
                wij <- weight_strengths[[colony_idx]][focal_eel_ID, buddy_eel_ID]
                
                if (rbinom(1,1,wij*max_rate*dt) == 1) {
                  dosage_matrix[jj,k] <- dosage_matrix[jj,k] + da
                } else {
                  
                }
              }
            } else { 
              if (state_matrix[j,k-tr] == "i") {
                state_matrix[j,k] <- "r"
              } else {
                state_matrix[j,k] <- "i"
                
                #dose everyone
                for (jj in 1:length(drop_eel_IDs)) {
                  buddy_eel_ID <- drop_eel_IDs[jj]
                  wij <- weight_strengths[[colony_idx]][focal_eel_ID, buddy_eel_ID]
                  
                  if (rbinom(1,1,wij*max_rate*dt) == 1) {
                    dosage_matrix[jj,k] <- dosage_matrix[jj,k] + da
                  } else {
                    
                  }
                }
              }
            }
          } else { #eel is susceptible to hide
            #calculate cumulative for the last x time steps
            #dosage_matrix[j] <- 0
            
            #choose to hide based on dosage from the last tm steps
            
            tm <- 5
            
            if (k < tm+1) {
              tm <- k - 1
            } 
            
            cuml_dose <- 0
            for (t in (k-tm):(k-1)) {
              cuml_dose <- dosage_matrix[j,t] + cuml_dose
            }
            
            norm_cuml_dose <- cuml_dose/K
            
            if (norm_cuml_dose > threshold) {
              state_matrix[j,k] <- "i"
              social_frame_recorder_matrix[j] <- k
              
              #dose everyone else
              for (jj in 1:length(drop_eel_IDs)) {
                buddy_eel_ID <- drop_eel_IDs[jj]
                wij <- weight_strengths[[colony_idx]][focal_eel_ID, buddy_eel_ID]
                
                if (rbinom(1,1,wij*max_rate*dt) == 1) {
                  dosage_matrix[jj,k] <- dosage_matrix[jj,k] + da
                } else {
                  
                }
              }
              
            } else {
              state_matrix[j,k] <- "s"
              dosage_matrix[j,k] <- dosage_matrix[j,k]
            }
          }
        }
      }
    }
    social_frame_recorder_list[[i]][[sim]] <- social_frame_recorder_matrix
  }
}


## Non social model ##
n_sims <- 200
non_social_frame_recorder_list <- vector(mode="list", length = length(unique(data$drop_ID)))
names(non_social_frame_recorder_list) <- unique(data$drop_ID)


#2 - simulating the cascade at each drop 
for (i in unique(data$drop_ID)) {
  
  print(i)
  
  non_social_frame_recorder_list[[i]] <- vector(mode = "list", length = n_sims)
  
  for (sim in 1:n_sims) {
    
    #Calculate which individuals are emerged 
    drop_data <- data %>%
      filter(drop_ID == i & !is.na(full_partial_none) & !is.na(base_x_cam1) & !is.na(base_x_cam2)) # & !is.na(dist_from_first_resp)
    
    drop_eel_IDs <- unique(drop_data$colony_eel_ID)
    
    resp_data <- as.data.frame(matrix(nrow=length(drop_eel_IDs),ncol=3))
    
    colony_idx <- which(unique(data$colony) == first(drop_data$colony))
    
    #create a frame recorder matrix
    non_social_frame_recorder_matrix <- matrix(nrow=length(drop_eel_IDs), dimnames=list(drop_eel_IDs, NULL))
    
    
    #determine first responder
    for (h in 1:length(drop_eel_IDs)) {
      l_drop_ID <- first(drop_data$drop_ID)
      l_colony_eel_ID <- drop_data$colony_eel_ID[h]
      l_date <- first(drop_data$date)
      l_colony <- first(drop_data$colony)
      #for each eel i in drop j nested in colony k, compute the linear predictor
      eta_j <- fr_intercept + fr_b_dist*(drop_data$distance_to_ball[h]) + fr_re_drop_ID$"(Intercept)"[fr_re_drop_ID$combo == l_drop_ID] + fr_re_colony_colony_eel_ID$"(Intercept)"[as.character(fr_re_colony_colony_eel_ID$name) == l_colony_eel_ID] + fr_re_date$"(Intercept)"[fr_re_date$combo == l_date] + fr_re_colony$"(Intercept)"[fr_re_colony$combo == l_colony]
      #convert this to a standard logistic transform - gives probability per eel
      p_respond <- 1/(1+exp(-eta_j))
      resp_data[h,1] <- l_colony_eel_ID
      resp_data[h,2] <- p_respond
      resp_data[h,3] <- rbinom(n = 1, size = 1, prob = p_respond)
    }
    
    #if there is a first responder
    if (sum(resp_data[,3], na.rm = TRUE) > 0) {
      
      #find IDs of first responder
      fr_ID <- resp_data$V1[resp_data$V3 == 1]
      
      #find index of first responder
      fr_idx <- which(drop_eel_IDs %in% fr_ID)
      
      non_social_frame_recorder_matrix[fr_idx] <- 1
      
      #create state matrix 
      state_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = 200)
      state_matrix[fr_idx,1] <- "i"
      state_matrix[-fr_idx,1] <- "s"
      
      #Compute second responders
      for (h in 1:length(drop_eel_IDs)) {
        if (state_matrix[h,1] == "i") {
        } else {
          l_drop_ID <- first(drop_data$drop_ID)
          l_colony_eel_ID <- drop_data$colony_eel_ID[h]
          l_date <- first(drop_data$date)
          l_colony <- first(drop_data$colony)
          eta_j <- subs_intercept + subs_b_dist_ball*(drop_data$distance_to_ball[h])
          p_respond <- 1/(1+exp(-eta_j))
          if (rbinom(n=1,size=1, p_respond) == 1) {
            state_matrix[h,1] <- "i"
            non_social_frame_recorder_matrix[h] <- 2
          } else {
            state_matrix[h,1] <- "s"
          }
        }
      }
    }
    
    non_social_frame_recorder_list[[i]][[sim]] <- non_social_frame_recorder_matrix
  }
}

#Response vs no confusion matrix 
library(tidyverse)
library(ggplot2)

# --- Observed: did any eel respond in this drop? ---
observed_drop <- data %>%
  filter(!is.na(full_partial_none) & !is.na(base_x_cam1) & !is.na(base_x_cam2)) %>%
  group_by(drop_ID) %>%
  summarise(obs_responded = any(!is.na(response_frame_cam1)), .groups = "drop") %>%
  mutate(drop_ID = as.character(drop_ID))

# --- Helper: one prediction per drop per sim ---
get_confusion_counts <- function(recorder_list, observed_df, n_sims) {
  
  rows <- list()
  
  for (i in seq_along(recorder_list)) {
    drop_id <- names(recorder_list)[i]
    sims    <- recorder_list[[i]]
    
    obs <- observed_df$obs_responded[observed_df$drop_ID == drop_id]
    if (length(obs) == 0) next
    
    for (sim in sims) {
      pred_responded <- any(!is.na(sim))
      
      if (obs & pred_responded)   category <- "True Positive"
      if (obs & !pred_responded)  category <- "False Negative"
      if (!obs & pred_responded)  category <- "False Positive"
      if (!obs & !pred_responded) category <- "True Negative"
      
      rows <- append(rows, list(data.frame(
        drop_ID  = as.character(drop_id),
        category = as.character(category)
      )))
    }
  }
  
  bind_rows(rows) %>%
    group_by(category) %>%
    summarise(total_count = n(), .groups = "drop")
}

# --- Compute for both models ---
social_counts     <- get_confusion_counts(social_frame_recorder_list,     observed_drop, n_sims)
non_social_counts <- get_confusion_counts(non_social_frame_recorder_list, observed_drop, n_sims)

social_counts$model     <- "Social"
non_social_counts$model <- "Non-Social"

# --- Ensure all four categories present for both models ---
all_categories <- c("True Positive", "False Negative", "False Positive", "True Negative")

all_counts <- bind_rows(social_counts, non_social_counts) %>%
  complete(model, category = all_categories, fill = list(total_count = 0)) %>%
  mutate(
    x_label = case_when(
      category %in% c("True Positive", "False Negative") ~ "Observed:\nResponded",
      TRUE                                                ~ "Observed:\nDid Not Respond"
    ),
    y_label = case_when(
      category %in% c("True Positive", "False Positive") ~ "Predicted:\nResponded",
      TRUE                                                ~ "Predicted:\nDid Not Respond"
    ),
    x_label = factor(x_label, levels = c("Observed:\nResponded", "Observed:\nDid Not Respond")),
    y_label = factor(y_label, levels = c("Predicted:\nResponded", "Predicted:\nDid Not Respond"))
  ) %>%
  group_by(model, x_label) %>%
  mutate(proportion = total_count / sum(total_count)) %>%
  ungroup()

# --- Plot ---
ggplot(all_counts, aes(x = x_label, y = y_label, fill = proportion)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = paste0(category, "\n", scales::percent(proportion, accuracy = 1))),
            size = 3.5, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#d1e5f0", high = "#2166ac",
                      labels = scales::percent,
                      name = "Proportion\nof observed") +
  facet_wrap(~model, ncol = 2) +
  labs(
    title = "Model Prediction Accuracy vs Observed Responses",
    x = "Observed",
    y = "Predicted"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid      = element_blank(),
    strip.text      = element_text(face = "bold", size = 14),
    axis.text       = element_text(size = 11),
    legend.position = "right"
  )

#Cascade vs single response
# --- Observed: among drops with a response, single vs cascade ---
observed_drop_cascade <- data %>%
  filter(!is.na(full_partial_none) & !is.na(base_x_cam1) & !is.na(base_x_cam2)) %>%
  group_by(drop_ID) %>%
  summarise(n_responded = sum(!is.na(response_frame_cam1)), .groups = "drop") %>%
  filter(n_responded > 0) %>%  # only drops where someone responded
  mutate(
    drop_ID = as.character(drop_ID),
    obs_cascade = n_responded > 1
  )

# --- Helper ---
get_confusion_counts_cascade <- function(recorder_list, observed_df, n_sims) {
  
  rows <- list()
  
  for (i in seq_along(recorder_list)) {
    drop_id <- names(recorder_list)[i]
    sims    <- recorder_list[[i]]
    
    obs <- observed_df$obs_cascade[observed_df$drop_ID == drop_id]
    if (length(obs) == 0) next  # skips drops with no observed response
    
    for (sim in sims) {
      pred_cascade <- sum(!is.na(sim)) > 1
      
      if (obs & pred_cascade)   category <- "True Positive"
      if (obs & !pred_cascade)  category <- "False Negative"
      if (!obs & pred_cascade)  category <- "False Positive"
      if (!obs & !pred_cascade) category <- "True Negative"
      
      rows <- append(rows, list(data.frame(drop_ID  = as.character(drop_id),
                                           category = as.character(category))))
    }
  }
  
  bind_rows(rows) %>%
    group_by(category) %>%
    summarise(total_count = n(), .groups = "drop")
}

# --- Compute for both models ---
social_counts_cascade     <- get_confusion_counts_cascade(social_frame_recorder_list,     observed_drop_cascade, n_sims)
non_social_counts_cascade <- get_confusion_counts_cascade(non_social_frame_recorder_list, observed_drop_cascade, n_sims)

social_counts_cascade$model     <- "Social"
non_social_counts_cascade$model <- "Non-Social"

# --- Ensure all four categories present ---
all_categories <- c("True Positive", "False Negative", "False Positive", "True Negative")

all_counts_cascade <- bind_rows(social_counts_cascade, non_social_counts_cascade) %>%
  complete(model, category = all_categories, fill = list(total_count = 0)) %>%
  mutate(
    x_label = case_when(
      category %in% c("True Positive", "False Negative") ~ "Observed:\nCascade",
      TRUE                                                ~ "Observed:\nSingle Response"
    ),
    y_label = case_when(
      category %in% c("True Positive", "False Positive") ~ "Predicted:\nCascade",
      TRUE                                                ~ "Predicted:\nSingle Response"
    ),
    x_label = factor(x_label, levels = c("Observed:\nCascade", "Observed:\nSingle Response")),
    y_label = factor(y_label, levels = c("Predicted:\nCascade", "Predicted:\nSingle Response"))
  ) %>%
  group_by(model, x_label) %>%
  mutate(proportion = total_count / sum(total_count)) %>%
  ungroup()

# --- Plot ---
ggplot(all_counts_cascade, aes(x = x_label, y = y_label, fill = proportion)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = paste0(category, "\n", scales::percent(proportion, accuracy = 1))),
            size = 3.5, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#d1e5f0", high = "#2166ac",
                      labels = scales::percent,
                      name = "Proportion\nof observed") +
  facet_wrap(~model, ncol = 2) +
  labs(
    title = "Cascade vs Single Response Prediction\n(drops with at least one responder only)",
    x = "Observed",
    y = "Predicted"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid      = element_blank(),
    strip.text      = element_text(face = "bold", size = 14),
    axis.text       = element_text(size = 11),
    legend.position = "right"
  )

#social vs non social difference in number of responders
# --- Observed: mean number of responders per drop ---
observed_cascade_size <- data %>%
  filter(!is.na(full_partial_none) & !is.na(base_x_cam1) & !is.na(base_x_cam2)) %>%
  group_by(drop_ID) %>%
  summarise(n_responded = sum(!is.na(response_frame_cam1)), .groups = "drop") %>%
  mutate(drop_ID = as.character(drop_ID))

# --- Helper: mean responders per sim, then difference to observed ---
get_cascade_size_diff <- function(recorder_list, observed_df, n_sims, model_name) {
  
  rows <- list()
  
  for (i in seq_along(recorder_list)) {
    drop_id <- names(recorder_list)[i]
    sims    <- recorder_list[[i]]
    
    obs_n <- observed_df$n_responded[observed_df$drop_ID == drop_id]
    if (length(obs_n) == 0) next
    
    # mean number of responders across sims for this drop
    mean_pred_n <- mean(sapply(sims, function(sim) sum(!is.na(sim))))
    
    rows <- append(rows, list(data.frame(
      drop_ID    = as.character(drop_id),
      obs_n      = obs_n,
      pred_n     = mean_pred_n,
      difference = mean_pred_n - obs_n,
      model      = model_name
    )))
  }
  
  bind_rows(rows)
}

# --- Compute for both models ---
social_diff     <- get_cascade_size_diff(social_frame_recorder_list,     observed_cascade_size, n_sims, "Social")
non_social_diff <- get_cascade_size_diff(non_social_frame_recorder_list, observed_cascade_size, n_sims, "Non-Social")

all_diff <- bind_rows(social_diff, non_social_diff) %>%
  mutate(model = factor(model, levels = c("Social", "Non-Social")))

# --- Plot ---
ggplot(all_diff, aes(x = model, y = difference, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8, width = 0.5) +
  scale_fill_manual(values = c("Social" = "#2166ac", "Non-Social" = "#E57373")) +
  labs(
    title = "Predicted vs Observed Cascade Size",
    subtitle = "Difference = mean predicted responders - observed responders per drop",
    x = "",
    y = "Difference in number of responders\n(predicted - observed)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

#Timing
# --- Observed: frame number relative to first responder ---
observed_timing <- data %>%
  filter(!is.na(full_partial_none) & !is.na(base_x_cam1) & !is.na(base_x_cam2) & 
           !is.na(response_frame_cam1)) %>%
  group_by(drop_ID) %>%
  mutate(
    first_frame    = min(response_frame_cam1, na.rm = TRUE),
    relative_frame = response_frame_cam1 - first_frame
  ) %>%
  ungroup() %>%
  mutate(drop_ID = as.character(drop_ID),
         source  = "Observed") %>%
  filter(relative_frame < 200)

# --- Predicted: extract frame numbers from social model, relative to first responder ---
sim_timing_rows <- list()

for (i in seq_along(social_frame_recorder_list)) {
  drop_id <- names(social_frame_recorder_list)[i]
  sims    <- social_frame_recorder_list[[i]]
  
  for (sim_idx in seq_along(sims)) {
    sim <- sims[[sim_idx]]
    
    frames <- sim[!is.na(sim)]
    
    if (length(frames) == 0) next
    
    first_frame <- min(frames)
    
    for (f in frames) {
      sim_timing_rows <- append(sim_timing_rows, list(data.frame(
        drop_ID        = as.character(drop_id),
        sim_idx        = sim_idx,
        relative_frame = f - first_frame,
        source         = "Social Model"
      )))
    }
  }
}

sim_timing <- bind_rows(sim_timing_rows)

# --- Combine ---
all_timing <- bind_rows(
  observed_timing %>% dplyr::select(drop_ID, relative_frame, source),
  sim_timing      %>% dplyr::select(drop_ID, relative_frame, source)
) %>%
  mutate(source = factor(source, levels = c("Observed", "Social Model")))

# --- Plot ---
ggplot(all_timing, aes(x = source, y = relative_frame, fill = source)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8, width = 0.5) +
  scale_fill_manual(values = c("Observed" = "#4CAF50", "Social Model" = "#2166ac")) +
  labs(
    title = "Timing of Individual Responses",
    subtitle = "Frame number relative to first responder (0 = first responder)",
    x = "",
    y = "Relative response frame"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank()
  )


#Time series subset
library(tidyverse)
library(ggplot2)

DROP_SUBSET <- unique(data$drop_ID)[1:8]

# ── Max frame from subset only ───────────────────────────────────────────────
max_frame <- data %>%
  filter(drop_ID %in% DROP_SUBSET,
         !is.na(response_frame_cam1)) %>%
  group_by(drop_ID) %>%
  mutate(first_frame    = min(response_frame_cam1, na.rm = TRUE),
         relative_frame = response_frame_cam1 - first_frame + 1) %>%
  ungroup() %>%
  summarise(max_rf = max(relative_frame, na.rm = TRUE)) %>%
  pull(max_rf)

# ── Observed (per frame) ─────────────────────────────────────────────────────
obs_long <- data %>%
  filter(drop_ID %in% DROP_SUBSET,
         !is.na(full_partial_none),
         !is.na(base_x_cam1),
         !is.na(base_x_cam2),
         !is.na(response_frame_cam1)) %>%
  group_by(drop_ID) %>%
  mutate(first_frame    = min(response_frame_cam1, na.rm = TRUE),
         relative_frame = response_frame_cam1 - first_frame + 1) %>%
  ungroup() %>%
  group_by(drop_ID, relative_frame) %>%
  summarise(n_hides = n_distinct(colony_eel_ID), .groups = "drop") %>%
  mutate(model = "Observed")

# ── Social model (per frame, no scaling) ─────────────────────────────────────
social_long <- map_dfr(DROP_SUBSET, function(did) {
  sims <- social_frame_recorder_list[[as.character(did)]]
  if (is.null(sims)) return(NULL)
  
  map_dfr(seq_along(sims), function(s) {
    sim <- sims[[s]]
    if (is.null(sim) || all(is.na(sim))) return(NULL)
    
    frames <- sim[!is.na(sim)]
    first  <- min(frames)
    
    tibble(
      drop_ID = did,
      relative_frame = (frames - first + 1),
      sim = s
    )
  })
}) %>%
  group_by(drop_ID, relative_frame, sim) %>%
  summarise(n_hides = n(), .groups = "drop") %>%
  group_by(drop_ID, relative_frame) %>%
  summarise(n_hides = mean(n_hides), .groups = "drop") %>%
  mutate(model = "Social model")

# ── Combine and fill missing frames ──────────────────────────────────────────
plot_data <- bind_rows(obs_long, social_long) %>%
  complete(drop_ID = DROP_SUBSET,
           relative_frame = 1:max_frame,
           model = c("Observed", "Social model"),
           fill = list(n_hides = 0)) %>%
  mutate(model = factor(model, levels = c("Observed", "Social model")))

# ── Plot ─────────────────────────────────────────────────────────────────────
ggplot(plot_data, aes(x = relative_frame,
                      y = factor(drop_ID),
                      fill = n_hides)) +
  geom_tile() +
  facet_grid(model ~ ., scales = "free_y", space = "free_y") +
  scale_fill_gradient(
    low  = "#e8f4f8",
    high = "#042c53",
    name = "Eels hiding"
  ) +
  scale_x_continuous(
    name = "Frame (relative to first responder)",
    breaks = seq(0, max_frame, by = 10),  # adjust spacing if needed
    expand = c(0, 0)
  ) +
  labs(y = "Drop ID") +
  theme_minimal(base_size = 12) +
  theme(
    strip.text      = element_text(face = "bold"),
    panel.grid      = element_blank(),
    panel.spacing.y = unit(0.8, "lines")
  )
