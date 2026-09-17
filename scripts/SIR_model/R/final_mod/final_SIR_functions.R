cascade_size_time_nll <- function(par, model, data_clean, initator_responder, coefs, n_sims, fixed, n_time, time_tol, PRIVATE_ONLY, SOCIAL_ONLY, NULL_MODEL, fractional_contagion_subs) {
  
  time_tol <- time_tol
  n_time <- 208
  n_sims <- 2
  
  model_result <- model(data_clean, initator_responder, par, coefs, n_sims, fixed, n_time, PRIVATE_ONLY, SOCIAL_ONLY, NULL_MODEL, fractional_contagion_subs)
  
  #Calculate log probabilities of cascade size
  experimental_cascade_size <- data_clean %>%
    group_by(drop_ID) %>%
    summarise(n_responders = first(n_responders),
              timing_extent = ifelse(n_responders == 0, NA, max(response_frame_cam1[!is.na(response_frame_cam1)], na.rm=TRUE) - 
                                       min(response_frame_cam1[!is.na(response_frame_cam1)], na.rm=TRUE)))
  
  loglik <- 0
  
  for (ii in experimental_cascade_size$drop_ID) {
    
    target_row <- experimental_cascade_size[experimental_cascade_size$drop_ID == ii, ]
    
    if (nrow(target_row) == 0 || is.na(target_row$n_responders)) next
    
    target_size <- target_row$n_responders
    target_timing <- target_row$timing_extent
    
    sims <- model_result[[as.character(ii)]]
    
    sim_size <- sapply(sims, function(sim_res) {
      sum(!is.na(sim_res[,1]))
    })
    
    sim_timing <- sapply(sims, function(sim_res) {
      rt <- sim_res[, 1]
      rt <- rt[!is.na(rt)]
      
      if (length(rt) < 2) return(NA_real_)
      max(rt) - min(rt)
    })
    
    #p_size
    hits <- sum(sim_size == target_size, na.rm=TRUE)
    p_size <- hits / length(sim_size)
    p_size <- max(p_size, 1/n_sims) # safety floor
    
    #p_timing
    if (is.na(target_timing)) {
      p_time <- NA
    } else {
      hits_time <- sum(abs(sim_timing - target_timing) <= time_tol, na.rm=TRUE)
      p_time <- max(hits_time / length(sim_timing), 1/n_sims)
    }
    
    loglik <- loglik + log(p_size)
    if (!is.na(p_time)) loglik <- loglik + log(p_time)
    
    
  }
  cat("NLL:", -loglik, "\n")
  flush.console()
  
  return(list(
    nll = -loglik,
    model_result = model_result))
  
}


social_private_model <- function(data_clean, initator_responder, params, coefs, n_sims, fixed, n_time, PRIVATE_ONLY, SOCIAL_ONLY, NULL_MODEL, fractional_contagion_subs) {
  
  data <- data_clean
  n_sims <- n_sims
  coefs <- coefs
  n_time <- n_time
  
  drop_data_groups <- data_clean %>% 
    filter(emerged == 1, !is.na(global_X), !is.na(ball_global_X))
  
  drop_data_list <- data_clean %>% 
    filter(emerged == 1, !is.na(global_X), !is.na(ball_global_X)) %>% 
    split(.$drop_ID) # Natively names the list items by drop_ID
  
  orig_topo_mean <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:center")
  orig_topo_sd   <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:scale")
  
  social_threshold <- as.numeric(params['social_threshold'])
  private_threshold <- as.numeric(params['private_threshold'])
  tm <- 204 #whole time
  tr <- 30 + 5 #retraction period + sensory motor delay
  tb <- 204 #whole time
  max_rate <- as.numeric(fixed['max_rate'])
  dt <- as.numeric(fixed['dt'])
  da <- as.numeric(fixed['da'])
  
  #ball_decay_time_coef <- as.numeric(params['ball_decay_time_coef'])
  #social_decay_time_coef <- as.numeric(params['social_decay_time_coef'])
  #tr <- as.numeric(fixed['tr']) #30
  #tb <- 50
  #tr <- 30
  #tm <- as.numeric(fixed['tm'])
  #tm <- 200
  #fractional_contagion_first <- as.logical(fixed['fractional_contagion_first'])

  #set recorder list for each drop
  social_private_frame_recorder_list <- vector(mode="list", length = length(unique(data$drop_ID)))
  names(social_private_frame_recorder_list) <- unique(data$drop_ID)
  
  #for each drop
  for (i in unique(data$drop_ID)) {
    
    #initiate a frame recorder list for drop
    social_private_frame_recorder_list[[as.character(i)]] <- vector(mode = "list", length = n_sims)
    
    #Calculate which individuals are emerged 
    drop_data <- drop_data_list[[as.character(i)]]
    if (is.null(drop_data) || nrow(drop_data) == 0) next
    
    drop_eel_IDs <- unique(drop_data$colony_eel_ID)
    
    #if (is.na(sum(drop_data$distance_to_ball, na.rm=TRUE))) next
    
    #if only no eels up skip
    if (length(drop_eel_IDs) < 1) next #check if this is necessary / what this is doing and if it messes up vector
    
    for (sim in 1:n_sims) {
      
      #create a frame recorder matrix for the simulation
      social_private_frame_recorder_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = 5, dimnames=list(drop_eel_IDs, NULL))
      
      #Initiate response df
      resp_data <- as.data.frame(matrix(nrow=length(drop_eel_IDs),ncol=4))
      
      colony_idx <- which(unique(data$colony) == first(drop_data$colony))
      
      #check if fractional contagion is on
      if (fractional_contagion_subs == TRUE) {
        K <- length(drop_eel_IDs) - 1
      } else {
        K <- 1
      }
      
      #Draw thresholds from a uniform distribution around social threshold
      theta_max <- 2*social_threshold
      drop_eel_ID_thresholds <- runif(length(drop_eel_IDs), min = 0, max = theta_max)
      
      #create state matrix 
      state_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = n_time)
      state_matrix[,1] <- "s"
      #create dosage matrices
      social_dosage_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = n_time)
      social_dosage_matrix[,] <- 0
      private_dosage_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = n_time)
      private_dosage_matrix[,] <- 0
    
      #for each time step 
      for (k in 2:n_time) {
        
        #keep last state as a base
        state_matrix[, k] <- state_matrix[, k-1]
        
        #Reset timestep dose to 0
        current_social_doses <- rep(0, length(drop_eel_IDs))
        current_private_doses <- rep(0, length(drop_eel_IDs))
        
        #For each eel, update state
        for (j in 1:length(drop_eel_IDs)) {
          
          focal_eel_ID <- drop_eel_IDs[j]
          
          #Assigning states
          if (state_matrix[j,k-1] == "r") { #if eel is recovered
            state_matrix[j,k] <- "r"
          } else if (state_matrix[j,k-1] == "i") { #if eel is infected
            frames_since_infected <- k - social_private_frame_recorder_matrix[j,1]
            if (!is.na(frames_since_infected) && (frames_since_infected*dt >= tr)) {
              state_matrix[j,k] <- "r" #recover
            } else {
              state_matrix[j,k] <- "i" #you're still infected
            } 
          } else if (state_matrix[j, k-1] == "s") { # if eel is suspectible
            
            #Evaluate cuml dose to see if switch to inf
            tm_frames <- round(tm/dt)
            window_start <- max(1, k-tm_frames)
            
            #social
            social_cuml_dose <- sum(social_dosage_matrix[j, window_start:k], na.rm=TRUE)
            norm_social_cuml_dose <- social_cuml_dose/K
            
            if (norm_social_cuml_dose > drop_eel_ID_thresholds[j]) {
              social_response <- 1
            } else {
              social_response <- 0 
            }
            
            private_cuml_dose <- sum(private_dosage_matrix[j, window_start:k], na.rm=TRUE)
            norm_private_cuml_dose <- private_cuml_dose/K
            
            if (norm_private_cuml_dose > drop_eel_ID_thresholds[j]) {
              private_response <- 1
            } else {
              private_response <- 0 
            }
            
            
            if (private_response == 1 | social_response == 1) {
              
              if (private_response == 1) {
                #transitions to infected
                state_matrix[j,k] <- "i"
                social_private_frame_recorder_matrix[j,1] <- k
                social_private_frame_recorder_matrix[j,2] <- 1
                social_private_frame_recorder_matrix[j,3] <- norm_private_cuml_dose
              } 
              
              if (social_response == 1) { 
                state_matrix[j,k] <- "i"
                social_private_frame_recorder_matrix[j,1] <- k
                social_private_frame_recorder_matrix[j,4] <- 2
                social_private_frame_recorder_matrix[j,5] <- norm_social_cuml_dose
                
              }
              
            } else {
                #stays suspectible
                state_matrix[j,k] <- "s"
                
                #evaluate if gets dosed at this step from everything that happened up to k-1
                #if there are any i at this step, get dosed socially + privately
                if (any(state_matrix[,k-1] == "i")) {
                  #get a dose for every infected individual
                  infected_eels_ID <- which(state_matrix[,k-1] == "i")
                  
                  for (inf in infected_eels_ID) {
                    inf_eel_ID <- drop_eel_IDs[inf]
                    focal_eel_ID_neighbours_ranked <- drop_data$inst_neighbours_topo_ranked[which(drop_data$colony_eel_ID == focal_eel_ID)]
                    rank <- which(focal_eel_ID_neighbours_ranked[[1]] == inf_eel_ID)
                    if (length(rank) == 0 || is.na(rank)) {
                      next
                    }
                    
                    log_inst_topo_dist_sc <- (log(rank) - orig_topo_mean) / orig_topo_sd
                    
                    eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[4])*log_inst_topo_dist_sc + as.numeric(coefs[5])*(drop_data$log_distance_to_ball_sc[drop_data$colony_eel_ID == focal_eel_ID])
                    
                    w_ij <- 1/(1+exp(-eta_j))
                    p_s_dose <- w_ij*max_rate*dt
                    
                    if (rbinom(1,1,p_s_dose) == 1) {
                      current_social_doses[j] <- current_social_doses[j] + da
                    }
                  }
                    
                } else { #there are no infecteds present, get dosed privately
                  
                  eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[5])*(drop_data$log_distance_to_ball_sc[drop_data$colony_eel_ID == focal_eel_ID])
                  
                  w_ij <- 1/(1+exp(-eta_j))
                  p_p_dose <- w_ij*max_rate*dt
                  
                  if (rbinom(1,1,p_p_dose) == 1) {
                    current_private_doses[j] <- current_private_doses[j] + da
                  }
                }
              } #end of no inf present
              }  #end of sus loop
          
          social_dosage_matrix[j,k] <- current_social_doses[j] #doses received from all infecteds at that time step
          private_dosage_matrix[j,k] <- current_private_doses[j] #doses received from ball if no infecteds at that time step
        } #end of scanning through individuals for that time step
        
      } #end of time step
      social_private_frame_recorder_list[[as.character(i)]][[sim]] <- social_private_frame_recorder_matrix
      
    } #end of simulation step
    
  } #end of drop
  return(social_private_frame_recorder_list)
}
    
    
      
            
            
            
            
            
            
            #if we are in a private only model, get dosed for just ball
            
            if (PRIVATE_ONLY || NULL_MODEL) {
              if (PRIVATE_ONLY) {
                if (k < tb) { #if we are within ball timestep
                  eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[5])*(drop_data$log_distance_to_ball_sc[j]) - ball_decay_time_coef*k #calculate linear predictor from this rank
                } else {
                  eta_j <- as.numeric(coefs[3])
                }
              } else if (NULL_MODEL) {
                eta_j <- as.numeric(coefs[3])
              } 
              
              w_ij <- 1/(1+exp(-eta_j))
              p_dose <- w_ij*max_rate*dt
              
              if (rbinom(1,1,p_dose) == 1) {
                current_doses[j] <- current_doses[j] + da
              }
              
            }
            
          }
          
          #put current doses in the dosage matrix for this time step
          if (state_matrix[j, k-1] == "s") {
            dosage_matrix[j,k] <- current_doses[j]
          } else {
            dosage_matrix[j,k] <- NA
          }
          
          
          #Phase 2: Accumulation and infection
          
          #for each eel
          for (j in 1:length(drop_eel_IDs)) {
            
            focal_eel_ID <- drop_eel_IDs[j]
            
            if (state_matrix[j, k-1] == "s") { #if they are suspectible
              
              #check for cumulative dosage
              tm_frames <- round(tm/dt)
              window_start <- max(1, k-tm_frames)
              cuml_dose <- sum(dosage_matrix[j, window_start:k], na.rm=TRUE)
              
              norm_cuml_dose <- cuml_dose/K #try take out K, see if fits better
              
              
              if (norm_cuml_dose > drop_eel_ID_thresholds[j]) {
                response <- 1
              } else {
                response <- 0 
              }
            
            }
            
          }
        }
      }
      social_private_frame_recorder_list[[as.character(i)]][[sim]] <- social_private_frame_recorder_matrix
    }
  }
  return(social_private_frame_recorder_list)
}



