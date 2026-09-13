library(datawizard)

evaluateHides <- function(sim_trials, coords, initator_responder) {
  
  #load
  orig_topo_mean <- 2.573319
  orig_topo_sd <- 0.9455378
  ball_decay_time_coef <- 0 #GET FROM FITTED MOD
  social_decay_time_coef <- 0.5 #GET FROM FITTED MOD
  social_threshold <- 12 #GET FROM FITTED MOD
  fractional_contagion_first <- TRUE #GET FROM FITTED MOD
  fractional_contagion_subs <- TRUE #GET FROM FITTED MOD
  tb <- 50
  tr <- 30
  tm <- 200
  max_rate <- 1
  dt <- 1
  da <- 1
  
  coefs <- list() #(all on scaled!)
  coefs[1] <- -2.56 #fr intercept NA est
  coefs[2] <- -2.06 #fr log distance from ball NA est
  coefs[3] <- -4.11 #sr intercept
  coefs[4] <- -0.64 #sr log inst topo dist
  coefs[5] <- -0.64 #sr log distance from ball
  
  social_private_frame_recorder_list <- vector(mode="list", length = length(unique(sim_trials$drop_ID)))
  names(social_private_frame_recorder_list) <- unique(sim_trials$drop_ID)
  
  for (i in unique(sim_trials$drop_ID)) {
    
    n_sims <- 1 #presuming we want 1 sim? could do more though
    social_private_frame_recorder_list[[as.character(i)]] <- vector(mode = "list", length = n_sims)
    
    #extract inst emerged number
    inst_emerged_count <- sim_trials$inst_emerged_numbers[i]
    
    #extract inst emerged IDs
    drop_eel_IDs <- sample(coords$ID, inst_emerged_count)
    
    #compute distances to the ball
    drop_data <- coords %>%
      filter(ID %in% drop_eel_IDs)
    
    #extract sim info of interest
    sim_trial_data <- sim_trials[i,]
    
    #add in simID
    drop_data$sim_ID <- i
    sim_trial_data$sim_ID <- i
    
    sim_trial_data <- left_join(drop_data, sim_trial_data, by = "sim_ID")
    
    #distance to ball
    sim_trial_data$distance_to_ball <- sqrt((sim_trial_data$positions_X - sim_trial_data$x_values)^2 + (sim_trial_data$positions_Y - sim_trial_data$y_values)^2 + (sim_trial_data$positions_Z - sim_trial_data$z_values)^2)
    sim_trial_data$log_distance_to_ball <- log(sqrt((sim_trial_data$positions_X - sim_trial_data$x_values)^2 + (sim_trial_data$positions_Y - sim_trial_data$y_values)^2 + (sim_trial_data$positions_Z - sim_trial_data$z_values)^2))
    
    #Instantaneous ranks to each other 
    for (gg in unique(sim_trial_data$ID)) { #for each eel
        focal <- gg
        focal_positions <- NA
        sim_trial_data_n <- sim_trial_data
        #calculate others distance to focal
        sim_trial_data_n$distance_to_focal <- sqrt((sim_trial_data$positions_X - sim_trial_data$positions_X[sim_trial_data$ID == focal])^2 + (sim_trial_data$positions_Y - sim_trial_data$positions_Y[sim_trial_data$ID == focal])^2 + (sim_trial_data$positions_Z - sim_trial_data$positions_Z[sim_trial_data$ID == focal])^2)
        #calculate others ranked distance to focal
        sim_trial_data_n$rank <- rank(sim_trial_data_n$distance_to_focal, na.last = "keep", ties.method = "first")
        #order others by rank
        sim_trial_data_ranked <- sim_trial_data_n[order(sim_trial_data_n$rank),]
        #remove self (where distance to focal is 0)
        sim_trial_data_ranked <- sim_trial_data_ranked[sim_trial_data_ranked$distance_to_focal != 0,]
        #convert ranked ID list to vector
        colony_eel_ID_ranked_for_focal <- as.vector(sim_trial_data_ranked$ID)
        #Insert list
        sim_trial_data$inst_neighbours_topo_ranked[sim_trial_data$ID == gg] <- list(colony_eel_ID_ranked_for_focal)
    }

    
    if (fractional_contagion_first == TRUE) {
      K_first <- length(drop_eel_IDs)
    } else {
      K_first <- 1
    }
    
    #if (is.na(sum(drop_data$distance_to_ball, na.rm=TRUE))) next
    
    if (length(drop_eel_IDs) < 1) next #check if this is necessary / what this is doing and if it messes up vector
    
    for (sim in 1:n_sims) {
      
      #print("starting sim")
      #print(sim)
      #create a frame recorder matrix
      social_private_frame_recorder_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = 4, dimnames=list(drop_eel_IDs, NULL))
      
      resp_data <- as.data.frame(matrix(nrow=length(drop_eel_IDs),ncol=4))

      #check if fractional contagion is on
      if (fractional_contagion_subs == TRUE) {
        K <- length(drop_eel_IDs) - 1
      } else {
        K <- 1
      }
      
      #Draw thresholds
      theta_max <- 2*social_threshold
      drop_eel_ID_thresholds <- runif(length(drop_eel_IDs), min = 0, max = theta_max)
      
      #time step 1
      #k <- 1
      
      #determine first responder
      for (h in 1:length(drop_eel_IDs)) {
        #for each eel i in drop j nested in colony k, compute the linear predictor
        eta_j <- as.numeric(coefs[1]) + as.numeric(coefs[2])*(drop_data$log_distance_to_ball_sc[h]) #RE removed for now... + fr_re_drop_ID$"(Intercept)"[fr_re_drop_ID$combo == l_drop_ID] + fr_re_colony_colony_eel_ID$"(Intercept)"[as.character(fr_re_colony_colony_eel_ID$name) == l_colony_eel_ID] + fr_re_date$"(Intercept)"[fr_re_date$combo == l_date] + fr_re_colony$"(Intercept)"[fr_re_colony$combo == l_colony]
        #convert this to a standard logistic transform - gives probability per eel
        p_private_cue <- 1/(1+exp(-eta_j))
        resp_data[h,1] <- l_colony_eel_ID
        resp_data[h,2] <- p_private_cue
        if (is.na(p_private_cue) | p_private_cue == 0) {
          print("zero prob")
          print(first(drop_data$drop_ID))
        } else {
          
        }
        resp_data[h,3] <- rbinom(n = 1, size = 1, prob = p_private_cue)
        resp_data[h,4] <- resp_data[h,3]#/K_first
        #resp_data[h,5] <- ifelse(resp_data[h,4] > private_threshold, 1, 0) #private threshold be on scale between 0 and 1
      }
      
      #create state matrix 
      state_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = n_time)
      state_matrix[,1] <- "s"
      #create dosage matrix 
      dosage_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = n_time)
      dosage_matrix[,] <- 0
      
      #if there is a first responder
      if (sum(resp_data[,4], na.rm = TRUE) > 0) { #is there something weird happening that with no first responders, eels never get logged as suspectible?
        
        #find IDs of first responder
        fr_ID <- resp_data$V1[resp_data$V4 == 1]
        
        #find index of first responder
        fr_idx <- which(drop_eel_IDs %in% fr_ID)
        
        social_private_frame_recorder_matrix[fr_idx,1] <- 1
        
        for (fr_IDD in fr_idx) {
          state_matrix[fr_IDD,1] <- "i"
        }
        
        state_matrix[-fr_idx,1] <- "s"
        
        dosage_matrix[fr_idx,] <- NA
        dosage_matrix[-fr_idx,] <- 0
        
      }
      
      #for each time step 
      for (k in 2:n_time) {
        
        #keep last state as abase
        state_matrix[, k] <- state_matrix[, k-1]
        
        
        current_doses <- rep(0, length(drop_eel_IDs))
        
        #Phase 1: broadcast doses
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
              state_matrix[j,k] <- "i"
              
              for (jj in 1:length(drop_eel_IDs)) {
                if (jj == j || state_matrix[jj, k-1] != "s") next
                buddy_eel_ID <- drop_eel_IDs[jj]
                
                buddy_neighbours_ranked <- drop_data$inst_neighbours_topo_ranked[which(drop_data$colony_eel_ID == buddy_eel_ID)]
                rank <- which(buddy_neighbours_ranked[[1]] == focal_eel_ID)
                if (length(rank) == 0 || is.na(rank)) {
                  next 
                }
                log_inst_topo_dist_sc <- (log(rank) - orig_topo_mean) / orig_topo_sd
                
                if (k < tb) {
                  eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[5])*(drop_data$log_distance_to_ball_sc[jj]) - private_decay_time_coef*log(k) #- social_decay_time_coef*log(frames_since_infected)
                } else {
                  eta_j <- as.numeric(coefs[3]) #- social_decay_time_coef*log(frames_since_infected)
                }
                
                w_ij <- 1/(1+exp(-eta_j))
                p_dose <- w_ij*max_rate*dt
                
                if (rbinom(1,1,p_dose) == 1) {
                  current_doses[jj] <- current_doses[jj] + da
                  
                }
              }
            }
          }
        }
        
        for (j in 1:length(drop_eel_IDs)) {
          if (state_matrix[j, k-1] == "s") {
            dosage_matrix[j,k] <- current_doses[j]
          } else {
            dosage_matrix[j,k] <- NA
          }
        }
        
        #Phase 2: Integration and infection
        
        for (j in 1:length(drop_eel_IDs)) {
          
          focal_eel_ID <- drop_eel_IDs[j]
          
          if (state_matrix[j, k-1] == "s") {
            
            #check for response to social
            tm_frames <- round(tm/dt)
            window_start <- max(1, k-tm_frames)
            
            cuml_dose <- sum(dosage_matrix[j, window_start:k], na.rm=TRUE)
            
            norm_cuml_dose <- cuml_dose#/K #try take out K, see if fits better
            
            
            if (norm_cuml_dose > drop_eel_ID_thresholds[j]) {
              response <- 1
            } else {
              response <- 0 
            }
            
            if (!is.na(response)) {
              if (response == 1) {
                
                state_matrix[j,k] <- "i"
                social_private_frame_recorder_matrix[j,1] <- k
                social_private_frame_recorder_matrix[j,2] <- norm_cuml_dose
              } else {
                state_matrix[j,k] <- "s"
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
  
    
    





