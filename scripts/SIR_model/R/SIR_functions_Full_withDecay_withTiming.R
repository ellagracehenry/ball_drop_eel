cascade_size_time_nll <- function(par, model, data_clean, initator_responder, coefs, n_sims, fixed, n_time, time_tol) {
  
  n_sims <- 1
  
  time_tol <- time_tol
  
  model_result <- model(data_clean, initator_responder, par, coefs, n_sims, fixed, n_time)
  
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
    
    sim_size <- sapply(sims, function(sim_res) sum(!is.na(sim_res)))
    sim_timing <- sapply(sims, function(sim_res) {
      rt <- sim_res[!is.na(sim_res)]
      if (length(rt) < 2) return(NA_real_)
      max(rt) - min(rt)
    })

    #p_size
    hits <- sum(sim_size == target_size)
    p_size <- hits / n_sims
    p_size <- max(p_size, 1/n_sims) # safety floor
    
    #p_timing
    if (is.na(target_timing)) {
      p_time <- NA
    } else {
      print("F")
      hits_time <- sum(abs(sim_timing - target_timing) <= time_tol, na.rm=TRUE)
      p_time <- max(hits_time / n_sims, 1/n_sims)
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


social_private_model <- function(data_clean, initator_responder, params, coefs, n_sims, fixed, n_time) {
  
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
  
  #range <- as.numeric(fixed['range'])
  #private_threshold <- as.numeric(params['private_threshold'])
  social_threshold <- as.numeric(params['social_threshold'])
  ball_decay_time_coef <- as.numeric(params['ball_decay_time_coef'])
  social_decay_time_coef <- as.numeric(params['social_decay_time_coef'])
  tr <- as.numeric(fixed['tr']) #30
  tb <- 50
  tr <- 30
  tm <- as.numeric(fixed['tm'])
  tm <- 200
  fractional_contagion_first <- as.logical(fixed['fractional_contagion_first'])
  fractional_contagion_subs <- as.logical(fixed['fractional_contagion_subs'])
  max_rate <- as.numeric(fixed['max_rate'])
  dt <- as.numeric(fixed['dt'])
  da <- as.numeric(fixed['da'])
  
  social_private_frame_recorder_list <- vector(mode="list", length = length(unique(data$drop_ID)))
  names(social_private_frame_recorder_list) <- unique(data$drop_ID)
  
  for (i in unique(data$drop_ID)) {
    
    social_private_frame_recorder_list[[as.character(i)]] <- vector(mode = "list", length = n_sims)
    
    #Calculate which individuals are emerged 
    drop_data <- drop_data_list[[as.character(i)]]
    if (is.null(drop_data) || nrow(drop_data) == 0) next
    
    drop_eel_IDs <- unique(drop_data$colony_eel_ID)
    
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
      
      colony_idx <- which(unique(data$colony) == first(drop_data$colony))
      
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
        l_drop_ID <- first(drop_data$drop_ID)
        l_colony_eel_ID <- drop_data$colony_eel_ID[h]
        l_date <- first(drop_data$date)
        l_colony <- first(drop_data$colony)
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
                  eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[4])*log_inst_topo_dist_sc + as.numeric(coefs[5])*(drop_data$log_distance_to_ball_sc[jj]) - social_decay_time_coef*frames_since_infected - ball_decay_time_coef*k
                } else {
                  eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[4])*log_inst_topo_dist_sc - social_decay_time_coef*frames_since_infected
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




