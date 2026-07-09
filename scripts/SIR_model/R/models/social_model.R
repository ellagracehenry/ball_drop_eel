
social_model <- function(data, params, coefs, n_sims) {
  
  range <- as.numeric(params['range'])
  ball_decay_time_coef <- as.numeric(params['ball_decay_time_coef'])
  social_decay_time_coef <- as.numeric(params['social_decay_time_coef'])
  private_threshold <- as.numeric(params['private_threshold'])
  social_threshold <- as.numeric(params['social_threshold'])
  tr <- as.numeric(params['tr'])
  tm <- as.numeric(params['tm'])
  fractional_contagion_first <- as.character(params['fractional_contagion_first'])
  fractional_contagion_subs <- as.character(params['fractional_contagion_subs'])
  
  social_frame_recorder_list <- vector(mode="list", length = length(unique(data$drop_ID)))
  names(social_frame_recorder_list) <- unique(data$drop_ID)
  
  for (i in unique(data$drop_ID)) {
    
    print(i)
    
    social_frame_recorder_list[[i]] <- vector(mode = "list", length = n_sims)
    
    #Calculate which individuals are emerged 
    drop_data <- data %>%
      filter(drop_ID == i & emerged == 1 & !is.na(global_X)) # & !is.na(dist_from_first_resp)
    
    drop_eel_IDs <- unique(drop_data$colony_eel_ID)
    
    if (fractional_contagion_first == TRUE) {
      K_first <- length(drop_eel_IDs)
    } else {
      K_first <- 1
    }
    
    if (length(drop_eel_IDs) < 3) next
    
    for (sim in 1:n_sims) {
      
      #create a frame recorder matrix
      social_frame_recorder_matrix <- matrix(nrow=length(drop_eel_IDs), dimnames=list(drop_eel_IDs, NULL))
      
      resp_data <- as.data.frame(matrix(nrow=length(drop_eel_IDs),ncol=3))
      
      colony_idx <- which(unique(data$colony) == first(drop_data$colony))
      
      #time step 1
      k <- 1
      
      #determine first responder
      for (h in 1:length(drop_eel_IDs)) {
        l_drop_ID <- first(drop_data$drop_ID)
        l_colony_eel_ID <- drop_data$colony_eel_ID[h]
        l_date <- first(drop_data$date)
        l_colony <- first(drop_data$colony)
        #for each eel i in drop j nested in colony k, compute the linear predictor
        eta_j <- as.numeric(coefs[1]) + as.numeric(coefs[2])*(drop_data$log_distance_to_ball[h]) - ball_decay_time_coef*log(k) #RE removed for now... + fr_re_drop_ID$"(Intercept)"[fr_re_drop_ID$combo == l_drop_ID] + fr_re_colony_colony_eel_ID$"(Intercept)"[as.character(fr_re_colony_colony_eel_ID$name) == l_colony_eel_ID] + fr_re_date$"(Intercept)"[fr_re_date$combo == l_date] + fr_re_colony$"(Intercept)"[fr_re_colony$combo == l_colony]
        #convert this to a standard logistic transform - gives probability per eel
        p_private_cue <- 1/(1+exp(-eta_j))
        resp_data[h,1] <- l_colony_eel_ID
        resp_data[h,2] <- p_private_cue
        resp_data[h,3] <- rbinom(n = 1, size = 1, prob = p_private_cue)
        resp_data[h,4] <- resp_data[h,3]/K_first
        resp_data[h,5] <- ifelse(resp_data[h,4] > private_threshold, 1, 0) #private threshold be on scale between 0 and 1
        
      }
      
      #if there is a first responder
      if (sum(resp_data[,5], na.rm = TRUE) > 0) {
        
        #find IDs of first responder
        fr_ID <- resp_data$V1[resp_data$V5 == 1]
        
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
        
        #for each time step 
        for (k in 2:30) {
          if (fractional_contagion_subs == TRUE) {
            K <- sum(state_matrix[,k-1] == "s")
          } else {
            K <- 1
          }
          for (j in 1:length(drop_eel_IDs)) {
            focal_eel_ID <- drop_eel_IDs[j]
            
            #Assigning states
            if (state_matrix[j,k-1] == "r") { #if eel is recovered
              state_matrix[j,k] <- "r"
              dosage_matrix[j,k] <- NA
            }
            else if (state_matrix[j,k-1] == "i") { #if eel is infected
              dosage_matrix[j,k] <- NA #state and frame recorder matrices stay the same
              frames_since_infected <- k - social_frame_recorder_matrix[j]
              if (k-tr <= 0) {
                state_matrix[j,k] <- "i"
                
                #dose everyone
                for (jj in 1:length(drop_eel_IDs)) {
                  buddy_eel_ID <- drop_eel_IDs[jj]
                  
                  if (buddy_eel_ID == focal_eel_ID) next  # skip self
                  
                  buddy_neighbours_ranked <- drop_data$inst_neighbours_topo_ranked[which(drop_data$colony_eel_ID == buddy_eel_ID)]
                  rank <- which(buddy_neighbours_ranked[[1]] == focal_eel_ID)
                  eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[4])*log(rank) - social_decay_time_coef*log(frames_since_infected)
                  p_cue <- 1/(1+exp(-eta_j))
                  
                  if (rbinom(1,1,p_cue*max_rate*dt) == 1) {
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
                    
                    if (buddy_eel_ID == focal_eel_ID) next  # skip self
                    
                    buddy_neighbours_ranked <- drop_data$inst_neighbours_topo_ranked[which(drop_data$colony_eel_ID == buddy_eel_ID)]
                    rank <- which(buddy_neighbours_ranked[[1]] == focal_eel_ID)
                    eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[4])*log(rank) - social_decay_time_coef*log(frames_since_infected)
                    p_cue <- 1/(1+exp(-eta_j))
                    
                    if (rbinom(1,1,p_cue*max_rate*dt) == 1) {
                      dosage_matrix[jj,k] <- dosage_matrix[jj,k] + da
                    } else {
                      
                    }
                  }
                }
              }
            } else { #eel is susceptible to hide
              
              #Check if responds to social cues
              cuml_dose <- sum(dosage_matrix[j, 1:(k-1)], na.rm=TRUE)
              norm_cuml_dose <- cuml_dose/K
              
              social_response <- ifelse(norm_cuml_dose > social_threshold, 1, 0)
              
              if (social_response == 1) {
                state_matrix[j,k] <- "i"
                social_frame_recorder_matrix[j] <- k
                
                #Dosing only starts next timestep
                
              } else {
                state_matrix[j,k] <- "s"
              }
            }
          }
        }
      }
      social_frame_recorder_list[[i]][[sim]] <- social_frame_recorder_matrix
    }
  }
  
  return(social_frame_recorder_list)
  
}



