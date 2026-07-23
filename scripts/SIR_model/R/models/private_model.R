
private_model <- function(data_clean, initator_responder, params, coefs, n_sims, fixed, n_time) {
  
  data <- data_clean
  n_sims <- n_sims
  coefs <- coefs
  n_time <- n_time
  
  drop_data_groups <- data_clean %>% 
    filter(emerged == 1, !is.na(global_X))
  
  drop_data_list <- data_clean %>% 
    filter(emerged == 1, !is.na(global_X)) %>% 
    split(.$drop_ID) # Natively names the list items by drop_ID
  
  orig_topo_mean <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:center")
  orig_topo_sd   <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:scale")
  
  #range <- as.numeric(fixed['range'])
  ball_decay_time_coef <- as.numeric(params['ball_decay_time_coef'])
  private_threshold <- as.numeric(params['private_threshold'])
  tr <- as.numeric(fixed['tr'])
  tm <- as.numeric(fixed['tm'])
  max_rate <- as.numeric(fixed['max_rate'])
  dt <- as.numeric(fixed['dt'])
  da <- as.numeric(fixed['da'])
  
  social_private_frame_recorder_list <- vector(mode="list", length = length(unique(data$drop_ID)))
  names(social_private_frame_recorder_list) <- unique(data$drop_ID)
  
  for (i in unique(data$drop_ID)) {
    
    print(i)
    private_frame_recorder_list[[i]] <- vector(mode = "list", length = n_sims)
    
    #Calculate which individuals are emerged 
    drop_data <- drop_data_list[[as.character(i)]]
    
    drop_eel_IDs <- unique(drop_data$colony_eel_ID)
    
    #if (is.na(sum(drop_data$distance_to_ball, na.rm=TRUE))) next
    
    if (length(drop_eel_IDs) < 3) next
    
    for (sim in 1:n_sims) {
      
      #print("starting sim")
      #print(sim)
      #create a frame recorder matrix
      private_frame_recorder_matrix <- matrix(nrow=length(drop_eel_IDs), dimnames=list(drop_eel_IDs, NULL))
      
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
        eta_j <- as.numeric(coefs[1]) + as.numeric(coefs[2])*(drop_data$log_distance_to_ball_sc[h]) - ball_decay_time_coef*log(k) #RE removed for now... + fr_re_drop_ID$"(Intercept)"[fr_re_drop_ID$combo == l_drop_ID] + fr_re_colony_colony_eel_ID$"(Intercept)"[as.character(fr_re_colony_colony_eel_ID$name) == l_colony_eel_ID] + fr_re_date$"(Intercept)"[fr_re_date$combo == l_date] + fr_re_colony$"(Intercept)"[fr_re_colony$combo == l_colony]
        #convert this to a standard logistic transform - gives probability per eel
        p_private_cue <- 1/(1+exp(-eta_j))
        resp_data[h,1] <- l_colony_eel_ID
        resp_data[h,2] <- p_private_cue
        resp_data[h,3] <- rbinom(n = 1, size = 1, prob = p_private_cue)
        resp_data[h,4] <- resp_data[h,3]
        resp_data[h,5] <- ifelse(resp_data[h,4] > private_threshold, 1, 0) #private threshold be on scale between 0 and 1
      }
      
      #create state matrix 
      state_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = n_time)
      state_matrix[,1] <- "s"
      #create dosage matrix 
      dosage_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = n_time)
      dosage_matrix[,] <- 0
      
      #if there is a first responder
      if (sum(resp_data[,5], na.rm = TRUE) > 0) {
        
        #find IDs of first responder
        fr_ID <- resp_data$V1[resp_data$V5 == 1]
        
        #find index of first responder
        fr_idx <- which(drop_eel_IDs %in% fr_ID)
        
        private_frame_recorder_matrix[fr_idx] <- 1
        
        for (fr_IDD in fr_idx) {
          state_matrix[fr_IDD,1] <- "i"
        }
        
        state_matrix[-fr_idx,1] <- "s"
        
        dosage_matrix[fr_idx,] <- NA
        dosage_matrix[-fr_idx,] <- 0
        
      }
      
      #for each time step 
      for (k in 2:n_time) {
        for (j in 1:length(drop_eel_IDs)) {
          focal_eel_ID <- drop_eel_IDs[j]
          #Assigning states
          if (state_matrix[j,k-1] == "r") { #if eel is recovered
            state_matrix[j,k] <- "r"
          } else if (state_matrix[j,k-1] == "i") { #if eel is infected
            frames_since_infected <- k - private_frame_recorder_matrix[j]
            if (k-tr <= 0) {
              state_matrix[j,k] <- "i"
            } else { 
              if (state_matrix[j,k-tr] == "i") {
                state_matrix[j,k] <- "r"
              } else {
                state_matrix[j,k] <- "i"
              }
            }
          } else { #eel is susceptible to hide
            
            if (K == 0) {
              private_response <- 0
            } else {
              #Check if responds to private cue of the ball, just delayed
              eta_j <-  as.numeric(coefs[1]) + as.numeric(coefs[2])*(drop_data$log_distance_to_ball_sc[drop_data$colony_eel_ID == focal_eel_ID]) - ball_decay_time_coef*log(k) #Interecept (let's just fit it with the threshold) and RE removed for now... + fr_re_drop_ID$"(Intercept)"[fr_re_drop_ID$combo == l_drop_ID] + fr_re_colony_colony_eel_ID$"(Intercept)"[as.character(fr_re_colony_colony_eel_ID$name) == l_colony_eel_ID] + fr_re_date$"(Intercept)"[fr_re_date$combo == l_date] + fr_re_colony$"(Intercept)"[fr_re_colony$combo == l_colony]
              #convert this to a standard logistic transform - gives probability per eel
              p_private_cue <- 1/(1+exp(-eta_j))
              private_cue_received <- rbinom(n = 1, size = 1, prob = p_private_cue) 
              private_response <- ifelse(private_cue_received > private_threshold, 1, 0) #private threshold be on scale between 0 and 1

            }
            if (!is.na(private_response)) {
              if (private_response == 1) {
                state_matrix[j,k] <- "i"
                private_frame_recorder_matrix[j] <- k
              } else {
                state_matrix[j,k] <- "s"
              }
            } else {
              state_matrix[j,k] <- "s"
            }
          }
        }
      }
      private_frame_recorder_list[[i]][[sim]] <- private_frame_recorder_matrix
    }
  }
  
  return(private_frame_recorder_list)
  
}

