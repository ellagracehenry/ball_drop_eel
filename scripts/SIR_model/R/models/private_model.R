


non_social_frame_recorder_list <- vector(mode="list", length = length(unique(data$drop_ID)))
names(non_social_frame_recorder_list) <- unique(data$drop_ID)

for (i in unique(data$drop_ID)) {
  
  print(i)
  
  non_social_frame_recorder_list[[i]] <- vector(mode = "list", length = n_sims)
  
  #Calculate which individuals are emerged 
  drop_data <- data %>%
    filter(drop_ID == i & emerged == 1 & !is.na(global_X)) 
  
  drop_eel_IDs <- unique(drop_data$colony_eel_ID)
  
  if (length(drop_eel_IDs) < 3) next
  
  for (sim in 1:n_sims) {
    
    #create a frame recorder matrix
    non_social_frame_recorder_matrix <- matrix(nrow=length(drop_eel_IDs), dimnames=list(drop_eel_IDs, NULL))   
    
    resp_data <- as.data.frame(matrix(nrow=length(drop_eel_IDs),ncol=3))
    
    colony_idx <- which(unique(data$colony) == first(drop_data$colony))
    
    #determine first responder
    for (h in 1:length(drop_eel_IDs)) {
      l_drop_ID <- first(drop_data$drop_ID)
      l_colony_eel_ID <- drop_data$colony_eel_ID[h]
      l_date <- first(drop_data$date)
      l_colony <- first(drop_data$colony)
      #for each eel i in drop j nested in colony k, compute the linear predictor
      eta_j <- as.numeric(coefs[1]) + as.numeric(coefs[2])*(drop_data$log_distance_to_ball[h])
      #convert this to a standard logistic transform - gives probability per eel
      p_private_cue <- 1/(1+exp(-eta_j))
      resp_data[h,1] <- l_colony_eel_ID
      resp_data[h,2] <- p_private_cue
      resp_data[h,3] <- rbinom(n = 1, size = 1, prob = p_private_cue)
      resp_data[h,4] <- resp_data[h,3]
      resp_data[h,5] <- ifelse(resp_data[h,4] > private_threshold, 1, 0) #private threshold be on scale between 0 and 1
    }
    
    #if there is a first responder
    if (sum(resp_data[,5], na.rm = TRUE) > 0) {
      
      #find IDs of first responder
      fr_ID <- resp_data$V1[resp_data$V5 == 1]
      
      #find index of first responder
      fr_idx <- which(drop_eel_IDs %in% fr_ID)
      
      non_social_frame_recorder_matrix[fr_idx] <- 1
      
      #create state matrix 
      state_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = 200)
      state_matrix[fr_idx,1] <- "i"
      state_matrix[-fr_idx,1] <- "s"
      
      #for each time step
      for (k in 2:30) {
        for (j in 1:length(drop_eel_IDs)) {
          focal_eel_ID <- drop_eel_IDs[j]
          
          if (state_matrix[j,k-1] == "r") { #if eel is recovered
            state_matrix[j,k] <- "r"
          } else if (state_matrix[j,k-1] == "i") { #if eel is infected
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
            #check if eel responds to private cue of the ball, just delayed
            eta_j <- as.numeric(coefs[1]) + as.numeric(coefs[2])*(drop_data$log_distance_to_ball[drop_data$colony_eel_ID == focal_eel_ID]) - ball_decay_time_coef*log(k)
            p_private_cue <- 1/(1+exp(-eta_j))
            private_cue_received <- rbinom(n = 1, size = 1, prob = p_private_cue) 
            private_response <- ifelse(private_cue_received/K > private_threshold, 1, 0) #private threshold be on scale between 0 and 1
            
            if (private_response == 1) {
              state_matrix[j,k] <- "i"
              non_social_frame_recorder_matrix[j] <- k
            } else {
              state_matrix[j,k] <- "s"
            }
          }
        }
      }
      
      non_social_frame_recorder_list[[i]][[sim]] <- non_social_frame_recorder_matrix
    }
  }
}