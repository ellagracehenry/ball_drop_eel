

cascade_size_nll <- function(par, model, data_clean, coefs, n_sims, fixed) {
  
  model_result <- model(data_clean, par, coefs, n_sims, fixed)
  
  loglik <- 0 
  
  #Calculate log probabilities of cascade size
  experimental_cascade_size <- data_clean %>%
    group_by(drop_ID) %>%
    summarise(n_responders = first(n_responders))
  
  #for social private
  
  
  loglik <- 0
  
  for (ii in experimental_cascade_size$drop_ID) {
    matches <- NULL
    
    
    for (iii in 1:length(model_result[[ii]])) {
      
      matches[iii] <- ifelse(sum(model_result[[ii]][[iii]], na.rm=TRUE) == experimental_cascade_size$n_responders[experimental_cascade_size$drop_ID == ii], 1, 0)
      
    }
    
    p_hat <- mean(matches)
    
    p_hat <- max(p_hat, 1/n_sims)
    
    loglik <- loglik + log(p_hat)
    
    
  }
  print(-loglik)
  return(-loglik)
  
}
