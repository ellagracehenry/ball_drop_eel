

cascade_size_nll <- function(par, model, data_clean, initator_responder, coefs, n_sims, fixed, n_time) {
  
  model_result <- model(data_clean, initator_responder, par, coefs, n_sims, fixed, n_time)
  
  #Calculate log probabilities of cascade size
  experimental_cascade_size <- data_clean %>%
    group_by(drop_ID) %>%
    summarise(n_responders = first(n_responders))
  
  #for social private
  
  
  loglik <- 0
  
  for (ii in experimental_cascade_size$drop_ID) {
    matches <- NULL
    
    
    # Pull the target responder count once before the loop
    target_responders <- experimental_cascade_size$n_responders[experimental_cascade_size$drop_ID == ii]
    
    if (length(target_responders) == 0 || is.na(target_responders)) {
      next # Skip or set safe default if drop_ID was missing from summary
    }
    
    # Use sapply instead of an explicit loop to calculate matches in one go
    #Extract the cascade size in the simulations
    #sim_sums <- sapply(model_result[[ii]], function(sim_res) sum(sim_res, na.rm = TRUE))
    #How many times do we get cascade size that equals the target
    #matches <- as.integer(sim_sums == target_responders)
    
    #p_hat <- mean(matches)
    
    #p_hat <- max(p_hat, 1/n_sims)
    
    #loglik <- loglik + log(p_hat)
    
    # PROBABILITY DENSITY
    # Instead of strict binary matches, evaluate the probability density of the target
    #sim_sums <- sapply(model_result[[ii]], function(sim_res) sum(!is.na(sim_res)))
    
    # Use a normal distribution approximation for the probability of landing near target
    # This creates a smooth gradient the optimizer can actually follow!
    #mean_sim <- mean(sim_sums, na.rm = TRUE)
   # sd_sim   <- sd(sim_sums, na.rm = TRUE)
    #if (is.na(sd_sim) || sd_sim == 0) sd_sim <- 0.1 # Safety bound
    
    # Calculate likelihood using a normal density evaluated at the target
   # p_hat <- dnorm(target_responders, mean = mean_sim, sd = sd_sim)
   # p_hat <- max(p_hat, 1e-5) # Soft lower bound
    
    #loglik <- loglik + log(p_hat)
    
    # # EXACT ML CALC
    sim_sums <- sapply(model_result[[as.character(ii)]], function(sim_res) sum(!is.na(sim_res)))
    hits <- sum(sim_sums == target_responders)
    p_hat <- hits / n_sims
    p_hat <- max(p_hat, 1/n_sims) # safety floor
    loglik <- loglik + log(p_hat)
    
    
  }
  cat("NLL:", -loglik, "\n")
  flush.console()
  
  return(list(
    nll = -loglik,
    model_result = model_result))
  
}
