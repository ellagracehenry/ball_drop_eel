generate_initator_responder_data <- function(data) {
  #Fit a model for initator responder pairs
  initator_responder <- data
  
  #Ensure is not a first responder
  initator_responder <- initator_responder[
    complete.cases(initator_responder[,c(
      "second_responder",
      "dist_from_first",
      "trial_ID",
      "distance_to_ball",
      "global_topo_dist_from_first",
      "log_global_topo_dist_from_first",
      "log_distance_to_ball"
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
  #nonsocial
  initator_responder$ball_sc        <- scale(initator_responder$distance_to_ball)
  initator_responder$log_ball_sc    <- scale(initator_responder$log_distance_to_ball)
  #social
  initator_responder$metric_dist_sc        <- scale(initator_responder$dist_from_first) #metric
  initator_responder$log_metric_dist_sc    <- scale(initator_responder$log_dist_from_first) #log metric
  initator_responder$global_topo_dist_sc        <- scale(initator_responder$global_topo_dist_from_first) #global topo
  initator_responder$log_global_topo_dist_sc        <- scale(initator_responder$log_global_topo_dist_from_first) #log global topo
  initator_responder$inst_topo_dist_sc        <- scale(initator_responder$inst_topo_dist_from_first) #inst topo
  initator_responder$log_inst_topo_dist_sc        <- scale(initator_responder$log_inst_topo_dist_from_first) #log inst topo
  
  initator_responder$ball_sc <- as.numeric(initator_responder$ball_sc)
  
  initator_responder$first_in_global_voronoi <- as.numeric(initator_responder$first_in_global_voronoi)
  initator_responder$first_in_inst_voronoi <- as.numeric(initator_responder$first_in_inst_voronoi)
  
  return(initator_responder)
  
}