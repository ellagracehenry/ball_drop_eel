glmm_lasso_fs <- function(initator_responder) {
  
  # testing random effects structure
  intercepts_random_model <- glmer(second_responder ~ 1 + (1 | colony/eel_ID) + (1|drop_ID) + (1|date), family = binomial, data = initator_responder)
  summary(intercepts_random_model)
  
  #Intercept-only glmmLasso has a bug, needs one predictor
  #Simple model
  start_model <- glmmLasso(
    fix    = second_responder ~ log_ball_sc + ball_sc + metric_dist_sc + log_metric_dist_sc + global_topo_dist_sc + log_global_topo_dist_sc + inst_topo_dist_sc + log_inst_topo_dist_sc + first_in_global_voronoi + first_in_inst_voronoi,
    rnd    = list(re_colony_eel_ID = ~1, colony = ~1, drop_ID = ~1, date = ~1),
    family = binomial(),
    data   = initator_responder,
    lambda = 100
  )
  summary(start_model)
  
  #Extract starting values
  Delta.start <- start_model$Deltamatrix[start_model$conv.step, ]
  Q.start     <- start_model$Q_long[[start_model$conv.step + 1]]
  
  
  ####
  lambda      <- 10^seq(2, -6, length = 50)
  devianz_vec <- rep(Inf, length(lambda))
  coeff_ma    <- NULL
  
  binom_deviance <- function(y, y_hat) {
    y_hat <- pmin(pmax(y_hat, 1e-6), 1 - 1e-6)
    -2 * sum(y * log(y_hat) + (1 - y) * log(1 - y_hat))
  }
  
  for (j in 1:length(lambda)) {
    
    glm1 <- try(glmmLasso(
      fix = second_responder ~ log_ball_sc + ball_sc + metric_dist_sc + log_metric_dist_sc + global_topo_dist_sc + log_global_topo_dist_sc + inst_topo_dist_sc + log_inst_topo_dist_sc + first_in_global_voronoi + first_in_inst_voronoi,  # full model formula
      rnd = list(re_colony_eel_ID = ~1, colony = ~1, drop_ID = ~1, date = ~1),
      family = binomial(),
      data = initator_responder,
      lambda = lambda[j],
      control = list(start   = Delta.start,
                     q_start = Q.start)
    )
    )
    
    if (!inherits(glm1, "try-error") & !is.null(glm1$coefficients)) {
      y_hat          <- predict(glm1, type = "response")
      devianz_vec[j] <- binom_deviance(initator_responder$second_responder, y_hat)
      coeff_ma       <- cbind(coeff_ma, glm1$coefficients)
      #cat("lambda:", round(lambda[j], 5),
      #    "| dev:", round(devianz_vec[j], 2),
      #    "| coefs:", round(glm1$coefficients[-1], 3), "\n")
      
      # Warm-start: carry this fit into the next lambda
      Delta.start <- glm1$Deltamatrix[glm1$conv.step, ]
      Q.start     <- glm1$Q_long[[glm1$conv.step + 1]]
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
  
  #FITTED LAMBDA
  lambda_min = 0.1676833 
  
  #Fit final model to the lambda with the lowest deviance to find the top-ranked predictors
  models_fine  <- glmmLasso(
    fix = second_responder ~ log_ball_sc + ball_sc + metric_dist_sc + log_metric_dist_sc + global_topo_dist_sc + log_global_topo_dist_sc + inst_topo_dist_sc + log_inst_topo_dist_sc + first_in_global_voronoi + first_in_inst_voronoi,  # full model formula
    rnd = list(re_colony_eel_ID = ~1, colony = ~1, drop_ID = ~1, date = ~1),
    family = binomial(),
    data = initator_responder,
    lambda = final_lambda
  )
  
  
  summary(models_fine)
  
  #Cross validated lamda
  cv_glmmLasso <- function(data, lambda_vec, k_folds = 10, seed = 42) {
    
    set.seed(seed)
    
    # Create folds (by drop_ID to avoid data leakage within a drop)
    drop_ids  <- unique(data$drop_ID)
    fold_assignment <- sample(rep(1:k_folds, length.out = length(drop_ids)))
    fold_map  <- data.frame(drop_ID = drop_ids, fold = fold_assignment)
    data      <- left_join(data, fold_map, by = "drop_ID")
    
    cv_deviance <- rep(0, length(lambda_vec))
    
    for (j in seq_along(lambda_vec)) {
      fold_deviances <- rep(NA, k_folds)
      
      for (f in 1:k_folds) {
        train <- data[data$fold != f, ]
        test  <- data[data$fold == f, ]
        
        # Refit start model on training fold
        start_mod <- try(glmmLasso(
          fix = second_responder ~ log_ball_sc + ball_sc + 
            log_metric_dist_sc + metric_dist_sc + log_global_topo_dist_sc + global_topo_dist_sc +
            log_inst_topo_dist_sc + inst_topo_dist_sc +
            first_in_global_voronoi + first_in_inst_voronoi,
          rnd    = list(re_colony_eel_ID = ~1, colony = ~1, drop_ID = ~1, date = ~1),
          family = binomial(),
          data   = train,
          lambda = lambda_vec[j]
        ), silent = TRUE)
        
        if (inherits(start_mod, "try-error")) next
        
        # Predict on test fold (fixed effects only, RE = 0 for unseen groups)
        X_test <- model.matrix(
          ~ log_ball_sc + ball_sc + 
            log_metric_dist_sc + metric_dist_sc + log_global_topo_dist_sc + global_topo_dist_sc +
            log_inst_topo_dist_sc + inst_topo_dist_sc + first_in_global_voronoi + first_in_inst_voronoi,
          data = test
        )
        
        eta      <- X_test %*% start_mod$coefficients
        y_hat    <- plogis(eta)
        y_hat    <- pmin(pmax(y_hat, 1e-6), 1 - 1e-6)
        y        <- test$second_responder
        
        fold_deviances[f] <- -2 * sum(y * log(y_hat) + (1 - y) * log(1 - y_hat), 
                                      na.rm = TRUE)
      }
      
      cv_deviance[j] <- mean(fold_deviances, na.rm = TRUE)
      cat("lambda:", round(lambda_vec[j], 6), 
          "| CV deviance:", round(cv_deviance[j], 3), "\n")
    }
    
    best_lambda <- lambda_vec[which.min(cv_deviance)]
    
    plot(log10(lambda_vec), cv_deviance, type = "b", pch = 19,
         xlab = "log10(lambda)", ylab = "Mean CV deviance",
         main = "Cross-validated lambda selection")
    abline(v = log10(best_lambda), lty = 2, col = "red")
    
    list(cv_deviance = cv_deviance, best_lambda = best_lambda)
  }
  
  # Run it - use a coarser grid first as it's slow
  lambda_cv <- 10^seq(2, -3, length = 10)
  cv_result  <- cv_glmmLasso(initator_responder, lambda_cv)
  cat("Best lambda:", cv_result$best_lambda, "\n")
  
  # Refit final model at CV-selected lambda
  final_model_cv <- glmmLasso(
    fix = second_responder ~ log_ball_sc + ball_sc + 
      log_metric_dist_sc + metric_dist_sc + log_global_topo_dist_sc + global_topo_dist_sc +
      log_inst_topo_dist_sc + inst_topo_dist_sc + first_in_global_voronoi + first_in_inst_voronoi,
    rnd    = list(re_colony_eel_ID = ~1, colony = ~1, drop_ID = ~1, date = ~1),
    family = binomial(),
    data   = initator_responder,
    lambda = cv_result$best_lambda
  )
  summary(final_model_cv)
  
  
  ### PREDICTORS ###
  #log_ball_sc <- -0.4016866 (corr with ball_sc)
  #log_inst_topo_disc_sc <- -0.7924365
  
  
  # Check what's actually happening across lambda
  matplot(log10(lambda), t(coeff_ma), type = "l", lty = 1,
          xlab = "log10(lambda)", ylab = "Coefficients",
          main = "glmmLasso coefficient paths")
  abline(v = log10(final_lambda), lty = 2, col = "red")
  legend("topright", rownames(coeff_ma), col = 1:nrow(coeff_ma), lty = 1, cex = 0.7)
  
  cv1(second_responder, initator_responder, lambda1=1,fold=10)
  
  #Collinearity... drop one of the logs or non logs and see what happens
  
  #Fitting logistic regression with chosen features, model coefficients are weights
  final_model <- glmer(
    second_responder ~ log_ball_sc + log_inst_topo_dist_sc +  # your top predictors
      (1 | colony/eel_ID) + (1 | drop_ID) + (1 | date),
    family = binomial(),
    data   = initator_responder
  )
  summary(final_model)
  
  #checking for corr
  d <- initator_responder %>% filter(!is.na(log_inst_topo_dist_sc) & !is.na(log_ball_sc))
  cor(d$log_inst_topo_dist_sc, d$log_ball_sc) #Low, 0.3482386
  
  ##COEFFICIENTS##
  #unscale
  b0 <- as.numeric(fixef(final_model)[1])
  b1 <- as.numeric(fixef(final_model)[2])
  b2 <- as.numeric(fixef(final_model)[3])
  
  sd_log_inst <- sd(initator_responder$log_inst_topo_dist_from_first, na.rm=TRUE)
  sd_log_ball <- sd(initator_responder$log_distance_to_ball, na.rm=TRUE)
  
  mean_log_inst <- mean(initator_responder$log_inst_topo_dist_from_first, na.rm=TRUE)
  mean_log_ball <- mean(initator_responder$log_distance_to_ball, na.rm=TRUE)
  
  b_log_inst_topo <- b1 / sd_log_inst
  b_log_ball <- b2 / sd_log_ball
  
  second_responder_intercept <- b0 -
    b1 * mean_log_inst -
    b2 * mean_log_ball
  
  #################################################################################
  
}