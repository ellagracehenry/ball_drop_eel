# Lambda scan
lambda      <- 10^seq(-4, 2, length = 10)
devianz_vec <- rep(Inf, length(lambda))
coeff_ma    <- NULL

binom_deviance <- function(y, y_hat) {
  y_hat <- pmin(pmax(y_hat, 1e-6), 1 - 1e-6)
  -2 * sum(y * log(y_hat) + (1 - y) * log(1 - y_hat))
}

for (j in 1:length(lambda)) {
  glm1 <- try(
    glmmLasso(
      fix = second_responder ~ inst_emerged_sc + colony_size_sc +
        log_dist_sc + dist_sc + log_ball_sc,
      rnd = list(re_colony_drop_ID = ~1, colony_eel_ID = ~1),
      family = binomial(),
      data = initator_responder,
      lambda = lambda[j],
      switch.NR = TRUE,
      final.re = TRUE
    ),
    silent = TRUE
  )
  
  if (!inherits(glm1, "try-error") & !is.null(glm1$coefficients)) {
    y_hat          <- predict(glm1, type = "response")
    devianz_vec[j] <- binom_deviance(initator_responder$second_responder, y_hat)
    coeff_ma       <- cbind(coeff_ma, glm1$coefficients)
    cat("lambda:", round(lambda[j], 5),
        "| dev:", round(devianz_vec[j], 2),
        "| coefs:", round(glm1$coefficients[-1], 3), "\n")
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

final_model <- glmmLasso(
  fix = second_responder ~ inst_emerged_sc + colony_size_sc +
    dist_sc + log_ball_sc + log_dist_sc,
  rnd = list(re_colony_drop_ID = ~1, colony_eel_ID = ~1),
  family = binomial(),
  data = initator_responder,
  lambda = final_lambda,
  switch.NR = TRUE,
  final.re = TRUE
)

# Extract and rank nonzero coefficients from final model
coefs <- final_model$coefficients[-1]  # drop intercept
names(coefs) <- coeff_names

# Show ranked by absolute magnitude
coefs_df <- data.frame(
  predictor  = names(coefs),
  coefficient = round(coefs, 4)
)
coefs_df <- coefs_df[order(abs(coefs_df$coefficient), decreasing = TRUE), ]
print(coefs_df)

#Model is robust across a wide range of penalty strengths, selected predictors are stable features

# Coefficient path plot
coeff_names <- c("inst_emerged_sc", "colony_size_sc", "dist_sc", 
                 "ball_sc", "log_dist_sc", "log_ball_sc")

# coeff_ma rows: intercept + 6 predictors, cols: lambdas
# remove intercept row
coeff_plot <- coeff_ma[-1, ]
rownames(coeff_plot) <- coeff_names

# only plot lambdas where model converged (cols of coeff_ma)
lambda_converged <- lambda[1:ncol(coeff_ma)]

matplot(log10(lambda_converged), t(coeff_plot),
        type = "l", lty = 1, lwd = 2,
        col = 1:6,
        xlab = "log10(lambda)", 
        ylab = "Coefficient",
        main = "L1 Coefficient Paths")
abline(h = 0, lty = 2, col = "grey50")
abline(v = log10(final_lambda), lty = 2, col = "red")
legend("topright", 
       legend = coeff_names,
       col = 1:6, lty = 1, lwd = 2, cex = 0.4)
