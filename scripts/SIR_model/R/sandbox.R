#plotting different param vals

#decay_time_coef 
ball_decay_time_coef <- seq(0.1,7,0.5)
ball_decay_time_coef<- -1
k <- seq(1,7,1)
eta_j <- as.numeric(coefs[1]) + as.numeric(coefs[2])*1.56 - ball_decay_time_coef*log(k) #RE removed for now... + fr_re_drop_ID$"(Intercept)"[fr_re_drop_ID$combo == l_drop_ID] + fr_re_colony_colony_eel_ID$"(Intercept)"[as.character(fr_re_colony_colony_eel_ID$name) == l_colony_eel_ID] + fr_re_date$"(Intercept)"[fr_re_date$combo == l_date] + fr_re_colony$"(Intercept)"[fr_re_colony$combo == l_colony]
#convert this to a standard logistic transform - gives probability per eel
p_private_cue <- 1/(1+exp(-eta_j))

plot(k, p_private_cue )


#thresholds
