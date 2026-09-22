library(ggplot2)
library(dplyr)

nll <- read.csv("/Users/ellag/Desktop/PhD/academic_projects/ball_drop_eel/data/v4/modS_v4.csv", header=FALSE)
params <- read.csv("/Users/ellag/Desktop/PhD/academic_projects/ball_drop_eel/data/v4/social_private_threshold_sparse.csv")

params <- params[!(params$X %in% c(142,156)),]

nll <- nll[order(nll$V1),,drop=FALSE]

full <- cbind(params,nll)

full <- left_join(params, nll, by = c("X" = "V1"))

plot(full$social_decay_time_coef, full$nll)

#Visualising the three params 
ggplot(full, aes(y = V2, x = social_threshold))+ geom_point(size = 3)+facet_grid(social_decay_time_coef ~ ball_decay_time_coef, scales = "free_x",space = "free",axis.labels = "all")+labs(x = "Social threshold", y= "Nll")

ggplot(full, aes(y = V2, x = social_threshold)) + 
  geom_point(size = 3)+
  facet_grid(~private_threshold, scales = "free_x",space = "free")+
  labs(x = "Social threshold", y= "Nll")

#ball decay threshold be less than 2
#social decay threshold should be less than 2 
#social threshold is between 0 and 10

noPrivate_Full <- full
fullfull <- full
noSocial_Full <- full