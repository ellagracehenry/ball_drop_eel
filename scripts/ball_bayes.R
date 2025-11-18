#load in packages
library(readxl)
library(ggplot2)
library(dplyr)
library(lme4)
library(tidyr)
library(purrr)
library(rethinking)
theme_set(theme_bw())

setwd("~/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/Shared drives/Field Research Videos/Gil Lab/Curacao_2024/garden_eels/position_drop_experiment")

#load in data
data <- read_excel("master_ball_drop_3D.xlsx") %>%
  filter(!drop_ID %in% c(131))

##Computed variables
#1. distance to ball
data$distance_to_ball <- sqrt((data$base_X - data$ball_hit_X)^2 + (data$base_Y - data$ball_hit_Y)^2 + (data$base_Z - data$ball_hit_Z)^2)

#2. binary responses
data$binary_response <- ifelse(data$full_partial_none == 2, 1, 0)
data <- data %>%
  mutate(binary_response = case_when(
    full_partial_none == 2 ~ 1,
    full_partial_none == 1 ~ 1,
    full_partial_none == 0 ~ 0,
    TRUE ~ NA_real_
  ))

#3. inst_emerged
data <- data %>%
  group_by(drop_ID) %>%
  mutate(inst_emerged = sum(!is.na(full_partial_none)), colony_size = n()) %>%
  ungroup()

data |>
  ggplot(aes(x = distance_to_ball, y = binary_response, col = colony))+
  geom_jitter(height = 0.05, width = 0.05, alpha = 0.5) 


#4. How many eels hidden previously when responded
prev_eel_data <- data %>%
  group_by(drop_ID) %>%
  filter(full_partial_none != 0) %>%
  filter(is.na(full_partial_none) == FALSE) %>%
  mutate(prev_hides = sapply(response_frame_cam1, function(x)
    sum(response_frame_cam1 < x))) 
#Join the prev hides in
data <- data %>%
  left_join(prev_eel_data[,c("colony","eel_ID","drop_ID","prev_hides")], by = c("colony", "eel_ID", "drop_ID"))

#5. get max_flights
data <- data %>%
  group_by(drop_ID) %>%
  mutate(max_flights = sum(binary_response, na.rm =TRUE))

data <- data %>%
  group_by(drop_ID) %>%
  mutate(
    prev_hides = ifelse(
      binary_response ==0,
      max_flights,
      prev_hides
    ) 
  )%>%
  ungroup()

#Training
mean_dist_to_ball <- mean(data$distance_to_ball, na.rm = TRUE)
SD_dist_to_ball <- sd(data$distance_to_ball, na.rm = TRUE)
mean_inst_emerged <- mean(data$inst_emerged, na.rm = TRUE)
SD_inst_emerged <- sd(data$inst_emerged, na.rm = TRUE)
mean_prev_hides <- mean(data$prev_hides, na.rm = TRUE)
SD_prev_hides <- sd(data$prev_hides, na.rm = TRUE)

#Design matrix
d <- data |> 
  mutate(intercept = rep(0, n()),
         colony = ifelse(colony == "S5", 0, ifelse(colony == "S9", 1, ifelse(colony == "S15",2, NA))),
         dist_to_ball = (distance_to_ball - mean_dist_to_ball)/SD_dist_to_ball,
         inst_emerged = (inst_emerged - mean_inst_emerged)/SD_inst_emerged,
         prev_hides = (prev_hides - mean_prev_hides)/SD_prev_hides,
         distance_to_ball_X_inst_emerged = inst_emerged * distance_to_ball) |>
  select(binary_response, intercept, colony, dist_to_ball, inst_emerged, prev_hides, distance_to_ball_X_inst_emerged) |>
  filter(!is.na(binary_response)) %>%
  filter(!is.na(dist_to_ball))

bysfitInstxD <- ulam(
  alist(
    binary_response ~ dbern(p),
    logit(p) <- beta_0*intercept+
                     beta_1*colony+
                     beta_2*inst_emerged+
                     beta_3*prev_hides+
                     beta_4*distance_to_ball_X_inst_emerged,
    beta_0 ~ dnorm(0,10),
    beta_1 ~ dnorm(0,10),
    beta_2 ~ dnorm(0,10),
    beta_3 ~ dnorm(0,10),
    beta_4 ~ dnorm(0,10)
  ),
  data=d
)

bysfitHxL <- ulam(bysfitInstxD, chains=4, cores=4, warmup=1000, iter=10000)

precis(bysfitHxL, prob=0.95, digits=4)

samples <- extract.samples(bysfitHxL)
class(samples) 
str(samples)
names(samples)

samplesdf <- data.frame(samples)
head(samplesdf)

samplesdf |>
  pivot_longer(cols=everything(), names_to="parameter", values_to="sample_value") |>
  ggplot() +
  geom_histogram(aes(x=sample_value, y=after_stat(density)), bins = 75) +
  facet_wrap(vars(parameter), scales = "free")

traceplot(bysfitHxL)

HPDI(samples$beta_0, prob=0.95)
HPDI(samples$beta_1, prob=0.95)
HPDI(samples$beta_2, prob=0.95)
HPDI(samples$beta_3, prob=0.95)
HPDI(samples$beta_4, prob=0.95)

loo(bysfitHxL)
