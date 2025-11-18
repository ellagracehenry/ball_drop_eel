#load in packages
library(readxl)
library(ggplot2)
library(dplyr)
library(lme4)
library(tidyr)
library(purrr)

 
setwd("~/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/Shared drives/Field Research Videos/Gil Lab/Curacao_2024/garden_eels/position_drop_experiment")

#load in data
data <- read_excel("master_ball_drop_3D.xlsx") %>%
  filter(!drop_ID %in% c(131))

  

##Computed variables
#1. distance to ball
data$distance_to_ball <- sqrt((data$base_X - data$ball_hit_X)^2 + (data$base_Y - data$ball_hit_Y)^2 + (data$base_Z - data$ball_hit_Z)^2)

#2. max response distance to ball
max_dist_df <- data %>%
  filter(full_partial_none != 0) %>%
  group_by(drop_ID) %>%
  summarise(max_dist_resp_to_ball = max(distance_to_ball, na.rm = TRUE), .groups = "drop")
data <- data %>%
  left_join(max_dist_df, by = "drop_ID")

#3. pairwise distances within each colony
coordsave <- data %>%
  group_by(colony, trial_ID, eel_ID) %>%
  summarise(avg_x = mean(base_X, na.rm = TRUE), avg_y = mean(base_Y, na.rm = TRUE), avg_z = mean(base_Z, na.rm = TRUE))

distance_wide <- coordsave %>%
  group_by(trial_ID) %>%
  do({
    coords <- as.matrix(.[, c("avg_x", "avg_y", "avg_z")])
    dist_mat <- as.matrix(dist(coords))
    rownames(dist_mat) <- .$eel_ID
    colnames(dist_mat) <- paste0("dist_to_", .$eel_ID)
    # combine the original data with the distance columns
    bind_cols(., as.data.frame(dist_mat))
  }) %>%
  ungroup()

#Join the avg distances in
data <- data %>%
  left_join(distance_wide[,1:6], by = c("colony", "trial_ID", "eel_ID"))

#4. cascade extent
cascade_summary <- data %>%
  # Work within each drop
  group_by(drop_ID) %>%
  # Keep only responders (both partial and full)
  filter(full_partial_none == 2) %>%
  filter(is.na(full_partial_none) == FALSE) %>%
  # Proceed only if at least one responder
  summarise(
    # Identify the first responder (min response frame)
    first_responder_id = eel_ID[which.min(response_frame_cam1)],
    first_x = avg_x[which.min(response_frame_cam1)],
    first_y = avg_y[which.min(response_frame_cam1)],
    first_z = avg_z[which.min(response_frame_cam1)],
    # Compute distances of all responders to the first responder
    cascade_extent = max(
      sqrt((avg_x - first_x)^2 + (avg_y - first_y)^2 + (avg_z - first_z)^2),
      na.rm = TRUE
    ),
    cascade_size = n()
  )

data <- data %>%
  left_join(cascade_summary, by = c("drop_ID"))

data$cascade_extent <- ifelse(is.na(data$cascade_extent), 0,data$cascade_extent)
data$cascade_size <- ifelse(is.na(data$cascade_size), 0,data$cascade_size)


#5. binary responses
data$binary_response <- ifelse(data$full_partial_none == 2, 1, 0)
data <- data %>%
  mutate(binary_response = case_when(
    full_partial_none == 2 ~ 1,
    full_partial_none == 1 ~ 1,
    full_partial_none == 0 ~ 0,
    TRUE ~ NA_real_
  ))

#6. inst_emerged
data <- data %>%
  group_by(drop_ID) %>%
  mutate(inst_emerged = sum(!is.na(full_partial_none)), colony_size = n()) %>%
  ungroup()

#7. How many eels hidden previously when responded
prev_eel_data <- data %>%
  group_by(drop_ID) %>%
  filter(full_partial_none != 0) %>%
  filter(is.na(full_partial_none) == FALSE) %>%
  mutate(prev_hides = sapply(response_frame_cam1, function(x)
    sum(response_frame_cam1 < x))) 

#Join the prev hides in
data <- data %>%
  left_join(prev_eel_data[,c("colony","eel_ID","drop_ID","prev_hides")], by = c("colony", "eel_ID", "drop_ID"))

#8. get max_flights
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

#9. Calculate inst_inst_emerged
data$inst_inst_emerged <- data$inst_emerged - data$prev_hides

#10. Colony area for density
coordsave <- data %>%
  group_by(colony, trial_ID, eel_ID) %>%
  summarise(avg_x = mean(base_X, na.rm = TRUE), avg_y = mean(base_Y, na.rm = TRUE), avg_z = mean(base_Z, na.rm = TRUE))

coordssave_count <- coordsave %>%
  group_by(colony, trial_ID) %>%
  summarise(non_na_count = sum(!is.na(avg_x)))

coordssave_count_max <- coordssave_count %>%
  group_by(colony) %>%
  summarise(max_eel_trial = max(non_na_count, na.rm =TRUE))

coordsave <- coordsave %>%
 left_join(coordssave_count, by = c("colony","trial_ID"))

#11. Colony size
data$colony_size[data$colony == "S5"] <- 34
data$colony_size[data$colony == "S9"] <- 59
data$colony_size[data$colony == "S15"] <- 67


data$colony <- factor(data$colony, levels = c("S5","S9", "S15"))

#12. Prop inst emergerd
prop_inst_emerged <- data$inst_emerged/data$colony_size

#Response ~ distance to ball
#A - Overall
hist(data$distance_to_ball)

data %>%
  group_by(drop_ID) %>%
  mutate(
    distance_to_ball_grouped = first(na.omit(distance_to_ball)), 
    bin_inst_emerged_grouped = first(na.omit(bin_inst_emerged))) %>%
  ungroup() %>%
  ggplot(aes(x = distance_to_ball, fill = as.factor(bin_inst_emerged_grouped))) +
  geom_histogram(binwidth = 1, alpha = 0.8, position = "identity") +
  labs(
    title = "Distribution of distance to ball",
    x = "Distance to ball",
    y = "Frequency"
  ) +
  theme_minimal()

min(data$distance_to_ball, na.rm = TRUE)

model <- glm(binary_response ~ distance_to_ball, data = data, family = binomial)

glm(binary_response ~ )


data$trial_ID <- as.factor(data$trial_ID)
ggplot(data, aes(x = distance_to_ball, y = binary_response, color = trial_ID)) +
  geom_jitter(height = 0.05, width = 0.05, alpha = 0.5) +  # Show individual points
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(y = "Probability of Hide", x = "Distance to ball") +
  facet_wrap(~colony)+
  theme_minimal() 

hist(data$distance_to_ball)

table(data$binary_response, useNA = "ifany") #how many 1, 0, NA

#B- By number of instaneous emerged individuals. If using social information on fear, would expect them to hide at further distances to the ball than expected when colony size is larger? If using social information safety, would expect them to stay emerged at closer distances to the ball than expected when colony size is larger?
#Histogram number of emerged individuals for each drop (add to each row), split in 3 to low medium high inst neighbour presence, give different colours and plot

hist(data$inst_emerged)

range <- (max(data$inst_emerged)-min(data$inst_emerged))

small <- c(1,14)
low <- c(15,26)
medium <- c(27,38)
high <- c(39,51)

data <- data %>%
  mutate(bin_inst_emerged = case_when(
    inst_emerged >= small[1] & inst_emerged <= small[2] ~ "lowest",
    inst_emerged >= low[1] & inst_emerged <= low[2] ~ "low",
    inst_emerged >= medium[1] & inst_emerged <= medium[2] ~ "medium",
    inst_emerged >= high[1] & inst_emerged <= high[2] ~ "high",
    TRUE ~ NA_character_
  ))

data%>%
  summarise(n_lowest = sum(bin_inst_emerged == "lowest"), n_low = sum(bin_inst_emerged == "low"), n_med = sum(bin_inst_emerged == "medium"), n_high = sum(bin_inst_emerged == "high"))
  
data$bin_inst_emerged <- factor(data$bin_inst_emerged, levels = c("lowest","low", "medium", "high"))

ggplot(data, aes(x = distance_to_ball, y = binary_response, color = bin_inst_emerged, group = bin_inst_emerged)) +
  geom_jitter(height = 0.05, width = 0.05, alpha = 0.5) +  # Show individual points
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(y = "Probability of Hide", x = "Distance to ball", color = "Number of emerged individuals") +
  theme_classic()

sum_data <- data %>%
  group_by(drop_ID) %>%
  summarise(prop_hides = first(max_flights)/first(inst_emerged), colony = first(colony), trial_ID = first(trial_ID), inst_emerged = first(inst_emerged))

data$trial_ID <- as.factor(data$trial_ID)
ggplot(data, aes(x = inst_emerged, y = binary_response), color = colony) +
  geom_jitter(height = 0.02, width = 0.1, alpha = 0.1) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  theme_minimal()

  
ggplot(data, aes(x = inst_emerged, y = binary_response, color = colony)) +
  geom_jitter(height = 0.02, width = 0.1, alpha = 0.1) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  scale_color_manual(values = c("#E63946", "#F1A208", "#6D597A")) +
  theme_minimal()

ggplot(data, aes(x = inst_emerged, y = binary_response, color = trial_ID)) +
  geom_jitter(height = 0.02, width = 0.1, alpha = 0.1) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_wrap(~colony)+
  theme_minimal()

min(data$distance_to_ball[data$binary_response == 0], na.rm = TRUE)

small <- c(16,21)
high <- c(22,26)

data <- data %>%
  mutate(bin_inst_emerged = case_when(
    inst_emerged >= small[1] & inst_emerged <= small[2] ~ "small",
    #inst_emerged >= medium[1] & inst_emerged <= medium[2] ~ "medium",
    inst_emerged >= high[1] & inst_emerged <= high[2] ~ "high",
    TRUE ~ NA_character_
  ))

ggplot(data, aes(x = distance_to_ball, y = binary_response, color = bin_inst_emerged, group = bin_inst_emerged)) +
  geom_jitter(height = 0.05, widt h = 0.05, alpha = 0.5) +  # Show individual points
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(y = "Probability of Hide", x = "Distance to ball") +
  theme_classic()

#C - By total colony size. If bigger colonies confer more safety in numbers, would expect them to stay emerged at closer distances to the ball when expected when colony size is larger. 
#Histogram total colony size for each drop (add to each row), split into 3 low med high total colony size give different colours and plot
hist(data$colony_size)

range <- (max(data$colony_size)-min(data$colony_size))

small <- c(1)
medium <- c(25,45)
high <- c(46,65)

data <- data %>%
  mutate(bin_colony_size = case_when(
    colony_size >= small[1] & colony_size <= small[2] ~ "small",
    colony_size >= medium[1] & colony_size <= medium[2] ~ "medium",
    colony_size >= high[1] & colony_size <= high[2] ~ "high",
    TRUE ~ NA_character_
  ))



ggplot(data, aes(x = distance_to_ball, y = binary_response, color = bin_colony_size, group = bin_colony_size)) +
  geom_jitter(height = 0.05, width = 0.05, alpha = 0.5) +  # Show individual points
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(y = "Probability of Hide", x = "Distance to ball") +
  theme_classic()


small <- c(16,21)
high <- c(22,26)

data <- data %>%
  mutate(bin_colony_size = case_when(
    colony_size >= small[1] & colony_size <= small[2] ~ "small",
    #inst_emerged >= medium[1] & inst_emerged <= medium[2] ~ "medium",
    colony_size >= high[1] & colony_size <= high[2] ~ "high",
    TRUE ~ NA_character_
  ))

ggplot(data, aes(x = distance_to_ball, y = binary_response, color = bin_inst_emerged, group = bin_inst_emerged)) +
  geom_jitter(height = 0.05, width = 0.05, alpha = 0.5) +  # Show individual points
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(y = "Probability of Hide", x = "Distance to ball") +
  theme_classic()


ggplot(data, aes(x = prev_hides, y = binary_response)) +
  geom_jitter(height = 0.05, width = 0.05, alpha = 0.5) +  # Show individual points
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_wrap(~colony) +
  labs(y = "Probability of Hide", x = "Number Emerged") +
  theme_classic()

#Interaction
model <- glmer(binary_response ~ distance_to_ball*inst_emerged + colony_size + (1|trial_ID) + (1|drop_ID) + (1|eel_ID), data = data, family = binomial(link = "logit"))
summary(model)

model <- glmer(binary_response ~ distance_to_ball + colony_size + prop_inst_emerged + (1|trial_ID/drop_ID/eel_ID), data = data, family = binomial(link = "logit"))
summary(model)


min_dist <- min(data$distance_to_ball, na.rm = TRUE)
max_dist <- max(data$distance_to_ball, na.rm = TRUE)

data <- data %>%
  mutate(bin_distance_to_ball = case_when(
    distance_to_ball <= 2 ~ "close",
    distance_to_ball > 2 ~ "far",
    TRUE ~ NA_character_
  ))

data %>%
  filter(!is.na(bin_distance_to_ball)) %>%
ggplot(aes(x = inst_emerged, y = binary_response, color = bin_distance_to_ball, group = bin_distance_to_ball)) +
  geom_jitter(height = 0.05, width = 0.05, alpha = 0.5) +  # Show individual points
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(y = "Probability of Hide", x = "Instaneous emerged") +
  theme_classic()

#Inter-individual distances. Go to an image with 95% emerged and dot all the eels, guessing if they are down for that frame. Use these as the global coordinate map for the distance between individuals. Datapoints - Distance to ball (comes from each drop), direction that the eel is looking (even just binary), distance to nearest responding neighbour, distance to nearest non responding neighbour, distance to nearest hidden neighbour, size of colony, size of instantaneous colonies, position in group.


#Max response distance to ball
data %>%
  distinct(drop_ID, max_dist_resp) %>%
  ggplot(aes(x = max_dist_resp)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Max Response Distance per Drop",
    x = "Max Distance to Ball (Responders)",
    y = "Count of Drops"
  ) +
  theme_minimal()

#Cascade extent / size 
data %>%
  #filter(!trial_ID %in% c(6,7)) %>%
  distinct(drop_ID, cascade_extent, cascade_size, inst_emerged, colony, trial_ID) %>%
  mutate(prop_size = cascade_size/inst_emerged) %>%
  ggplot(aes(x = inst_emerged, y = cascade_extent, color = colony)) +
  geom_point() +
  labs(
    x = "Number of eels emerged before drop",
    y = "Cascade extent (distance from ball to furthest responder)"
  ) +
  facet_wrap(~colony)+
  theme_minimal()

#Plot inter lag response time distributions for colonies of different sizes / instantaneous sizes

#Likelihood of responding as a function of those already hid
data %>% 
  filter(full_partial_none != 0) %>%
  mutate(response_likeli = 1/inst_inst_emerged) %>%
  ggplot(aes(x = prev_hides/inst_emerged, y = response_likeli)) +
  geom_point()

data %>%
  mutate(prop_hidden = prev_hides/inst_emerged) %>%
  mutate(response_likeli = 1/inst_inst_emerged) %>%
  ggplot(aes(x = prop_hidden, y = response_likeli)) +
  geom_jitter(width = 0.05, height = 0.05) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"))

data$trial_ID <- as.factor(data$trial_ID)
data %>%
  ggplot(aes(x = (prev_hides/inst_emerged), y = binary_response, color = trial_ID)) +
  geom_jitter(width = 0.05, height = 0.05) +
  stat_smooth(method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(~colony) +
  theme_minimal()

#Time lag to respond as a function of distance
data <- data %>%
  group_by(drop_ID) %>%
  mutate(response_lag = if (all(is.na(response_frame_cam1))) {
    NA_real_
    } else {
      response_frame_cam1 - min(response_frame_cam1, na.rm = TRUE)
    }) %>%
  ungroup()

data_processed <- data %>%
  group_by(drop_ID) %>%
  # Identify first responder per drop (if any)
  mutate(
    first_index = if (any(full_partial_none != 0 & !is.na(full_partial_none)))
      which.min(ifelse(full_partial_none != 0 & !is.na(full_partial_none),
                       response_frame_cam1, Inf))
    else NA_integer_
  ) %>%
  # Grab first responder's coordinates (NA if none)
  mutate(
    first_x = avg_x[first_index],
    first_y = avg_y[first_index],
    first_z = avg_z[first_index],
    # Compute distance only for responders
    dist_from_first_resp = ifelse(
      full_partial_none != 0 & !is.na(full_partial_none),
      sqrt((avg_x - first_x)^2 + (avg_y - first_y)^2 + (avg_z - first_z)^2),
      NA_real_
    )
  ) %>%
  ungroup()



data_processed %>%
  filter(is.na(response_lag) == FALSE) %>%
  filter(full_partial_none != 0) %>%
  ggplot(aes(x = distance_to_ball, y = response_lag, color = colony)) +
  geom_point(alpha = 0.8) +
  stat_smooth(method = "lm", se = TRUE)+
  facet_wrap(~colony) +
  theme_minimal()

data_processed %>%
  filter(full_partial_none == 2) %>%
  ggplot(aes(x=response_lag)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(
    x = "Response lag",
    y = "Frequency"
  ) +
  facet_wrap(~colony) +
  theme_minimal()

model <- lmer(response_lag ~ distance_to_ball*dist_from_first_resp + colony_size + (1|trial_ID) + (1|drop_ID) + (1|eel_ID), data = data_processed)
summary(model)

data <- data %>%
  mutate(distXemerge = distance_to_ball*inst_emerged)
bysfitHxL <- ulam(
  alist(
    response_lag ~ dguass(mu),
    mu <- beta_0 * intercept +
      beta_1 * distance_to_ball +
      beta_2 * inst_emerged +
      beta_3 * distXemerge,
    beta_0 ~ dnorm(1, 10),
    beta_1 ~ dnorm(0, 10),
    beta_2 ~ dnorm(0, 10),
    beta_3 ~ dnorm(0, 10)
  ),
  data=data
)




lm = lm(response_lag ~ distance_to_ball*colony_size + inst_emerged, data = data)
summary(lm)
#Colony size

ggp

  