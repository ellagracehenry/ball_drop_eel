#Packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)
library(MASS) 
library(readxl)
library(lme4)
library(glmmLasso)
library(tibble)
library(stringr)

setwd("/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/My Drive/Colorado/PhD/PROJECTS/ball_drop_garden_eel/triangulation/final_triangulation")
data <- read_excel("final_master_ball_drop_3D.xlsx") %>%
  filter(drop_ID != 152) %>%
  filter (drop_ID != 169) %>%
  filter (drop_ID != 146) %>%
  filter(drop_ID != 176) %>%
  filter(drop_ID != 157) %>%
  filter(drop_ID != 147) %>%
  filter(drop_ID != 180) %>%
  filter(drop_ID != 179)  %>%
  filter(!trial_ID %in% c(17)) 

data$colony_drop_ID <- paste(data$drop_ID,":",data$colony,sep="")
data$colony_eel_ID <- paste(data$eel_ID,data$colony,sep = "_")

data$distance_to_ball <- sqrt((data$base_X - data$ball_hit_X)^2 + (data$base_Y - data$ball_hit_Y)^2 + (data$base_Z - data$ball_hit_Z)^2)

data <- data %>%
  mutate(binary_response = case_when(
    full_partial_none == 2 ~ 1,
    full_partial_none == 1 ~ 1,
    full_partial_none == 0 ~ 0,
    TRUE ~ NA_real_
  ))

data$colony_size[data$colony == "S5"] <- 34
data$colony_size[data$colony == "S9"] <- 59
data$colony_size[data$colony == "S15"] <- 67
data$colony_size[data$colony == "S12"] <- 47
data$colony_size[data$colony == "S7"] <- 116

data <- data %>%
  group_by(drop_ID) %>%
  mutate(inst_emerged = sum(!is.na(full_partial_none))) %>%
  ungroup()

all_colony_distances <- list()

for (c in 1:length(unique(data$colony))) {
  
  #filter out that colony data
  data_colony <- data %>%
    filter(colony == unique(data$colony)[c])
  
  # get the full set of eel IDs for this colony
  all_eels <- unique(data_colony$colony_eel_ID)
  
  trial_distances <- vector(mode="list", length = length(unique(data_colony$trial_ID)))
  
  #for each trial
  for (t in 1:length(unique(data_colony$trial_ID))) {
    
    current_trial <- unique(data_colony$trial_ID)[t]
    
    #average eel position per trial
    data_coords <- data_colony %>%
      filter(trial_ID == unique(data_colony$trial_ID)[t]) %>%
      group_by(colony_eel_ID) %>%
      summarise(avg_x = mean(base_X, na.rm=TRUE), avg_y = mean(base_Y, na.rm=TRUE), avg_z = mean(base_Z, na.rm=TRUE))
    
    #name the rownames the eel_ID
    coords <- data_coords[, c("avg_x", "avg_y", "avg_z")]
    rownames(coords) <- data_coords$colony_eel_ID
    
    #transform into distance matrix
    dist_mat <- as.data.frame(as.matrix(dist(coords)))
    
    # pad with NA rows/cols for eels absent in this trial
    missing_eels <- setdiff(all_eels, rownames(dist_mat))
    
    if (length(missing_eels) > 0) {
      # add NA rows for missing eels
      na_rows <- as.data.frame(matrix(NA, nrow = length(missing_eels), ncol = ncol(dist_mat),
                                      dimnames = list(missing_eels, colnames(dist_mat))))
      dist_mat <- rbind(dist_mat, na_rows)
      
      # add NA cols for missing eels
      na_cols <- as.data.frame(matrix(NA, nrow = nrow(dist_mat), ncol = length(missing_eels),
                                      dimnames = list(rownames(dist_mat), missing_eels)))
      dist_mat <- cbind(dist_mat, na_cols)
    }
    
    # reorder rows and cols to consistent order across trials
    dist_mat <- dist_mat[all_eels, all_eels]
    
    # convert matrix to long format
    dist_long <- dist_mat %>%
      tibble::rownames_to_column("eel_1") %>%
      pivot_longer(
        cols = -eel_1,
        names_to = "eel_2",
        values_to = "distance"
      ) %>%
      mutate(
        colony = unique(data_colony$colony),
        trial_ID = current_trial
      )
    
    # optional: remove self-distances
    dist_long <- dist_long %>%
      filter(eel_1 != eel_2)
    
    trial_distances[[t]] <- dist_long
  }
  
  all_colony_distances[[c]] <- bind_rows(trial_distances)
}

final_distances <- bind_rows(all_colony_distances)


final_distances$trial_ID <- as.factor(final_distances$trial_ID)

final_distances$pair <- apply(
  final_distances[, c("eel_1", "eel_2")],
  1,
  function(x) paste(sort(x), collapse = " — ")
)

final_distances <- final_distances %>%
  distinct(colony, trial_ID, pair, .keep_all = TRUE)

final_distances <- final_distances %>%
  filter(!is.na(distance))


library(ggplot2)
library(dplyr)
library(ggforce)  # for geom_sina

# --- Shared prep: filter to one colony, remove NAs ---
df <- final_distances %>%
  filter(colony == "S12", !is.na(distance)) %>%
  group_by(pair) %>%
  filter(n_distinct(trial_ID) > 1) %>%  # only pairs seen in >1 trial
  mutate(median_dist = median(distance, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pair = reorder(pair, median_dist)) #%>%  # order by median %>%
  filter(distance <1)


# -------------------------------------------------------
# OPTION 2: Sina plot
# -------------------------------------------------------
ggplot(df, aes(x = pair, y = distance, color = trial_ID)) +
    stat_summary(                          # 20cm band around median
      aes(group = 1),
      fun.min = function(x) median(x) - 0.30,
      fun.max = function(x) median(x) + 0.30,
      geom = "ribbon",
      fill = "grey80", color = NA, alpha = 0.4
    ) +
    geom_sina(size = 1, alpha = 0.6) +
    stat_summary(                          # median crossbar on top
      fun = median, geom = "crossbar",
      width = 0.5, color = "black", linewidth = 0.4
    ) +
    labs(x = "Pair (ordered by median distance)",
         y = "Distance (m)",
         title = "Within-pair distance variability across trials — S5") +
    theme_bw() +
    theme(axis.ticks.x = element_blank())

# -------------------------------------------------------
# OPTION 3: Dumbbell / linerange plot
# -------------------------------------------------------
df_summary <- df %>%
  group_by(pair, median_dist) %>%
  summarise(min_dist  = min(distance, na.rm = TRUE),
            max_dist  = max(distance, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(pair = reorder(pair, median_dist))

ggplot(df_summary, aes(y = pair)) +
  geom_linerange(aes(xmin = min_dist, xmax = max_dist),
                 color = "#4d4d4d", linewidth = 0.5) +
  geom_point(aes(x = min_dist), color = "#2c7bb6", size = 1.5) +
  geom_point(aes(x = max_dist), color = "#d7191c", size = 1.5) +
  geom_point(aes(x = median_dist), color = "black",
             shape = 21, fill = "white", size = 2) +
  labs(x = "Distance (m)", y = "Pair (ordered by median distance)",
       title = "Min–max range of inter-eel distance across trials — S5",
       caption = "Blue = min, Red = max, White = median") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# -------------------------------------------------------
# OPTION 4: CV-ranked dot plot
# -------------------------------------------------------
df_cv <- df %>%
  group_by(pair) %>%
  summarise(mean_dist = mean(distance, na.rm = TRUE),
            sd_dist   = sd(distance,   na.rm = TRUE),
            n_trials  = n_distinct(trial_ID),
            .groups = "drop") %>%
  mutate(cv = sd_dist / mean_dist) %>%
  arrange(cv) %>%
  mutate(rank = row_number())

ggplot(df_cv, aes(x = rank, y = cv)) +
  geom_point(aes(color = mean_dist), size = 2.5, alpha = 0.8) +
  scale_color_viridis_c(name = "Mean dist (m)") +
  labs(x = "Pair rank (least → most variable)",
       y = "Coefficient of variation (SD / mean)",
       title = "Variability of inter-eel distances across trials — S5") +
  theme_bw()



length(unique(data$eel_ID[data$trial_ID == 9]))


