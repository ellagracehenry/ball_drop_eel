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
library(pracma)
library(spatstat)
library(deldir)
library(MuMIn) #https://ecologyforacrowdedplanet.wordpress.com/2013/08/27/r-squared-in-mixed-models-the-easy-way/

source("~/Desktop/PhD/academic_projects/ball_drop_eel/scripts/SIR_model/R/sharededge.R")

#Some drops the first response frame is before the ball enters view, filter these. - 176
#Fish in frame - 146
#Some have far apart second and first because eel across the group started to hide but it seems like the others don't see - 168, 40
#Tangled line - 157

#Data manipulation
setwd("/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/My Drive/Colorado/PhD/PROJECTS/ball_drop_garden_eel/triangulation/final_triangulation")
data <- read_excel("final_master_ball_drop_3D.xlsx") %>%
  filter(drop_ID != 152) %>%
  filter (drop_ID != 169) %>%
  filter (drop_ID != 146) %>%
  filter(drop_ID != 176) %>%
  filter(drop_ID != 157) %>%
  filter(drop_ID != 147) %>%
  filter(drop_ID != 180) %>%
  filter(drop_ID != 179) %>%
  #filter(trial_ID != 5) %>%
  filter(drop_ID != 173) %>%
  filter(trial_ID != 17) %>% #needs correcting annotations %>%
  filter(drop_ID != 149)  #two 156s?

data %>%
  group_by(colony) %>%
  summarise(n_drop_ids = n_distinct(drop_ID))

data$colony_drop_ID <- paste(data$drop_ID,":",data$colony,sep="")
data$colony_eel_ID <- paste(data$eel_ID,data$colony,sep = "_")

data$distance_to_ball <- sqrt((data$base_X - data$ball_hit_X)^2 + (data$base_Y - data$ball_hit_Y)^2 + (data$base_Z - data$ball_hit_Z)^2)

#Computing global distances

# --- reference trials per colony ---
ref_trials <- c("S5" = 1, "S9" = 2, "S15" = 7, "S12" = 13, "S7" = 17)

# Step 1: get global eel positions from reference trial only
global_positions <- data %>%
  # cleaner way:
  filter(paste(colony, trial_ID) %in% paste(names(ref_trials), ref_trials)) %>%
  group_by(colony, colony_eel_ID) %>%
  summarise(
    global_X = mean(base_X, na.rm = TRUE), #taking the average of the all positions for that trial
    global_Y = mean(base_Y, na.rm = TRUE),
    global_Z = mean(base_Z, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    !is.nan(global_X),
    !is.nan(global_Y),
    !is.nan(global_Z)
  )

# join global positions onto every row
data <- data %>%
  left_join(global_positions, by = c("colony","colony_eel_ID"))


# --- rigid transform function ---
rigid_transform_3D <- function(source_pts, target_pts) {
  source_centroid <- colMeans(source_pts)
  target_centroid <- colMeans(target_pts)
  A <- sweep(source_pts, 2, source_centroid)
  B <- sweep(target_pts, 2, target_centroid)
  H <- t(A) %*% B
  svd_H <- svd(H)
  R <- svd_H$v %*% t(svd_H$u)
  if (det(R) < 0) {
    svd_H$v[, 3] <- -svd_H$v[, 3]
    R <- svd_H$v %*% t(svd_H$u)
  }
  t_vec <- target_centroid - R %*% source_centroid
  list(R = R, t = t_vec)
}


# Step 2: transform ball per drop using 3 eels closest to ball
ball_global <- data %>%
  group_by(drop_ID) %>%
  group_modify(~ {
    
    drop_data <- .x
    
    # eels with both drop-space AND global positions, sorted by distance to ball
    landmarks <- drop_data %>%
      filter(!is.na(base_X), !is.na(global_X), !is.na(distance_to_ball)) %>%
      #filter(base_reproj_error < 5) %>%  # TO DO! CONFIRM WHETHER WE WANT THIS FILTER OR NOT. only well-triangulated eels as landmarks. DOES EVERYTHING GO TO SHIT IF THIS IS REMOVED?? CURRENTLY IT CULLS TRIAL 12 AND 14
      slice_min(distance_to_ball, n = 3, with_ties = FALSE)
    
    if (nrow(landmarks) < 3) {
      return(tibble(
        ball_global_X  = NA_real_,
        ball_global_Y  = NA_real_,
        ball_global_Z  = NA_real_,
        n_landmarks    = nrow(landmarks),
        transform_rmse = NA_real_
      ))
    }
    
    source_pts <- as.matrix(landmarks[, c("base_X",   "base_Y",   "base_Z")])
    target_pts <- as.matrix(landmarks[, c("global_X", "global_Y", "global_Z")])
    
    tf <- rigid_transform_3D(source_pts, target_pts)
    
    # transform ball
    ball <- as.numeric(drop_data[1, c("ball_hit_X", "ball_hit_Y", "ball_hit_Z")])
    ball_t <- as.numeric(tf$R %*% ball + tf$t)
  
    tibble(
      ball_global_X  = ball_t[1],
      ball_global_Y  = ball_t[2],
      ball_global_Z  = ball_t[3]
    )
  }) %>%
  ungroup()

# join back onto data
data <- data %>%
  left_join(ball_global, by = "drop_ID")


data$distance_to_ball <- sqrt((data$global_X - data$ball_global_X)^2 + (data$global_Y - data$ball_global_Y)^2 + (data$global_Z - data$ball_global_Z)^2)
data$log_distance_to_ball <- log(sqrt((data$global_X - data$ball_global_X)^2 + (data$global_Y - data$ball_global_Y)^2 + (data$global_Z - data$ball_global_Z)^2))


data <- data %>%
  mutate(binary_response = case_when(
    full_partial_none == 2 ~ 1,
    full_partial_none == 1 ~ 1,
    full_partial_none == 0 ~ 0,
    TRUE ~ NA_real_
  ))

data$colony_size <- NA
data$colony_size[data$colony == "S5"] <- 34
data$colony_size[data$colony == "S9"] <- 59
data$colony_size[data$colony == "S15"] <- 67
data$colony_size[data$colony == "S12"] <- 47
data$colony_size[data$colony == "S7"] <- 116

data <- data %>%
  group_by(drop_ID) %>%
  mutate(inst_emerged = sum(!is.na(full_partial_none))) %>%
  ungroup()

#True ordering
# data <- data %>%
#   group_by(drop_ID) %>%
#   mutate(
#     emerged = as.integer(!is.na(full_partial_none)),
#     any_response = any(full_partial_none != 0 & !is.na(full_partial_none)), 
#     rank_order = rank(ifelse(full_partial_none != 0, response_frame_cam1, NA),  
#                       na.last = "keep", ties.method = "min"),
#     first_responder = case_when(rank_order == 1 ~ 1, emerged == 1 & binary_response == 0 ~ 0, emerged == 1 & rank_order > 1 ~0, TRUE ~ NA_real_),
#     first_index = if (first(any_response)) which.min(ifelse(first_responder == 1, response_frame_cam1, Inf)) else NA_integer_,
#     second_responder = case_when(!any_response ~ NA_real_, rank_order == 1 ~ NA, rank_order == 2 ~ 1, emerged == 1 ~ 0, TRUE ~ NA_real_),
#     third_responder = case_when(!any_response ~ NA_real_, rank_order %in% c(1,2) ~ NA, rank_order == 3 ~ 1, emerged == 1 ~ 0, TRUE ~ NA_real_),
#     fourth_responder = case_when(!any_response ~ NA_real_, rank_order %in% c(1,2,3) ~ NA, rank_order == 4 ~ 1, emerged == 1 ~ 0, TRUE ~ NA_real_),
#     fifth_responder = case_when(!any_response ~ NA_real_, rank_order %in% c(1,2,3,4)  ~ NA, rank_order == 5 ~ 1, emerged == 1 ~ 0, TRUE ~ NA_real_),
#     sixth_responder = case_when(!any_response ~ NA_real_, rank_order %in% c(1,2,3,4,5)  ~ NA, rank_order == 6 ~ 1, emerged == 1 ~ 0, TRUE ~ NA_real_),
#     subsequent_responder = case_when(!any_response ~ NA_real_, emerged == 0 ~ NA_real_, rank_order == 1 ~ NA_real_, binary_response == 0 ~ 0, rank_order > 1 ~ 1),
#     first_x = global_X[first_index],
#     first_y = global_Y[first_index],
#     first_z = global_Z[first_index],
#     # Compute distance only for emerged
#     time_lag_since_first =
#       response_frame_cam1 - response_frame_cam1[first_index]
#     ) %>%
#   ungroup()

#Incorporating time lag. Need to move everyone up a rank
data <- data %>%
  group_by(drop_ID) %>%
  mutate(
    emerged = as.integer(!is.na(full_partial_none)),
    any_response = any(full_partial_none != 0 & !is.na(full_partial_none)),
    
    # Step 1: true chronological rank (matches true ordering exactly)
    raw_rank = rank(ifelse(full_partial_none != 0, response_frame_cam1, NA),
                    na.last = "keep", ties.method = "min"),
    
    # Step 2: find initiator frame (matches true ordering first_index logic)
    first_index = if (first(any_response)) 
      which.min(ifelse(raw_rank == 1, response_frame_cam1, Inf)) 
    else NA_integer_,
    
    first_ID = colony_eel_ID[first_index],
    
    initiator_frame = response_frame_cam1[first_index],
    time_from_init = response_frame_cam1 - initiator_frame
  ) %>%
  
  # Step 3: collapse within-threshold individuals to rank 1 (only change from true ordering)
  mutate(
    n_first = sum(time_from_init <= 4 & !is.na(raw_rank), na.rm = TRUE),
    rank_order = case_when(
      is.na(raw_rank)      ~ NA_real_,
      time_from_init <= 4 ~ 1,
      TRUE                 ~ raw_rank - (n_first - 1)
    )
  ) %>%
  
  mutate(
    first_responder = case_when(
      rank_order == 1 ~ 1,
      emerged == 1 & binary_response == 0 ~ 0,
      emerged == 1 & rank_order > 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    second_responder = case_when(
      !any_response ~ NA_real_,
      rank_order == 1 ~ NA,
      rank_order == 2 ~ 1,
      emerged == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    third_responder = case_when(
      !any_response ~ NA_real_,
      rank_order %in% c(1,2) ~ NA,
      rank_order == 3 ~ 1,
      emerged == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    fourth_responder = case_when(
      !any_response ~ NA_real_,
      rank_order %in% c(1,2,3) ~ NA,
      rank_order == 4 ~ 1,
      emerged == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    fifth_responder = case_when(
      !any_response ~ NA_real_,
      rank_order %in% c(1,2,3,4) ~ NA,
      rank_order == 5 ~ 1,
      emerged == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    sixth_responder = case_when(
      !any_response ~ NA_real_,
      rank_order %in% c(1,2,3,4,5) ~ NA,
      rank_order == 6 ~ 1,
      emerged == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    subsequent_responder = case_when(
      !any_response ~ NA_real_,
      emerged == 0 ~ NA_real_,
      rank_order == 1 ~ NA_real_,
      binary_response == 0 ~ 0,
      rank_order > 1 ~ 1
    ),
    
    first_x = global_X[first_index],
    first_y = global_Y[first_index],
    first_z = global_Z[first_index],
    time_lag_since_first = time_from_init
  ) %>%
  ungroup()

data <- data %>%
  group_by(drop_ID) %>%
  mutate(n_responders = sum(binary_response, na.rm=TRUE)) %>%
  ungroup()

data$dist_from_first <- NA
data$log_dist_from_first <- NA
data$global_topo_dist_from_first <- NA
data$log_global_topo_dist_from_first <- NA
data$inst_topo_dist_from_first <- NA
data$log_inst_topo_dist_from_first <- NA
data$global_vor_neighbours <- vector("list", nrow(data))
data$inst_vor_neighbours <- vector("list", nrow(data))
data$first_in_global_voronoi <- NA
data$first_in_inst_voronoi <- NA
data$inst_neighbours_topo_ranked <- vector("list", nrow(data))
data$global_neighbours_topo_ranked <- vector("list", nrow(data))

#computing other social metrics
for (g in unique(data$drop_ID)) {
  if (sum(data$emerged[data$drop_ID == g], na.rm=TRUE) > 2) {
      for (gg in unique(data$colony_eel_ID[data$drop_ID == g])) {
        focal <- gg
        focal_colony <- data$colony[data$colony_eel_ID == gg & data$drop_ID == g] 
        emerged <- data$colony_eel_ID[data$drop_ID == g & data$emerged == 1]
        focal_positions <- NA
        global_positions_sub <- global_positions[!is.na(global_positions$global_X),]
        if (focal %in% global_positions$colony_eel_ID) {
          if (data$emerged[data$colony_eel_ID == gg & data$drop_ID == g] == 1) {
            #Global
            focal_global_positions <- global_positions_sub[global_positions_sub$colony == focal_colony & !is.na(global_positions_sub$global_X),]
            focal_global_positions$distance_to_focal <- sqrt((focal_global_positions$global_X - focal_global_positions$global_X[focal_global_positions$colony_eel_ID == focal])^2 + (focal_global_positions$global_Y - focal_global_positions$global_Y[focal_global_positions$colony_eel_ID == focal])^2 + (focal_global_positions$global_Z - focal_global_positions$global_Z[focal_global_positions$colony_eel_ID == focal])^2)
            focal_global_positions$rank <- rank(focal_global_positions$distance_to_focal, na.last = "keep", ties.method = "first")
            focal_global_positions_ranked <- focal_global_positions[order(focal_global_positions$rank),]
            focal_global_positions_ranked <- focal_global_positions_ranked [focal_global_positions_ranked$distance_to_focal != 0,]
            colony_eel_ID_global_ranked_for_focal <- as.vector(focal_global_positions_ranked$colony_eel_ID)
            center <- colMeans(focal_global_positions[,c("global_X", "global_Y", "global_Z")], na.rm=TRUE) #get centre
            pts_centered <- sweep(as.matrix(focal_global_positions[,c("global_X","global_Y","global_Z")],), 2, center) #centre points
            pca <- prcomp(pts_centered, center =FALSE) #fit plane via PCA, 3rd PC to normal to the best-fit plane
            min_pc <- which.min(pca$sdev^2)  # flattest = normal to plane → new Z
            max_pc <- which.max(pca$sdev^2)  # most spread → new X
            mid_pc <- setdiff(1:3, c(min_pc, max_pc))  # middle → new Y
            pts_rotated <- as.data.frame(pts_centered %*% pca$rotation[, c(max_pc, mid_pc, min_pc)])
            colnames(pts_rotated) <- c("X_width", "Y_length", "Z_flat")
            pts_rotated$id <- focal_global_positions$colony_eel_ID
            points <- ppp(x = pts_rotated$X_width, y = pts_rotated$Y_length, window = owin(xrange=c(min(pts_rotated$X_width), max(pts_rotated$X_width)), yrange=c(min(pts_rotated$Y_length), max(pts_rotated$Y_length))))
            sharededge <- function(X) {
              verifyclass(X, "ppp")
              Y <- X[as.rectangle(X)]
              dX <- deldir(Y)
              DS <- dX$dirsgs
              xyxy <- DS[,1:4]
              names(xyxy) <- c("x0","y0","x1","y1")
              sX <- as.psp(xyxy,window=dX$rw)
              marks(sX) <- 1:nobjects(sX)
              sX <- sX[as.owin(X)]
              tX <- tapply(lengths_psp(sX), marks(sX), sum)
              jj <- as.integer(names(tX))
              ans <- data.frame(ind1=DS[jj,5], 
                                ind2=DS[jj,6], 
                                leng=as.numeric(tX))
              return(ans)
            }
            shared_edge_lengths <- sharededge(points)
            
            # Extract neighbour list
            voronoi_neighbours_df <- do.call(rbind, lapply(seq_len(nrow(pts_rotated)), function(i) {
              neighbour_rows <- shared_edge_lengths[shared_edge_lengths$ind1 == i | 
                                                      shared_edge_lengths$ind2 == i, ]
              neighbour_idx <- ifelse(neighbour_rows$ind1 == i, 
                                      neighbour_rows$ind2, 
                                      neighbour_rows$ind1)
              data.frame(
                focal     = pts_rotated$id[i],
                neighbour = pts_rotated$id[neighbour_idx]
              )
            }))
            
            global_v_neighbours_list <- voronoi_neighbours_df %>%
              group_by(focal) %>%
              summarise(v_neighbours = list(neighbour))
            
            #Instantaneous
            focal_inst_positions <- focal_global_positions[focal_global_positions$colony_eel_ID %in% emerged, ]
            focal_inst_positions$distance_to_focal <- NA
            focal_inst_positions$distance_to_focal <- sqrt((focal_inst_positions$global_X - focal_inst_positions$global_X[focal_inst_positions$colony_eel_ID == focal])^2 + (focal_inst_positions$global_Y - focal_inst_positions$global_Y[focal_inst_positions$colony_eel_ID == focal])^2 + (focal_inst_positions$global_Z - focal_inst_positions$global_Z[focal_inst_positions$colony_eel_ID == focal])^2)
            focal_inst_positions$rank <- rank(focal_inst_positions$distance_to_focal, na.last = "keep", ties.method = "min")
            focal_inst_positions_ranked <- focal_inst_positions[order(focal_inst_positions$rank),]
            focal_inst_positions_ranked <- focal_inst_positions_ranked[focal_inst_positions_ranked$distance_to_focal != 0,]
            colony_eel_ID_inst_ranked_for_focal <- as.vector(focal_inst_positions_ranked$colony_eel_ID)
            center <- colMeans(focal_inst_positions[,c("global_X", "global_Y", "global_Z")], na.rm=TRUE) #get centre
            pts_centered <- sweep(as.matrix(focal_inst_positions[,c("global_X","global_Y","global_Z")]), 2, center) #centre points
            pca <- prcomp(pts_centered, center =FALSE) #fit plane via PCA, 3rd PC to normal to the best-fit plane
            min_pc <- which.min(pca$sdev^2)  # flattest = normal to plane → new Z
            max_pc <- which.max(pca$sdev^2)  # most spread → new X
            mid_pc <- setdiff(1:3, c(min_pc, max_pc))  # middle → new Y
            pts_rotated <- as.data.frame(pts_centered %*% pca$rotation[, c(max_pc, mid_pc, min_pc)])
            colnames(pts_rotated) <- c("X_width", "Y_length", "Z_flat")
            pts_rotated$id <- focal_inst_positions$colony_eel_ID
            points <- ppp(x = pts_rotated$X_width, y = pts_rotated$Y_length, window = owin(xrange=c(min(pts_rotated$X_width), max(pts_rotated$X_width)), yrange=c(min(pts_rotated$Y_length), max(pts_rotated$Y_length))))
            sharededge <- function(X) {
              verifyclass(X, "ppp")
              Y <- X[as.rectangle(X)]
              dX <- deldir(Y)
              DS <- dX$dirsgs
              xyxy <- DS[,1:4]
              names(xyxy) <- c("x0","y0","x1","y1")
              sX <- as.psp(xyxy,window=dX$rw)
              marks(sX) <- 1:nobjects(sX)
              sX <- sX[as.owin(X)]
              tX <- tapply(lengths_psp(sX), marks(sX), sum)
              jj <- as.integer(names(tX))
              ans <- data.frame(ind1=DS[jj,5], 
                                ind2=DS[jj,6], 
                                leng=as.numeric(tX))
              return(ans)
            }
            shared_edge_lengths <- sharededge(points)
            
            # Extract neighbour list
            voronoi_neighbours_df <- do.call(rbind, lapply(seq_len(nrow(pts_rotated)), function(i) {
              neighbour_rows <- shared_edge_lengths[shared_edge_lengths$ind1 == i | 
                                                      shared_edge_lengths$ind2 == i, ]
              neighbour_idx <- ifelse(neighbour_rows$ind1 == i, 
                                      neighbour_rows$ind2, 
                                      neighbour_rows$ind1)
              data.frame(
                focal     = pts_rotated$id[i],
                neighbour = pts_rotated$id[neighbour_idx]
              )
            }))
            
            inst_v_neighbours_list <- voronoi_neighbours_df %>%
              group_by(focal) %>%
              summarise(v_neighbours = list(neighbour))
            
            #Metrics for all drops
            data$global_vor_neighbours[data$colony_eel_ID == gg & data$drop_ID == g] <- global_v_neighbours_list$v_neighbours[global_v_neighbours_list$focal == focal]
            data$inst_vor_neighbours[data$colony_eel_ID == gg & data$drop_ID == g] <- inst_v_neighbours_list$v_neighbours[inst_v_neighbours_list$focal == focal]
            data$inst_neighbours_topo_ranked[data$colony_eel_ID == gg & data$drop_ID == g] <- list(colony_eel_ID_inst_ranked_for_focal)
            data$global_neighbours_topo_ranked[data$colony_eel_ID == gg & data$drop_ID == g] <- list(colony_eel_ID_global_ranked_for_focal)
            
            #Metrics for drops with responses
            first_responder_id <- data %>%
              filter(drop_ID == g, first_responder == 1) %>%
              slice_min(response_frame_cam1, n = 1, with_ties = FALSE) %>%
              pull(colony_eel_ID)
            
            # check for exact ties among rank_order == 1
            n_exact_ties <- data %>%
              filter(drop_ID == g, first_responder == 1) %>%
              summarise(n = sum(response_frame_cam1 == min(response_frame_cam1, na.rm = TRUE), na.rm = TRUE)) %>%
              pull(n)
            
            first_has_global <- n_exact_ties == 1 &&
              length(first_responder_id) == 1 &&
              !is.na(data$global_X[data$colony_eel_ID == first_responder_id & data$drop_ID == g])
            
            n_first_responders <- sum(data$drop_ID == g & data$first_responder == 1, na.rm = TRUE)
            
            if (n_first_responders == 1 && first_has_global && first_responder_id != gg) {
              # all the distance calculations — unchanged
              data$dist_from_first[data$colony_eel_ID == gg & data$drop_ID == g] <- focal_global_positions$distance_to_focal[focal_global_positions$colony_eel_ID == first_responder_id]
              data$log_dist_from_first[data$colony_eel_ID == gg & data$drop_ID == g] <- log(focal_global_positions$distance_to_focal[focal_global_positions$colony_eel_ID == first_responder_id])
              data$global_topo_dist_from_first[data$colony_eel_ID == gg & data$drop_ID == g] <- focal_global_positions$rank[focal_global_positions$colony_eel_ID == first_responder_id] - 1
              data$log_global_topo_dist_from_first[data$colony_eel_ID == gg & data$drop_ID == g] <- log(focal_global_positions$rank[focal_global_positions$colony_eel_ID == first_responder_id] - 1)
              data$inst_topo_dist_from_first[data$colony_eel_ID == gg & data$drop_ID == g] <- focal_inst_positions$rank[focal_inst_positions$colony_eel_ID == first_responder_id] - 1
              data$log_inst_topo_dist_from_first[data$colony_eel_ID == gg & data$drop_ID == g] <- log(focal_inst_positions$rank[focal_inst_positions$colony_eel_ID == first_responder_id] - 1)
              data$first_in_global_voronoi[data$colony_eel_ID == gg & data$drop_ID == g] <- list(ifelse(first_responder_id %in% data$global_vor_neighbours[data$colony_eel_ID == gg & data$drop_ID == g][[1]], 1, 0))
              data$first_in_inst_voronoi[data$colony_eel_ID == gg & data$drop_ID == g] <- list(ifelse(first_responder_id %in% data$inst_vor_neighbours[data$colony_eel_ID == gg & data$drop_ID == g][[1]], 1, 0))
            }
            
          
          }
        }
      }
    }
  }





#Summarise data for time lag between first and second
first_pair_time_lag <- data %>% 
  filter(second_responder == 1) %>% 
  group_by(drop_ID) %>%
  filter(sum(time_lag_since_first == 0, na.rm=TRUE) == 0) %>%
  ungroup()

first_pair_time_lag %>%
  ggplot(aes(x = time_lag_since_first * (1000/60))) +
  geom_histogram(binwidth = 3 * (1000/60), color = "black", fill = "lightblue") +
  labs(x = "Time lag (ms)")

first_pair_time_lag %>%
  ggplot(aes(x=dist_from_first, y = time_lag_since_first* (1000/60))) +
  geom_point() +
  geom_smooth(method ="lm") +
  labs(y = "Time lag (ms)", x = "Distance from first responder")

##Correlation test for distance to first resp and to ball
d <- initator_responder %>% filter(!is.na(distance_to_ball) & !is.na(ball_sc))
cor(d$dist_from_first, d$distance_to_ball)
#0.64 - correlated but not that much - and gllmmLASSO doesn't make one zero - keep both! both important, use both in each individuals decision

d <- initator_responder %>% filter(!is.na(log_inst_topo_dist_sc) & !is.na(log_ball_sc))
cor(d$log_inst_topo_dist_sc, d$log_ball_sc)
d <- initator_responder %>% filter(!is.na(ball_sc) & !is.na(log_ball_sc))
cor(d$ball_sc, d$log_ball_sc)


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
#Simulate the ball landing at a random position
#Draw first responder using the parameter from the likelihood of being a first responder with distance from ball (logistic regression 1 or 0 first responder with distance from ball)
#For each ms after first responder, compute the dose experience by each individual. 
#Set a dose threshold (can simulate across) and if inidividual dose exceeds this, it is activated - flees
#if flees, this acts to affect the dose of the individuals around it.
#compute cascade size across many simulations of group size and dose threshold
#see how these cascade size distributions compare to real data
#For each trial, get the ball position and the eel positions, compute an interaction network
#Weights - probability individual i startles given that individual j has startle. The logistic regression gives you w_ij, and you're justified in using it as p_ij in the contagion model because of the proportionality argument. 

#1 - First responder model
fr_model_all <- glmer(first_responder ~ distance_to_ball + (1|colony/colony_eel_ID) + (1|drop_ID) + (1|date), family = binomial, data = data)
summary(fr_model_all)
data %>%
  ggplot(aes(x = distance_to_ball, y = first_responder)) +
  geom_point() +
  geom_smooth()

#R2
r.squaredGLMM(fr_model_all) #worse r^2 than sans sbubs

# Fixed effects
fr_intercept <- as.numeric(fixef(fr_model)[1])  # gives β₀ and β_distance
fr_b_dist <- as.numeric(fixef(fr_model)[2]) 
# Random effects
fr_re_colony_colony_eel_ID <- ranef(fr_model)$'colony_eel_ID:colony'
fr_re_colony_colony_eel_ID$combo <- rownames(fr_re_colony_colony_eel_ID)
fr_re_colony_colony_eel_ID$name <- str_extract(fr_re_colony_colony_eel_ID$combo, "^[^:]+")
fr_re_drop_ID <- ranef(fr_model)$drop_ID    # u_drop for each drop nested in colony
fr_re_drop_ID$combo <- rownames(fr_re_drop_ID)
fr_re_date <- ranef(fr_model)$date
fr_re_date$combo <- rownames(fr_re_date)
fr_re_colony <- ranef(fr_model)$colony
fr_re_colony$combo <- rownames(fr_re_colony)

data_sans_subs <- data %>% filter(is.na(subsequent_responder) | subsequent_responder!= 1)
fr_model_sans_subs <- glmer(first_responder ~ distance_to_ball + (1|colony/colony_eel_ID) + (1|drop_ID) + (1|date), family = binomial, data = data_sans_subs)
summary(fr_model_sans_subs)
data_sans_subs %>%
  ggplot(aes(x = distance_to_ball, y = first_responder)) +
  geom_point() +
  geom_smooth()
# Fixed effects
fr_intercept_sans_subs <- as.numeric(fixef(fr_model_sans_subs)[1])  # gives β₀ and β_distance
fr_b_dist_sans_subs <- as.numeric(fixef(fr_model_sans_subs)[2]) 

r.squaredGLMM(fr_model_sans_subs) # much higher with non log

#First responder bayesian

#2 - Second responder model
sr_model <- glmer(second_responder ~ log_distance_to_ball + log_inst_topo_dist_from_first + (1 | colony/colony_eel_ID) + (1|drop_ID) + (1|date), family = binomial, data = initator_responder)
summary(sr_model)

#R2
r.squaredGLMM(sr_model) #lots to look at with log vs non log, grouped first responders or not

#Fixed effects
sr_intercept <- as.numeric(fixef(sr_model)[1])
sr_b_dist_ball <- as.numeric(fixef(sr_model)[2])
sr_b_dist_first <- as.numeric(fixef(sr_model)[3])

#Random effects
sr_re_colony_colony_eel_ID <- ranef(sr_model)$'colony_eel_ID:colony'
sr_re_colony_colony_eel_ID$combo <- rownames(sr_re_colony_colony_eel_ID)
sr_re_colony_colony_eel_ID$name <- str_extract(sr_re_colony_colony_eel_ID$combo, "^[^:]+")
sr_re_drop_ID <- ranef(sr_model)$drop_ID    # u_drop for each drop nested in colony
sr_re_drop_ID$combo <- rownames(sr_re_drop_ID)
sr_re_date <- ranef(sr_model)$date
sr_re_date$combo <- rownames(sr_re_date)
sr_re_colony <- ranef(sr_model)$colony
sr_re_colony$combo <- rownames(sr_re_colony)

coefs <- list()
coefs[1] <- fr_intercept_sans_subs 
coefs[2] <- fr_b_dist_sans_subs
coefs[3] <- sr_intercept
coefs[4] <- sr_b_dist_first
coefs[5] <- sr_b_dist_ball


#3 - Subsequent responder model
subs_model <- glmer(subsequent_responder ~ log_distance_to_ball*log_inst_topo_dist_from_first + (1|colony/colony_eel_ID) + (1|drop_ID) + (1|date), family = binomial, data = data)
summary(subs_model)
#Fixed effects
subs_intercept <- as.numeric(fixef(subs_model)[1])
subs_b_dist_ball <- as.numeric(fixef(subs_model)[2])
#Random effects
subs_re_colony_colony_eel_ID <- ranef(subs_model)$'colony_eel_ID:colony'
subs_re_colony_colony_eel_ID$combo <- rownames(subs_re_colony_colony_eel_ID)
subs_re_colony_colony_eel_ID$name <- str_extract(subs_re_colony_colony_eel_ID$combo, "^[^:]+")
subs_re_drop_ID <- ranef(subs_model)$drop_ID    # u_drop for each drop nested in colony
subs_re_drop_ID$combo <- rownames(subs_re_drop_ID)
subs_re_date <- ranef(subs_model)$date
subs_re_date$combo <- rownames(subs_re_date)
subs_re_colony <- ranef(subs_model)$colony
subs_re_colony$combo <- rownames(subs_re_colony)

n_drops <- length(unique(data$drop_ID))

#All responder model
all_model <- glmer(binary_response ~ log_distance_to_ball*log_inst_topo_dist_from_first + (1|colony/colony_eel_ID) + (1|drop_ID) + (1|date), family = binomial, data = data)
summary(all_model)


#First responder time lag
data_first <- data %>% filter(first_responder ==1)
data_first$lag <- data_first$response_frame_cam1 - data_first$ball_hit_frame_cam1 #small number means you responded early on
hist(data_first$lag)
data_first %>%
  ggplot(aes(distance_to_ball, lag)) +
  geom_point() +
  geom_smooth(method = "lm")
#no correlation with distance to ball and response lag - closer individuals do not respond earlier. Simulation starts at the first responder, doesn't matter where they are from the ball. 

######### Model fit weight strengths (option 1) ###########
weight_strengths <- vector(mode="list", length = length(unique(data$colony)))
colony_distances <- vector(mode="list", length = length(unique(data$colony)))

for (c in 1:length(unique(global_positions$colony))) {
  
  #filter out that colony data
  data_colony <- global_positions %>%
    filter(colony == unique(global_positions$colony)[c])
  
  # get the full set of eel IDs for this colony
  all_eels <- unique(data_colony$colony_eel_ID)

  #name the rownames the eel_ID
  coords <- data_colony[, c("global_X", "global_Y", "global_Z")]
  rownames(coords) <- all_eels
    
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
    
  dist_mean_lin_pred <- apply(trial_distances_mean, 
                                c(1,2), 
                                function(x) {
                                  if (!is.na(x)) {
                                    1/(1+exp(-sr_intercept-(sr_b_dist_first*x))) #add in random effects!! but that would give a per drop weighting... 
                                  } else { 
                                    NA }
                                })
  
  weight_strengths[[c]] <- trial_distances_mean_lin_pred
  
  #Convert distance matrix to pairwise distance list
  xy <- t(combn(colnames(trial_distances_mean),2))
  xy_rev <- xy[, c(2, 1)]
  xy_all <- rbind(xy, xy_rev)
  xy_dist <- data.frame(xy_all, dist = trial_distances_mean[xy_all])
  colony_distances[[c]] <- xy_dist
  
}

colony_distances_all <- bind_rows(colony_distances)

#############################################

######## Directed empirical weight strengths  for initator and responder (option 2, too limited data...) ########
pairs_all <- vector(mode="list", length = length(unique(data$colony)))

for (c in 1:length(unique(data$colony))) {
  #filter out that colony data
  data_colony <- data %>%
    filter(colony == unique(data$colony)[c])
  
  # get the full set of eel IDs for this colony
  all_eels <- unique(data_colony$colony_eel_ID)
  
  #grid of all eels
  pairs <- expand.grid(initator = all_eels, responder = all_eels)
  
  #remove rows where inititor and responder are same
  pairs <- subset(pairs, initator != responder)
  pairs$weight <- 0
  
  for (cc in 1:nrow(pairs)) {
  weight <- 0
    #count up the number of drops in which i initated and j was present (a)
    filter_rows <- data_colony %>%
      group_by(drop_ID) %>%
      #filter(colony_eel_ID == pairs$initator[cc] | colony_eel_ID == pairs$responder[cc]) %>% #only the rows where it is initiator or responder
      filter( #only where first responder is initator and responder is not NA
        any(colony_eel_ID == pairs$initator[cc] & rank_order == 1) &
        any(colony_eel_ID == pairs$responder[cc] & emerged == 1)
      ) %>%
      ungroup()
        
    n_qual <- 0
    
    if (nrow(filter_rows) != 0) {
      #count the number of qualifying drops
      n_qual <- length(unique(filter_rows$drop_ID))
      #did the responder respond
      n_response <- filter_rows %>%
        group_by(drop_ID) %>%
        summarise(j_third = any(colony_eel_ID == pairs$responder[cc] &
                                   second_responder == 1)) %>%
        summarise(sum(j_third)) %>%
        pull()
      weight <- n_response/n_qual
    } else {
      weight <- NA

    }
    
    
    pairs$n_qual[cc] <- n_qual
    pairs$weight[cc] <- weight
  }
  
  pairs_all[[c]] <- pairs  
}

pairs_df <- bind_rows(pairs_all)
colnames(pairs_df) <- c("X1","X2","weight","n_qual")

colony_distances 
    
# Join each direction separately then bind
final <- bind_rows(
  left_join(pairs_df, colony_distances_all, by = c("X1", "X2")),
  #left_join(pairs_df, colony_distances_all, by = c("X1" = "X2", "X2" = "X1"))
) %>% 
  distinct() %>%
  filter(n_qual > 0)  # remove pairs with no distance (never co-occurred in trials)

final %>%
  ggplot(aes(x = dist, y = weight, color = n_qual)) +
  geom_point(alpha =0.8) +
  geom_smooth(
  )

#checking directed 
pairs_df %>%
  rename(i = X1, j = X2) %>%
  inner_join(
    pairs_df %>% rename(i = X2, j = X1, weight_reverse = weight, n_qual_reverse = n_qual),
    by = c("i", "j")
  ) %>%
  mutate(symmetric = weight == weight_reverse) %>%
  summarise(prop_symmetric = mean(symmetric, na.rm = TRUE))

#Undirected empirical weight strengths for initator and responder
colony_distances_all <- bind_rows(colony_distances)

pairs_all <- vector(mode="list", length = length(unique(data$colony)))
for (c in 1:length(unique(data$colony))) {
  
  data_colony <- data %>%
    filter(colony == unique(data$colony)[c])
  
  all_eels <- unique(data_colony$colony_eel_ID)
  
  # unordered pairs only
  pairs <- expand.grid(eel_a = all_eels, eel_b = all_eels) %>%
    filter(as.character(eel_a) < as.character(eel_b))
  
  pairs$weight <- NA
  pairs$n_qual <- 0
  
  for (cc in 1:nrow(pairs)) {
    
    filter_rows <- data_colony %>%
      group_by(drop_ID) %>%
      filter(
        (any(colony_eel_ID == pairs$eel_a[cc] & rank_order == 1) &
           any(colony_eel_ID == pairs$eel_b[cc] & emerged == 1))
        |
          (any(colony_eel_ID == pairs$eel_b[cc] & rank_order == 1) &
             any(colony_eel_ID == pairs$eel_a[cc] & emerged == 1))
      ) %>%
      ungroup()
    
    if (nrow(filter_rows) != 0) {
      n_qual <- length(unique(filter_rows$drop_ID))
      n_response <- filter_rows %>%
        group_by(drop_ID) %>%
        summarise(responded = any(
          # a initiated, did b respond second?
          (any(colony_eel_ID == pairs$eel_a[cc] & rank_order == 1) &
             any(colony_eel_ID == pairs$eel_b[cc] & second_responder == 1))
          |
            # b initiated, did a respond second?
            (any(colony_eel_ID == pairs$eel_b[cc] & rank_order == 1) &
               any(colony_eel_ID == pairs$eel_a[cc] & second_responder == 1))
        )) %>%
        summarise(sum(responded)) %>%
        pull()
      pairs$weight[cc] <- n_response / n_qual
      pairs$n_qual[cc] <- n_qual
    }
  }
  
  pairs_all[[c]] <- pairs
}

pairs_df <- bind_rows(pairs_all)
colnames(pairs_df) <- c("X1", "X2", "weight", "n_qual")

#Plotting
final <- pairs_df %>%
  mutate(
    key1 = pmin(as.character(X1), as.character(X2)),
    key2 = pmax(as.character(X1), as.character(X2))
  ) %>%
  left_join(
    colony_distances_all %>%
      mutate(
        key1 = pmin(as.character(X1), as.character(X2)),
        key2 = pmax(as.character(X1), as.character(X2))
      ),
    by = c("key1", "key2")
  ) %>%
  dplyr::select(-key1, -key2) %>%
  distinct() %>%
  filter(n_qual > 0)

final %>%
  ggplot(aes(x = dist, y = weight, color = n_qual)) +
  geom_point(alpha = 0.8) +
  geom_smooth()


#Empirical weight strengths for every subseuqent pair
pairs_all <- vector(mode="list", length = length(unique(data$colony)))

for (c in 1:length(unique(data$colony))) {
  #filter out that colony data
  data_colony <- data %>%
    filter(colony == unique(data$colony)[c])
  
  # get the full set of eel IDs for this colony
  all_eels <- unique(data_colony$colony_eel_ID)
  
  #grid of all eels
  pairs <- expand.grid(initiator = all_eels, responder = all_eels)
  
  #remove rows where inititor and responder are same
  pairs <- subset(pairs, initiator != responder)
  pairs$weight <- 0
  
  for (cc in 1:nrow(pairs)) {
    weight <- 0
    #count up the number of drops in which i initated and j was present (a)
    filter_rows <- data_colony %>%
      group_by(drop_ID) %>%
      #filter(colony_eel_ID == pairs$initator[cc] | colony_eel_ID == pairs$responder[cc]) %>% #only the rows where it is initiator or responder
      filter( #only where first responder is initator and responder is not NA
        any(colony_eel_ID == pairs$initiator[cc] & !is.na(full_partial_none)) &
          any(colony_eel_ID == pairs$responder[cc] & !is.na(full_partial_none))
      ) %>%
      ungroup()
    
    n_qual <- 0
    
    if (nrow(filter_rows) != 0) {
      #count the number of qualifying drops
      n_qual <- length(unique(filter_rows$drop_ID))
      #did the responder respond
      n_response <- filter_rows %>%
        arrange(drop_ID, rank_order) %>%
        group_by(drop_ID) %>%
        summarise(j_after_i = any(colony_eel_ID == pairs$initiator[cc] &
                                   lead(colony_eel_ID) == pairs$responder[cc])) %>%
        summarise(sum(j_after_i)) %>%
        pull()
      weight <- n_response/n_qual
    } else {
      weight <- NA
      
    }
    
    
    pairs$n_qual[cc] <- n_qual
    pairs$weight[cc] <- weight
  }
  
  pairs_all[[c]] <- pairs  
}

pairs_df <- bind_rows(pairs_all)
colnames(pairs_df) <- c("X1","X2","weight","n_qual")

# Join each direction separately then bind
final <- bind_rows(
  left_join(pairs_df, colony_distances_all, by = c("X1", "X2")),
  #left_join(pairs_df, colony_distances_all, by = c("X1" = "X2", "X2" = "X1"))
) %>% 
  distinct() %>%
  filter(n_qual > 0)  # remove depending on n_qual

final %>%
  ggplot(aes(x = dist, y = weight, color = n_qual)) +
  geom_jitter(alpha =0.8) #+
  #geom_smooth()

########################################################






##Distributions
hist(data$distance_to_ball[data$first_responder == 1])

hist(data$dist_from_first_resp[data$second_responder == 1])
hist(data$distance_to_ball[data$second_responder == 1])

hist(data$distance_to_ball[data$third_responder == 1])
hist(data$distance_to_ball[data$fourth_responder == 1])

alptheta <- runif(n_eels) #not using right now, fixed

#Frame range of cascades
ranges <- data %>%
  group_by(drop_ID) %>%
  summarise(range = max(response_frame_cam1, na.rm=TRUE) - min(response_frame_cam1, na.rm=TRUE))

max(ranges$range)

max_rate <- 1
dt <- 1
da <- 1
threshold <- 5
tm <- 5
tr <- 5
fractional_contagion_first <- TRUE
fractional_contagion_subs <- TRUE
n_sims <- 10
private_threshold <- 0.01
social_threshold <- 0.7
ball_decay_time_coef <- 0.2
social_decay_time_coef <- 0.1 

data$log_distance_to_ball_sc <- scale(data$log_distance_to_ball)
data$log_inst_topo_dist_from_first_sc <- scale(data$log_inst_topo_dist_from_first)
orig_topo_mean <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:center")
orig_topo_sd   <- attr(initator_responder$log_inst_topo_dist_sc, "scaled:scale")
orig_ball_mean <- attr(data_no_first$log_distance_to_ball_sc, "scaled:center")
orig_ball_sd   <- attr(data_no_first$log_distance_to_ball_sc, "scaled:scale")

param_grids <- expand.grid(threshold = c(1,5), tm = c(10), tr = c(5), fractional_contagion_first = TRUE, fractional_contagion_subs = TRUE)
param_list <- split(param_grids, seq(nrow(param_grids)))
eligble_drops <- 1
#Eventually when model is a function: results <- pmap(param_grids, run_model)

# Social and private information model
social_private_frame_recorder_list <- vector(mode="list", length = length(unique(data$drop_ID)))
names(social_private_frame_recorder_list) <- unique(data$drop_ID)

for (i in unique(data$drop_ID)) {
  
  print(i)
  
  social_private_frame_recorder_list[[i]] <- vector(mode = "list", length = n_sims)
  
  #Calculate which individuals are emerged 
  drop_data <- data %>%
    filter(drop_ID == i & emerged == 1 & !is.na(global_X)) # & !is.na(dist_from_first_resp)
  
  drop_eel_IDs <- unique(drop_data$colony_eel_ID)
  
  if (fractional_contagion_first == TRUE) {
    K_first <- length(drop_eel_IDs)
  } else {
    K_first <- 1
  }
  
  #if (is.na(sum(drop_data$distance_to_ball, na.rm=TRUE))) next
      
  if (length(drop_eel_IDs) < 3) next
  
  eligble_drops <- eligble_drops +1
  
  for (sim in 1:n_sims) {
    
    #create a frame recorder matrix
    social_private_frame_recorder_matrix <- matrix(nrow=length(drop_eel_IDs), dimnames=list(drop_eel_IDs, NULL))
    
    resp_data <- as.data.frame(matrix(nrow=length(drop_eel_IDs),ncol=3))
    
    colony_idx <- which(unique(data$colony) == first(drop_data$colony))
    
    #time step 1
    k <- 1
    
    #determine first responder
    for (h in 1:length(drop_eel_IDs)) {
      l_drop_ID <- first(drop_data$drop_ID)
      l_colony_eel_ID <- drop_data$colony_eel_ID[h]
      l_date <- first(drop_data$date)
      l_colony <- first(drop_data$colony)
      #for each eel i in drop j nested in colony k, compute the linear predictor
      eta_j <- as.numeric(coefs[1]) + as.numeric(coefs[2])*(drop_data$log_distance_to_ball_sc[h]) - ball_decay_time_coef*log(k) #RE removed for now... + fr_re_drop_ID$"(Intercept)"[fr_re_drop_ID$combo == l_drop_ID] + fr_re_colony_colony_eel_ID$"(Intercept)"[as.character(fr_re_colony_colony_eel_ID$name) == l_colony_eel_ID] + fr_re_date$"(Intercept)"[fr_re_date$combo == l_date] + fr_re_colony$"(Intercept)"[fr_re_colony$combo == l_colony]
      #convert this to a standard logistic transform - gives probability per eel
      p_private_cue <- 1/(1+exp(-eta_j))
      resp_data[h,1] <- l_colony_eel_ID
      resp_data[h,2] <- p_private_cue
      resp_data[h,3] <- rbinom(n = 1, size = 1, prob = p_private_cue)
      resp_data[h,4] <- resp_data[h,3]/K_first
      resp_data[h,5] <- ifelse(resp_data[h,4] > private_threshold, 1, 0) #private threshold be on scale between 0 and 1
    }
    
    #create state matrix 
    state_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = 200)
    state_matrix[,1] <- "s"
    #create dosage matrix 
    dosage_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = 200)
    dosage_matrix[,] <- 0
    
    #if there is a first responder
    if (sum(resp_data[,5], na.rm = TRUE) > 0) {
      
      #find IDs of first responder
      fr_ID <- resp_data$V1[resp_data$V5 == 1]
      
      #find index of first responder
      fr_idx <- which(drop_eel_IDs %in% fr_ID)
      
      social_private_frame_recorder_matrix[fr_idx] <- 1
      
      for (fr_IDD in fr_idx) {
        state_matrix[fr_IDD,1] <- "i"
      }

      state_matrix[-fr_idx,1] <- "s"
      
      dosage_matrix[fr_idx,] <- NA
      dosage_matrix[-fr_idx,] <- 0
      
    }
      
      #for each time step 
      for (k in 2:200) {
        if (fractional_contagion_subs == TRUE) {
          K <- sum(state_matrix[,k-1] == "s")
        } else {
          K <- 1
        }
        for (j in 1:length(drop_eel_IDs)) {
          focal_eel_ID <- drop_eel_IDs[j]
          
          #Assigning states
          if (state_matrix[j,k-1] == "r") { #if eel is recovered
            state_matrix[j,k] <- "r"
            dosage_matrix[j,k] <- NA
          } else if (state_matrix[j,k-1] == "i") { #if eel is infected
            dosage_matrix[j,k] <- NA #state and frame recorder matrices stay the same
            frames_since_infected <- k - social_private_frame_recorder_matrix[j]
            if (k-tr <= 0) {
              state_matrix[j,k] <- "i"
              
              #dose everyone
              for (jj in 1:length(drop_eel_IDs)) {
                buddy_eel_ID <- drop_eel_IDs[jj]
                
                if (buddy_eel_ID == focal_eel_ID) next  # skip self
                

                
                buddy_neighbours_ranked <- drop_data$inst_neighbours_topo_ranked[which(drop_data$colony_eel_ID == buddy_eel_ID)]
                rank <- which(buddy_neighbours_ranked[[1]] == focal_eel_ID)
                
                log_inst_topo_dist_sc <- (log(rank) - orig_topo_mean) / orig_topo_sd
                
                eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[4])*log_inst_topo_dist_sc - social_decay_time_coef*log(frames_since_infected)
                p_cue <- 1/(1+exp(-eta_j))
                if (rbinom(1,1,p_cue*max_rate*dt) == 1) {
                  dosage_matrix[jj,k] <- dosage_matrix[jj,k] + da
                } else {
                  
                }
              }
            } else { 
              if (state_matrix[j,k-tr] == "i") {
                state_matrix[j,k] <- "r"
              } else {
                state_matrix[j,k] <- "i"
                
                #dose everyone
                for (jj in 1:length(drop_eel_IDs)) {
                  buddy_eel_ID <- drop_eel_IDs[jj]
                  
                  if (buddy_eel_ID == focal_eel_ID) next  # skip self
                  
                  buddy_neighbours_ranked <- drop_data$inst_neighbours_topo_ranked[which(drop_data$colony_eel_ID == buddy_eel_ID)]
                  rank <- which(buddy_neighbours_ranked[[1]] == focal_eel_ID)
                  log_inst_topo_dist_sc <- (log(rank) - orig_topo_mean) / orig_topo_sd
                  eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[4])*log_inst_topo_dist_sc - social_decay_time_coef*log(frames_since_infected)
                  p_cue <- 1/(1+exp(-eta_j))
                  
                  if (rbinom(1,1,p_cue*max_rate*dt) == 1) {
                    dosage_matrix[jj,k] <- dosage_matrix[jj,k] + da
                  } else {
                    
                  }
                }
              }
            }
          } else { #eel is susceptible to hide
            
            if (K == 0) {
              private_response <- 0
              social_response <- 0
            } else {
            #Check if responds to private cue of the ball, just delayed
            eta_j <-  as.numeric(coefs[1]) + as.numeric(coefs[2])*(drop_data$log_distance_to_ball_sc[drop_data$colony_eel_ID == focal_eel_ID]) - ball_decay_time_coef*log(k) #Interecept (let's just fit it with the threshold) and RE removed for now... + fr_re_drop_ID$"(Intercept)"[fr_re_drop_ID$combo == l_drop_ID] + fr_re_colony_colony_eel_ID$"(Intercept)"[as.character(fr_re_colony_colony_eel_ID$name) == l_colony_eel_ID] + fr_re_date$"(Intercept)"[fr_re_date$combo == l_date] + fr_re_colony$"(Intercept)"[fr_re_colony$combo == l_colony]
            #convert this to a standard logistic transform - gives probability per eel
            p_private_cue <- 1/(1+exp(-eta_j))
            private_cue_received <- rbinom(n = 1, size = 1, prob = p_private_cue) 
            private_response <- ifelse(private_cue_received/K > private_threshold, 1, 0) #private threshold be on scale between 0 and 1
            
            cuml_dose <- 0
            
            if (k <= 5) {
              social_response <- 0
            } else {
              window_end <- k - 5
              window_start <- max(1, window_end - tm)
              cuml_dose <- sum(dosage_matrix[j, window_start:window_end], na.rm=TRUE)
              #Check if responds to social cues
              norm_cuml_dose <- cuml_dose/K
              social_response <- ifelse(norm_cuml_dose > social_threshold, 1, 0)
            }
          }
            if (!is.na(social_response) & !is.na(private_response)) {
              if (social_response == 1 | private_response == 1) {
                state_matrix[j,k] <- "i"
                social_private_frame_recorder_matrix[j] <- k
              } else {
                state_matrix[j,k] <- "s"
              }
            } else {
              state_matrix[j,k] <- "s"
            }
            }
        }
      }
  }
    social_private_frame_recorder_list[[i]][[sim]] <- social_private_frame_recorder_matrix
}



#Next step 
# - make the non social model look like the social model - done
# - make social only model thatthey only respond to social info after the first responder (first model is social + private) - done
# - make the 5 frame delay processing. First responders can respond in any of the first 5 frames. Subsequent responders only start responding from frame 5 and look to their dosage 5 frames previous from the current frame. 

## Private-only model ##
n_sims <- 10
non_social_frame_recorder_list <- vector(mode="list", length = length(unique(data$drop_ID)))
names(non_social_frame_recorder_list) <- unique(data$drop_ID)

for (i in unique(data$drop_ID)) {
  
  print(i)
  
  non_social_frame_recorder_list[[i]] <- vector(mode = "list", length = n_sims)
  
  #Calculate which individuals are emerged 
  drop_data <- data %>%
    filter(drop_ID == i & emerged == 1 & !is.na(global_X)) 
  
  drop_eel_IDs <- unique(drop_data$colony_eel_ID)
  
  if (length(drop_eel_IDs) < 3) next
  
  for (sim in 1:n_sims) {
    
    #create a frame recorder matrix
    non_social_frame_recorder_matrix <- matrix(nrow=length(drop_eel_IDs), dimnames=list(drop_eel_IDs, NULL))   
    
    resp_data <- as.data.frame(matrix(nrow=length(drop_eel_IDs),ncol=3))
    
    colony_idx <- which(unique(data$colony) == first(drop_data$colony))
  
    #determine first responder
    for (h in 1:length(drop_eel_IDs)) {
      l_drop_ID <- first(drop_data$drop_ID)
      l_colony_eel_ID <- drop_data$colony_eel_ID[h]
      l_date <- first(drop_data$date)
      l_colony <- first(drop_data$colony)
      #for each eel i in drop j nested in colony k, compute the linear predictor
      eta_j <- as.numeric(coefs[1]) + as.numeric(coefs[2])*(drop_data$log_distance_to_ball[h])
      #convert this to a standard logistic transform - gives probability per eel
      p_private_cue <- 1/(1+exp(-eta_j))
      resp_data[h,1] <- l_colony_eel_ID
      resp_data[h,2] <- p_private_cue
      resp_data[h,3] <- rbinom(n = 1, size = 1, prob = p_private_cue)
      resp_data[h,4] <- resp_data[h,3]
      resp_data[h,5] <- ifelse(resp_data[h,4] > private_threshold, 1, 0) #private threshold be on scale between 0 and 1
    }
    
    #if there is a first responder
    if (sum(resp_data[,5], na.rm = TRUE) > 0) {
      
      #find IDs of first responder
      fr_ID <- resp_data$V1[resp_data$V5 == 1]
      
      #find index of first responder
      fr_idx <- which(drop_eel_IDs %in% fr_ID)
      
      non_social_frame_recorder_matrix[fr_idx] <- 1
      
      #create state matrix 
      state_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = 200)
      state_matrix[fr_idx,1] <- "i"
      state_matrix[-fr_idx,1] <- "s"
      
      #for each time step
      for (k in 2:30) {
        for (j in 1:length(drop_eel_IDs)) {
          focal_eel_ID <- drop_eel_IDs[j]
          
          if (state_matrix[j,k-1] == "r") { #if eel is recovered
            state_matrix[j,k] <- "r"
          } else if (state_matrix[j,k-1] == "i") { #if eel is infected
            if (k-tr <= 0) {
          state_matrix[j,k] <- "i"
            } else {
              if (state_matrix[j,k-tr] == "i") {
                state_matrix[j,k] <- "r"
              } else {
                state_matrix[j,k] <- "i"
              }
            }
          } else { #eel is susceptible to hide
              #check if eel responds to private cue of the ball, just delayed
            eta_j <- as.numeric(coefs[1]) + as.numeric(coefs[2])*(drop_data$log_distance_to_ball[drop_data$colony_eel_ID == focal_eel_ID]) - ball_decay_time_coef*log(k)
            p_private_cue <- 1/(1+exp(-eta_j))
            private_cue_received <- rbinom(n = 1, size = 1, prob = p_private_cue) 
            private_response <- ifelse(private_cue_received/K > private_threshold, 1, 0) #private threshold be on scale between 0 and 1
            
            if (private_response == 1) {
              state_matrix[j,k] <- "i"
              non_social_frame_recorder_matrix[j] <- k
            } else {
              state_matrix[j,k] <- "s"
            }
            }
            }
      }
    
    non_social_frame_recorder_list[[i]][[sim]] <- non_social_frame_recorder_matrix
    }
  }
}

## Social-only model ##
social_frame_recorder_list <- vector(mode="list", length = length(unique(data$drop_ID)))
names(social_frame_recorder_list) <- unique(data$drop_ID)

for (i in unique(data$drop_ID)) {
  
  print(i)
  
  social_frame_recorder_list[[i]] <- vector(mode = "list", length = n_sims)
  
  #Calculate which individuals are emerged 
  drop_data <- data %>%
    filter(drop_ID == i & emerged == 1 & !is.na(global_X)) # & !is.na(dist_from_first_resp)
  
  drop_eel_IDs <- unique(drop_data$colony_eel_ID)
  
  if (fractional_contagion_first == TRUE) {
    K_first <- length(drop_eel_IDs)
  } else {
    K_first <- 1
  }
  
  if (length(drop_eel_IDs) < 3) next
  
  for (sim in 1:n_sims) {
    
    #create a frame recorder matrix
    social_frame_recorder_matrix <- matrix(nrow=length(drop_eel_IDs), dimnames=list(drop_eel_IDs, NULL))
    
    resp_data <- as.data.frame(matrix(nrow=length(drop_eel_IDs),ncol=3))
    
    colony_idx <- which(unique(data$colony) == first(drop_data$colony))
    
    #time step 1
    k <- 1
    
    #determine first responder
    for (h in 1:length(drop_eel_IDs)) {
      l_drop_ID <- first(drop_data$drop_ID)
      l_colony_eel_ID <- drop_data$colony_eel_ID[h]
      l_date <- first(drop_data$date)
      l_colony <- first(drop_data$colony)
      #for each eel i in drop j nested in colony k, compute the linear predictor
      eta_j <- as.numeric(coefs[1]) + as.numeric(coefs[2])*(drop_data$log_distance_to_ball[h]) - ball_decay_time_coef*log(k) #RE removed for now... + fr_re_drop_ID$"(Intercept)"[fr_re_drop_ID$combo == l_drop_ID] + fr_re_colony_colony_eel_ID$"(Intercept)"[as.character(fr_re_colony_colony_eel_ID$name) == l_colony_eel_ID] + fr_re_date$"(Intercept)"[fr_re_date$combo == l_date] + fr_re_colony$"(Intercept)"[fr_re_colony$combo == l_colony]
      #convert this to a standard logistic transform - gives probability per eel
      p_private_cue <- 1/(1+exp(-eta_j))
      resp_data[h,1] <- l_colony_eel_ID
      resp_data[h,2] <- p_private_cue
      resp_data[h,3] <- rbinom(n = 1, size = 1, prob = p_private_cue)
      resp_data[h,4] <- resp_data[h,3]/K_first
      resp_data[h,5] <- ifelse(resp_data[h,4] > private_threshold, 1, 0) #private threshold be on scale between 0 and 1
      
    }
    
    #if there is a first responder
    if (sum(resp_data[,5], na.rm = TRUE) > 0) {
      
      #find IDs of first responder
      fr_ID <- resp_data$V1[resp_data$V5 == 1]
      
      #find index of first responder
      fr_idx <- which(drop_eel_IDs %in% fr_ID)
      
      social_frame_recorder_matrix[fr_idx] <- 1
      
      #create state matrix 
      state_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = 200)
      state_matrix[fr_idx,1] <- "i"
      state_matrix[-fr_idx,1] <- "s"
      
      #create dosage matrix 
      dosage_matrix <- matrix(nrow=length(drop_eel_IDs), ncol = 200)
      dosage_matrix[fr_idx,] <- NA
      dosage_matrix[-fr_idx,] <- 0
      
      #for each time step 
      for (k in 2:30) {
        if (fractional_contagion_subs == TRUE) {
          K <- sum(state_matrix[,k-1] == "s")
        } else {
          K <- 1
        }
        for (j in 1:length(drop_eel_IDs)) {
          focal_eel_ID <- drop_eel_IDs[j]
          
          #Assigning states
          if (state_matrix[j,k-1] == "r") { #if eel is recovered
            state_matrix[j,k] <- "r"
            dosage_matrix[j,k] <- NA
          }
          else if (state_matrix[j,k-1] == "i") { #if eel is infected
            dosage_matrix[j,k] <- NA #state and frame recorder matrices stay the same
            frames_since_infected <- k - social_frame_recorder_matrix[j]
            if (k-tr <= 0) {
              state_matrix[j,k] <- "i"
              
              #dose everyone
              for (jj in 1:length(drop_eel_IDs)) {
                buddy_eel_ID <- drop_eel_IDs[jj]
                
                if (buddy_eel_ID == focal_eel_ID) next  # skip self
                
                buddy_neighbours_ranked <- drop_data$inst_neighbours_topo_ranked[which(drop_data$colony_eel_ID == buddy_eel_ID)]
                rank <- which(buddy_neighbours_ranked[[1]] == focal_eel_ID)
                eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[4])*log(rank) - social_decay_time_coef*log(frames_since_infected)
                p_cue <- 1/(1+exp(-eta_j))
                
                if (rbinom(1,1,p_cue*max_rate*dt) == 1) {
                  dosage_matrix[jj,k] <- dosage_matrix[jj,k] + da
                } else {
                  
                }
              }
            } else { 
              if (state_matrix[j,k-tr] == "i") {
                state_matrix[j,k] <- "r"
              } else {
                state_matrix[j,k] <- "i"
                
                #dose everyone
                for (jj in 1:length(drop_eel_IDs)) {
                  buddy_eel_ID <- drop_eel_IDs[jj]
                  
                  if (buddy_eel_ID == focal_eel_ID) next  # skip self
                  
                  buddy_neighbours_ranked <- drop_data$inst_neighbours_topo_ranked[which(drop_data$colony_eel_ID == buddy_eel_ID)]
                  rank <- which(buddy_neighbours_ranked[[1]] == focal_eel_ID)
                  eta_j <- as.numeric(coefs[3]) + as.numeric(coefs[4])*log(rank) - social_decay_time_coef*log(frames_since_infected)
                  p_cue <- 1/(1+exp(-eta_j))
                  
                  if (rbinom(1,1,p_cue*max_rate*dt) == 1) {
                    dosage_matrix[jj,k] <- dosage_matrix[jj,k] + da
                  } else {
                    
                  }
                }
              }
            }
          } else { #eel is susceptible to hide

            #Check if responds to social cues
            cuml_dose <- sum(dosage_matrix[j, 1:(k-1)], na.rm=TRUE)
            norm_cuml_dose <- cuml_dose/K
            
            social_response <- ifelse(norm_cuml_dose > social_threshold, 1, 0)
            
            if (social_response == 1) {
              state_matrix[j,k] <- "i"
              social_frame_recorder_matrix[j] <- k
              
              #Dosing only starts next timestep
              
            } else {
              state_matrix[j,k] <- "s"
            }
          }
        }
      }
    }
    social_frame_recorder_list[[i]][[sim]] <- social_frame_recorder_matrix
  }
}




#Response vs no confusion matrix 
library(tidyverse)
library(ggplot2)

# --- Observed: did any eel respond in this drop? ---
observed_drop <- data %>%
  filter(!is.na(full_partial_none) & !is.na(base_x_cam1) & !is.na(base_x_cam2)) %>%
  group_by(drop_ID) %>%
  summarise(obs_responded = any(!is.na(response_frame_cam1)), .groups = "drop") %>%
  mutate(drop_ID = as.character(drop_ID))

# --- Helper: one prediction per drop per sim ---
get_confusion_counts <- function(recorder_list, observed_df, n_sims) {
  
  rows <- list()
  
  for (i in seq_along(recorder_list)) {
    drop_id <- names(recorder_list)[i]
    sims    <- recorder_list[[i]]
    
    obs <- observed_df$obs_responded[observed_df$drop_ID == drop_id]
    if (length(obs) == 0) next
    
    for (sim in sims) {
      pred_responded <- any(!is.na(sim))
      
      if (obs & pred_responded)   category <- "True Positive"
      if (obs & !pred_responded)  category <- "False Negative"
      if (!obs & pred_responded)  category <- "False Positive"
      if (!obs & !pred_responded) category <- "True Negative"
      
      rows <- append(rows, list(data.frame(
        drop_ID  = as.character(drop_id),
        category = as.character(category)
      )))
    }
  }
  
  bind_rows(rows) %>%
    group_by(category) %>%
    summarise(total_count = n(), .groups = "drop")
}

# --- Compute for both models ---
social_counts     <- get_confusion_counts(social_frame_recorder_list,     observed_drop, n_sims)
non_social_counts <- get_confusion_counts(non_social_frame_recorder_list, observed_drop, n_sims)

social_counts$model     <- "Social"
non_social_counts$model <- "Non-Social"

# --- Ensure all four categories present for both models ---
all_categories <- c("True Positive", "False Negative", "False Positive", "True Negative")

all_counts <- bind_rows(social_counts, non_social_counts) %>%
  complete(model, category = all_categories, fill = list(total_count = 0)) %>%
  mutate(
    x_label = case_when(
      category %in% c("True Positive", "False Negative") ~ "Observed:\nResponded",
      TRUE                                                ~ "Observed:\nDid Not Respond"
    ),
    y_label = case_when(
      category %in% c("True Positive", "False Positive") ~ "Predicted:\nResponded",
      TRUE                                                ~ "Predicted:\nDid Not Respond"
    ),
    x_label = factor(x_label, levels = c("Observed:\nResponded", "Observed:\nDid Not Respond")),
    y_label = factor(y_label, levels = c("Predicted:\nResponded", "Predicted:\nDid Not Respond"))
  ) %>%
  group_by(model, x_label) %>%
  mutate(proportion = total_count / sum(total_count)) %>%
  ungroup()

# --- Plot ---
ggplot(all_counts, aes(x = x_label, y = y_label, fill = proportion)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = paste0(category, "\n", scales::percent(proportion, accuracy = 1))),
            size = 3.5, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#d1e5f0", high = "#2166ac",
                      labels = scales::percent,
                      name = "Proportion\nof observed") +
  facet_wrap(~model, ncol = 2) +
  labs(
    title = "Model Prediction Accuracy vs Observed Responses",
    x = "Observed",
    y = "Predicted"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid      = element_blank(),
    strip.text      = element_text(face = "bold", size = 14),
    axis.text       = element_text(size = 11),
    legend.position = "right"
  )

#Cascade vs single response
# --- Observed: among drops with a response, single vs cascade ---
observed_drop_cascade <- data %>%
  filter(!is.na(full_partial_none) & !is.na(base_x_cam1) & !is.na(base_x_cam2)) %>%
  group_by(drop_ID) %>%
  summarise(n_responded = sum(!is.na(response_frame_cam1)), .groups = "drop") %>%
  filter(n_responded > 0) %>%  # only drops where someone responded
  mutate(
    drop_ID = as.character(drop_ID),
    obs_cascade = n_responded > 1
  )

# --- Helper ---
get_confusion_counts_cascade <- function(recorder_list, observed_df, n_sims) {
  
  rows <- list()
  
  for (i in seq_along(recorder_list)) {
    drop_id <- names(recorder_list)[i]
    sims    <- recorder_list[[i]]
    
    obs <- observed_df$obs_cascade[observed_df$drop_ID == drop_id]
    if (length(obs) == 0) next  # skips drops with no observed response
    
    for (sim in sims) {
      pred_cascade <- sum(!is.na(sim)) > 1
      
      if (obs & pred_cascade)   category <- "True Positive"
      if (obs & !pred_cascade)  category <- "False Negative"
      if (!obs & pred_cascade)  category <- "False Positive"
      if (!obs & !pred_cascade) category <- "True Negative"
      
      rows <- append(rows, list(data.frame(drop_ID  = as.character(drop_id),
                                           category = as.character(category))))
    }
  }
  
  bind_rows(rows) %>%
    group_by(category) %>%
    summarise(total_count = n(), .groups = "drop")
}

# --- Compute for both models ---
social_counts_cascade     <- get_confusion_counts_cascade(social_frame_recorder_list,     observed_drop_cascade, n_sims)
non_social_counts_cascade <- get_confusion_counts_cascade(non_social_frame_recorder_list, observed_drop_cascade, n_sims)

social_counts_cascade$model     <- "Social"
non_social_counts_cascade$model <- "Non-Social"

# --- Ensure all four categories present ---
all_categories <- c("True Positive", "False Negative", "False Positive", "True Negative")

all_counts_cascade <- bind_rows(social_counts_cascade, non_social_counts_cascade) %>%
  complete(model, category = all_categories, fill = list(total_count = 0)) %>%
  mutate(
    x_label = case_when(
      category %in% c("True Positive", "False Negative") ~ "Observed:\nCascade",
      TRUE                                                ~ "Observed:\nSingle Response"
    ),
    y_label = case_when(
      category %in% c("True Positive", "False Positive") ~ "Predicted:\nCascade",
      TRUE                                                ~ "Predicted:\nSingle Response"
    ),
    x_label = factor(x_label, levels = c("Observed:\nCascade", "Observed:\nSingle Response")),
    y_label = factor(y_label, levels = c("Predicted:\nCascade", "Predicted:\nSingle Response"))
  ) %>%
  group_by(model, x_label) %>%
  mutate(proportion = total_count / sum(total_count)) %>%
  ungroup()

# --- Plot ---
ggplot(all_counts_cascade, aes(x = x_label, y = y_label, fill = proportion)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = paste0(category, "\n", scales::percent(proportion, accuracy = 1))),
            size = 3.5, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#d1e5f0", high = "#2166ac",
                      labels = scales::percent,
                      name = "Proportion\nof observed") +
  facet_wrap(~model, ncol = 2) +
  labs(
    title = "Cascade vs Single Response Prediction\n(drops with at least one responder only)",
    x = "Observed",
    y = "Predicted"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid      = element_blank(),
    strip.text      = element_text(face = "bold", size = 14),
    axis.text       = element_text(size = 11),
    legend.position = "right"
  )

#social vs non social difference in number of responders
# --- Observed: mean number of responders per drop ---
observed_cascade_size <- data %>%
  filter(!is.na(full_partial_none) & !is.na(base_x_cam1) & !is.na(base_x_cam2)) %>%
  group_by(drop_ID) %>%
  summarise(n_responded = sum(!is.na(response_frame_cam1)), .groups = "drop") %>%
  mutate(drop_ID = as.character(drop_ID))

# --- Helper: mean responders per sim, then difference to observed ---
get_cascade_size_diff <- function(recorder_list, observed_df, n_sims, model_name) {
  
  rows <- list()
  
  for (i in seq_along(recorder_list)) {
    drop_id <- names(recorder_list)[i]
    sims    <- recorder_list[[i]]
    
    obs_n <- observed_df$n_responded[observed_df$drop_ID == drop_id]
    if (length(obs_n) == 0) next
    
    # mean number of responders across sims for this drop
    mean_pred_n <- mean(sapply(sims, function(sim) sum(!is.na(sim))))
    
    rows <- append(rows, list(data.frame(
      drop_ID    = as.character(drop_id),
      obs_n      = obs_n,
      pred_n     = mean_pred_n,
      difference = mean_pred_n - obs_n,
      model      = model_name
    )))
  }
  
  bind_rows(rows)
}

# --- Compute for both models ---
social_diff     <- get_cascade_size_diff(social_frame_recorder_list,     observed_cascade_size, n_sims, "Social")
non_social_diff <- get_cascade_size_diff(non_social_frame_recorder_list, observed_cascade_size, n_sims, "Non-Social")

all_diff <- bind_rows(social_diff, non_social_diff) %>%
  mutate(model = factor(model, levels = c("Social", "Non-Social")))

# --- Plot ---
ggplot(all_diff, aes(x = model, y = difference, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8, width = 0.5) +
  scale_fill_manual(values = c("Social" = "#2166ac", "Non-Social" = "#E57373")) +
  labs(
    title = "Predicted vs Observed Cascade Size",
    subtitle = "Difference = mean predicted responders - observed responders per drop",
    x = "",
    y = "Difference in number of responders\n(predicted - observed)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

#Timing
# --- Observed: frame number relative to first responder ---
observed_timing <- data %>%
  filter(!is.na(full_partial_none) & !is.na(base_x_cam1) & !is.na(base_x_cam2) & 
           !is.na(response_frame_cam1)) %>%
  group_by(drop_ID) %>%
  mutate(
    first_frame    = min(response_frame_cam1, na.rm = TRUE),
    relative_frame = response_frame_cam1 - first_frame
  ) %>%
  ungroup() %>%
  mutate(drop_ID = as.character(drop_ID),
         source  = "Observed") %>%
  filter(relative_frame < 200)

# --- Predicted: extract frame numbers from social model, relative to first responder ---
sim_timing_rows <- list()

for (i in seq_along(social_frame_recorder_list)) {
  drop_id <- names(social_frame_recorder_list)[i]
  sims    <- social_frame_recorder_list[[i]]
  
  for (sim_idx in seq_along(sims)) {
    sim <- sims[[sim_idx]]
    
    frames <- sim[!is.na(sim)]
    
    if (length(frames) == 0) next
    
    first_frame <- min(frames)
    
    for (f in frames) {
      sim_timing_rows <- append(sim_timing_rows, list(data.frame(
        drop_ID        = as.character(drop_id),
        sim_idx        = sim_idx,
        relative_frame = f - first_frame,
        source         = "Social Model"
      )))
    }
  }
}

sim_timing <- bind_rows(sim_timing_rows)

# --- Combine ---
all_timing <- bind_rows(
  observed_timing %>% dplyr::select(drop_ID, relative_frame, source),
  sim_timing      %>% dplyr::select(drop_ID, relative_frame, source)
) %>%
  mutate(source = factor(source, levels = c("Observed", "Social Model")))

# --- Plot ---
ggplot(all_timing, aes(x = source, y = relative_frame, fill = source)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8, width = 0.5) +
  scale_fill_manual(values = c("Observed" = "#4CAF50", "Social Model" = "#2166ac")) +
  labs(
    title = "Timing of Individual Responses",
    subtitle = "Frame number relative to first responder (0 = first responder)",
    x = "",
    y = "Relative response frame"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank()
  )


#Time series subset
library(tidyverse)
library(ggplot2)

DROP_SUBSET <- unique(data$drop_ID)[1:8]

# ── Max frame from subset only ───────────────────────────────────────────────
max_frame <- data %>%
  filter(drop_ID %in% DROP_SUBSET,
         !is.na(response_frame_cam1)) %>%
  group_by(drop_ID) %>%
  mutate(first_frame    = min(response_frame_cam1, na.rm = TRUE),
         relative_frame = response_frame_cam1 - first_frame + 1) %>%
  ungroup() %>%
  summarise(max_rf = max(relative_frame, na.rm = TRUE)) %>%
  pull(max_rf)

# ── Observed (per frame) ─────────────────────────────────────────────────────
obs_long <- data %>%
  filter(drop_ID %in% DROP_SUBSET,
         !is.na(full_partial_none),
         !is.na(base_x_cam1),
         !is.na(base_x_cam2),
         !is.na(response_frame_cam1)) %>%
  group_by(drop_ID) %>%
  mutate(first_frame    = min(response_frame_cam1, na.rm = TRUE),
         relative_frame = response_frame_cam1 - first_frame + 1) %>%
  ungroup() %>%
  group_by(drop_ID, relative_frame) %>%
  summarise(n_hides = n_distinct(colony_eel_ID), .groups = "drop") %>%
  mutate(model = "Observed")

# ── Social model (per frame, no scaling) ─────────────────────────────────────
social_long <- map_dfr(DROP_SUBSET, function(did) {
  sims <- social_frame_recorder_list[[as.character(did)]]
  if (is.null(sims)) return(NULL)
  
  map_dfr(seq_along(sims), function(s) {
    sim <- sims[[s]]
    if (is.null(sim) || all(is.na(sim))) return(NULL)
    
    frames <- sim[!is.na(sim)]
    first  <- min(frames)
    
    tibble(
      drop_ID = did,
      relative_frame = (frames - first + 1),
      sim = s
    )
  })
}) %>%
  group_by(drop_ID, relative_frame, sim) %>%
  summarise(n_hides = n(), .groups = "drop") %>%
  group_by(drop_ID, relative_frame) %>%
  summarise(n_hides = mean(n_hides), .groups = "drop") %>%
  mutate(model = "Social model")

# ── Combine and fill missing frames ──────────────────────────────────────────
plot_data <- bind_rows(obs_long, social_long) %>%
  complete(drop_ID = DROP_SUBSET,
           relative_frame = 1:max_frame,
           model = c("Observed", "Social model"),
           fill = list(n_hides = 0)) %>%
  mutate(model = factor(model, levels = c("Observed", "Social model")))

# ── Plot ─────────────────────────────────────────────────────────────────────
ggplot(plot_data, aes(x = relative_frame,
                      y = factor(drop_ID),
                      fill = n_hides)) +
  geom_tile() +
  facet_grid(model ~ ., scales = "free_y", space = "free_y") +
  scale_fill_gradient(
    low  = "#e8f4f8",
    high = "#042c53",
    name = "Eels hiding"
  ) +
  scale_x_continuous(
    name = "Frame (relative to first responder)",
    breaks = seq(0, max_frame, by = 10),  # adjust spacing if needed
    expand = c(0, 0)
  ) +
  labs(y = "Drop ID") +
  theme_minimal(base_size = 12) +
  theme(
    strip.text      = element_text(face = "bold"),
    panel.grid      = element_blank(),
    panel.spacing.y = unit(0.8, "lines")
  )
