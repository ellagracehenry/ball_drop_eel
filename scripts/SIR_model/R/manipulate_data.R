manipulate_data <- function(data) {
  
  
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

  #Incorporating time lag. 
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
  
  return(data)

}