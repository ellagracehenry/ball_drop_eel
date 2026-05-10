drop_distances_long <- data %>%
  group_by(colony, trial_ID, drop_ID, colony_eel_ID) %>%
  summarise(
    base_X = mean(base_X, na.rm = TRUE),
    base_Y = mean(base_Y, na.rm = TRUE),
    base_Z = mean(base_Z, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(colony, trial_ID, drop_ID) %>%
  group_modify(~{
    coords <- .x[, c("base_X", "base_Y", "base_Z")]
    rownames(coords) <- .x$colony_eel_ID
    
    if (nrow(coords) < 2) return(data.frame())
    
    dist_mat <- as.matrix(dist(coords))
    
    xy <- t(combn(rownames(dist_mat), 2))
    data.frame(
      eel1 = xy[,1],
      eel2 = xy[,2],
      dist = dist_mat[xy]
    )
  }) %>%
  ungroup() %>%
  mutate(pair = paste(eel1, eel2, sep = " — "))

# Convert your trial_distances list to long format first
drop_distances_long <- data %>%
  group_by(colony, trial_ID, drop_ID) %>%
  group_modify(~{
    coords <- .x[, c("base_X", "base_Y", "base_Z")]
    rownames(coords) <- .x$colony_eel_ID
    
    if (nrow(coords) < 2) return(data.frame())
    
    dist_mat <- as.matrix(dist(coords))
    
    # to long format, upper triangle only
    xy <- t(combn(rownames(dist_mat), 2))
    data.frame(
      eel1 = xy[,1],
      eel2 = xy[,2],
      dist = dist_mat[xy]
    )
  }) %>%
  ungroup() %>%
  mutate(pair = paste(eel1, eel2, sep=" — "))

drop_distances_long$trial_ID <- as.factor(drop_distances_long$trial_ID)
drop_distances_long %>%
  filter(colony == "S5") %>%
  ggplot(aes(x = trial_ID, y = dist, color = trial_ID)) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) +
  geom_boxplot(alpha = 0.2, outlier.shape = NA) +
  facet_wrap(~pair) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = "Trial", y = "Distance (m)",
       title = "Pairwise distances across drops per trial")

drop_distances_long %>%
  filter(colony == "S5", !is.na(dist)) %>%
  group_by(pair) %>%
  summarise(n_trials = n_distinct(trial_ID)) %>%
  filter(n_trials > 1) %>%
  arrange(desc(n_trials))

drop_distances_long %>%
  filter(colony == "S5", pair == "11_S5 — 25_S5") %>%
  ggplot(aes(x = drop_ID, y = dist, color = trial_ID, group = trial_ID)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(x = "Drop", y = "Distance (m)", title = "Distance: 1_S5 — 2_S5")
