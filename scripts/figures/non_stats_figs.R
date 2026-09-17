f1 <- data_clean_fr_real %>%
  ggplot(aes(x = time_lag_since_first)) +
  geom_histogram(aes(y= stat(count/sum(count))), bins = 20, fill = "#330033") +
  labs(x = "Time lag after first responder (1/60th sec)", 
       y = "Density")  +
  theme_classic(base_size = 40) +
  theme()
  
ggsave(f1, filename="/Users/ellag/Desktop/PhD/academic_projects/ball_drop_eel/manuscipt/figures/time_lag_hist.png", width = 12, height = 8)

drop2 <- data_clean_fr_real %>%
  filter(drop_ID == 2)

plot(drop2$response_frame_cam1)
