library(dplyr)
library(ggplot2)

sp <- cbind("Social and private cues", -254.3866342)
s <- cbind("Social cues only", -255.3119)
p <- cbind("Private cues only", -264.540785010046)
#add in no K?

ll <- as.data.frame(rbind(sp,s,p))
ll$V1 <- as.factor(ll$V1)

ll <- ll %>%
  mutate(V1 = factor(V1, levels = c("Private cues only", "Social cues only", "Social and private cues")))

ll$V2 <- as.numeric(ll$V2)

#Log liklihood support for models
ll %>%
  ggplot(aes(x = V1, y = V2, fill = V1, colour = V1)) +
  geom_point(shape = 4, size = 4, stroke = 2) +
  labs(
  y = "Log liklihood",
  x = NULL
) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

  