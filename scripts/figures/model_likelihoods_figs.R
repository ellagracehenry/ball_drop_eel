library(dplyr)
library(ggplot2)

n <- cbind("Null", (2*1 + 2*172.974))
p <- cbind("Private cues only", (2*2 + 2*144.470))
s <- cbind("Social cues only", (2*2 + 2*134.565))
sp <- cbind("Social and private cues", (3*2 + 2*134.593))


#add in no K?

ll <- as.data.frame(rbind(n,sp,s,p))
ll$V1 <- as.factor(ll$V1)

ll <- ll %>%
  mutate(V1 = factor(V1, levels = c("Null", "Private cues only", "Social cues only", "Social and private cues")))

ll$V2 <- as.numeric(ll$V2)

#Log liklihood support for models
ll %>%
  ggplot(aes(x = V1, y = V2, fill = V1, colour = V1)) +
  geom_point(shape = 4, size = 4, stroke = 2) +
  labs(
  y = "Cross validated AIC",
  x = NULL
) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )
#send 20% held out figure to Mike
  