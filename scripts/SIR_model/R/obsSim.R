
#Load in the positions of eels
coords <- read.csv("/Users/ellag/Desktop/PhD/academic_projects/eel_diel/data/topology/garden_eel_diel-270525_D1/D2_28-05-25_annotations_3d.csv")
IDs <- coords$ID


  
#Get colony extent / boundaries
minX <- min(coords$positions_X)
maxX <- max(coords$positions_X)
minY <- min(coords$positions_Y)
maxY <- max(coords$positions_Y)
minZ <- min(coords$positions_Z)
maxZ <- max(coords$positions_Z)

#Random even distribution of ball positions across space
x_values <- runif(n = 200, min = minX, max = maxX)
y_values <- runif(n = 200, min = minY, max = maxY)
z_values <- runif(n = 200, min = minZ, max = maxZ)

ball_positions <- as.data.frame(cbind(x_values, y_values, z_values))

scatterplot3d(ball_positions$x_values, ball_positions$y_values, ball_positions$z_values)

#Random even distribution of instantaneous emerged. Is there a different result when you or don't assume uniform distribution of emered 
inst_emerged_numbers<- floor(runif(n=200, min = 2, max = length(coords)))

#Combine random positions with random inst emerged
sim_trials <- cbind(ball_positions, inst_emerged_numbers)

sim_trials$drop_ID <- 1:nrow(sim_trials)

#Evaluate SIR model on simulations
#input ball positions and emerged eels into simulation
sim_result <- evaluateHides(sim_trials, coords, initator_responder)
  

#Extract proportion of times each individual hides

#