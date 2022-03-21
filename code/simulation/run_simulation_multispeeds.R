### RUNNING THE SIMULATION USING PIPELINE - for multiple different speeds ###

rm(list = ls())
dev.off()


# speed parameter:
# to simulate different speeds: make vector listing different speeds:


# TO ADD IN LATER: SIMULATE EACH SPEED MULTIPLE TIMES TO BOOTSTRAP SO THAT YOU OVERCOME THE ISSUE OF THE PATH NOT BEING EXACTLY THE SAME EACH TIME


# set lowest starting speed parameter
starting_sp <- log(0.025)

# set number of speed parameters to simulate
n_sp <- 15

speed_parameter <- c()
for (i in 1:n_sp){
  # want to add 0.25 each time
  sp <- exp(starting_sp) + i*0.01
  speed_parameter <- c(speed_parameter, log(sp))
}


# range of speeds that are realistic to simulate: 
# hedgehog speeds: roughly up to 2m/s
# fox speeds: roughly up to 13.9m/s


# number of steps per path:
step_no <- 5e3
# --> if increase this could help to make realised speeds converge better on the speed parameter
# --> problem: as you increase this it makes the simulation take forever









# run pipeline.R script:
source("~/Documents/Project/CamtrapSimulation/code/simulation/pipeline_multispeeds.R", echo=TRUE)


