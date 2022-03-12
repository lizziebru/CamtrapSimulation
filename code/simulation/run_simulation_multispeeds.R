### RUNNING THE SIMULATION USING PIPELINE - for multiple different speeds ###

rm(list = ls())
dev.off()


# speed parameter:
# to simulate different speeds: make vector listing different speeds:
speed_parameter <- c(log(0.5), log(0.75), log(1), log(1.25), log(1.5), log(1.75), log(2), log(2.25), log(2.5), log(2.75))
# range of speeds that are realistic to simulate: 
# hedgehog speeds: roughly up to 2m/s
# fox speeds: roughly up to 13.9m/s


# number of steps per path:
step_no <- 5e3
# --> if increase this could help to make realised speeds converge better on the speed parameter
# --> problem: as you increase this it makes the simulation take forever



# run pipeline.R script:
source("~/Documents/Project/CamtrapSimulation/code/simulation/pipeline.R", echo=TRUE)


