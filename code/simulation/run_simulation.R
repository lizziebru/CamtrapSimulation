### RUNNING THE SIMULATION USING PIPELINE ###

rm(list = ls())
dev.off()


# speed parameter:

speed_parameter <- log(0.5)
# range of speeds that are realistic to simulate: 
# hedgehog speeds: roughly up to 2m/s
# fox speeds: roughly up to 13.9m/s


# number of simulation repeats:

n <- 100 
# maybe stick to 100 for now but just increase the step_no to make realised speed converge better on speed parameter


# number of steps per path:

step_no <- 5e3
# --> if increase this could help to make realised speeds converge better on the speed parameter
# --> problem: as you increase this it makes the simulation take forever


# 

## other things to vary:

# hazard rate vs hazard rate with logistic mix detection PDF -- esp should probs use logistic mix when simulating smaller spp






# run pipeline.R script:

source("~/Documents/Project/CamtrapSimulation/code/simulation/pipeline.R", echo=TRUE)


