### RUNNING THE SIMULATION USING PIPELINE ###

# TO DO:
# - parallelise sapply to use all 4 cores -- maybe try mclapply instead?
# - play with mclapply
# - also actually probably just do fewer runs but do more steps per run to fix this discrepancy


rm(list = ls())
#dev.off()


# speed parameter:

speed_parameter <- log(0.5)
# range of speeds that are realistic to simulate: 
# hedgehog speeds: roughly up to 2m/s
# fox speeds: roughly up to 13.9m/s


# number of simulation repeats:

n <- 100 
# maybe stick to 100 for now but just increase the step_no to make realised speed converge better on speed parameter


# number of steps per path:

step_no <- 5e4
# --> if increase this could help to make realised speeds converge better on the speed parameter
# --> problem: as you increase this it makes the simulation take forever


# run pipeline.R script:

source("~/Documents/Project/CamtrapSimulation/code/simulation/pipeline.R", echo=TRUE)


