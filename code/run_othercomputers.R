## FOR RUNNING THE SIMULATION ON OTHER COMPUTERS ##

# required packages and functions

#install.packages("ggplot2")
require(ggplot2)

#install.packages("circular")
require(circular)

# install.packages("parallel")
# require(parallel)

source("CamtrapSimulation.R", echo=TRUE)

parentfolder <- paste0("")
pathfolder <- paste0("")

iter <- 100 

species = 0 
r = 9
th = 0.7
path_cutby = 0.5
twoCTs = TRUE
connectedCTs = FALSE

run_and_analyse(parentfolder=parentfolder, pathfolder=pathfolder, iter=iter, species=species, r=r, th=th, twoCTs=twoCTs, connectedCTs=connectedCTs, path_cutby = path_cutby, n_cores=4)
