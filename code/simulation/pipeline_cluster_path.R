## PIPELINE for HPC use - for generating paths only ##

rm(list = ls())

source("CamtrapSimulation.R", echo=TRUE)

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

# set speed parameter
speeds <- round(seq(0.05,0.2,0.001), digits = 4) # list whatever speeds you want (can be multiple) - and if you want 10 of each need -J 1-40 (needs to be a multiple of number of speeds)
# for multiple speeds: seq(from = 0.05, to = 0.2, by = 0.01)
speed_parameter <- speeds[(iter - 1)%%length(speeds) + 1] # then use this as the speed parameter

# set parameters
date <- Sys.time()
xlim = c(0,40)
step_no = 5e5
speedSD = 1
pTurn = 0.5
speedCor = 0.9
kTurn = 2
kCor = TRUE
wrap = TRUE



# generate path
path <- pathgen(n=step_no, kTurn=kTurn, kCor=kCor, pTurn=pTurn, logspeed=speed_parameter, speedSD=speedSD, speedCor=speedCor, xlim=xlim, wrap=wrap)

metadata <- list(datetime=date, iter=iter, speed_parameter=speed_parameter, xlim=xlim, step_no=step_no, speedSD=speedSD, pTurn=pTurn, speedCor=speedCor, kTurn=kTurn, kCor=kCor, wrap=wrap)

# save path
filename <- paste0(format(date, "%d%b%y_%H%M_iter"), iter) # format datetime object 
save(path, metadata, file = paste0(filename, ".RData")) # to do: store the path and metadata too along with date as well

# put date & time in metadata too
# also save iter of the job 
# save the speed parameter & other params too
