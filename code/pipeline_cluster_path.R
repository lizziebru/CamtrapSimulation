## PIPELINE for HPC use - for generating paths only ##

# # to help decide on parameters:
# sp = 1.5
# sd = 1
# distr <- rlnorm(1000, mean = log(sp), sd = log(sd))
# plot(density(distr))

rm(list = ls())

source("CamtrapSimulation.R", echo=TRUE)

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

# set speed parameter
speeds <- log(0.02) # list whatever speeds you want (can be multiple) - and if you want 10 of each need -J 1-40 (needs to be a multiple of number of speeds)
# for multiple speeds: seq(from = 0.05, to = 0.2, by = 0.01)
speed_parameter <- speeds[(iter - 1)%%length(speeds) + 1] # then use this as the speed parameter

# set parameters
date <- Sys.time()
xlim = c(0,40)
step_no = 5e5
size = 0
pTurn = 0.5
speedCor = 0.9
kTurn = 2
kCor = TRUE
wrapped = TRUE # default in pathgen though so don't give as an argument

# generate path
path <- pathgen(n=step_no, kTurn=kTurn, kCor=kCor, pTurn=pTurn, logspeed=speed_parameter, size=size, speedCor=speedCor, xlim=xlim)

metadata <- list(datetime=date, iter=iter, speed_parameter=speed_parameter, size=size, xlim=xlim, step_no=step_no, speedSD=speedSD, pTurn=pTurn, speedCor=speedCor, kTurn=kTurn, kCor=kCor, wrap=wrapped)

# save path
filename <- paste0("sp", exp(speed_parameter), "_", format(date, "%d%b%y_%H%M_iter"), iter) # format datetime object 
save(path, metadata, file = paste0(filename, ".RData")) 

