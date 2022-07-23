## PIPELINE for HPC use - for generating paths only ##

# # to help decide on parameters:
# sp = 1.5
# sd = 1
# distr <- rlnorm(1000, mean = log(sp), sd = log(sd))
# plot(density(distr))

rm(list = ls())

source("CamtrapSimulation.R", echo=TRUE)

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

# set body masses
body_masses <- c(1, seq(from = 5, to = 50, by = 5))
Mb <- body_masses[(iter - 1)%%length(body_masses) + 1] # use this as input Mb - if want 20 of each use -J 1-220

# set parameters
date <- Sys.time()
xlim = c(0,40)
n = 5e5
speedCor = 0.9
kTurn = 2
kCor = TRUE
wrapped = TRUE # default in pathgen though so don't give as an argument
pTurn = 0.5 # for unimodal speeds
mov_prop = 0.1 # simulate: 0.1, 0.25, 0.4, 0.75, 0.9
pTurn_mov = 0.8 # for bimodal speeds
pTurn_feed = 0.3 # for bimodal speeds
bimodal = TRUE


# generate path
path <- pathgen(n=n, kTurn=kTurn, kCor=kCor, pTurn=pTurn, Mb=Mb, speedCor=speedCor, xlim=xlim, bimodal=bimodal, mov_prop=mov_prop, pTurn_mov=pTurn_mov, pTurn_feed=pTurn_feed)

metadata <- list(datetime=date, iter=iter, Mb=Mb, xlim=xlim, step_no=n, pTurn=pTurn, speedCor=speedCor, kTurn=kTurn, kCor=kCor, wrap=wrapped, bimodal=bimodal, mov_prop=mov_prop, pTurn_mov=pTurn_mov, pTurn_feed=pTurn_feed)

# save path
filename <- paste0("Mb", Mb, "_mov", mov_prop, "_", format(date, "%d%b%y_%H%M_iter"), iter) # format datetime object 
save(path, metadata, file = paste0(filename, ".RData")) 

