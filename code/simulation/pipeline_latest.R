## PIPELINE for HPC use##

rm(list = ls())
#dev.off()

source("CamtrapSimulation.R", echo=TRUE)
source("point_in_poly.R", echo = TRUE)

require(ggplot2)
require(gridExtra)
require(ggpubr)
require(parallel)


iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX")) # and submit as qsub -J 1-10 - but don't need to change bash scripts

# to simulate multiple speeds:
speeds <- c(0.05) # list whatever speeds you want (can be multiple) - and if you want 10 of each need -J 1-40 (needs to be a multiple of number of speeds)
speed_parameter <- speeds[(iter - 1)%%length(speeds) + 1] # then use this as the speed parameter


# # if simulating multiple different speeds:
# starting_sp <- log(0.045) # set lowest starting speed parameter - somewhere so that get speeds of 0.05-0.2
# n_sp <- 10 # set number of speed parameters to simulate
# speed_parameter <- c()
# for (i in 1:n_sp){
#   sp <- exp(starting_sp) + i*0.005
#   speed_parameter <- c(speed_parameter, log(sp))
# }

speedSD = 0.5
pTurn = 0.5
speedCor = 0
kTurn = 2

# dz dimensions:
x = 15
y = 15
r = 10 # try 6 
th = 1

filename <- paste0(speed_parameter, "_y", y, "_r", r, "_th", th, "_run", iter) # to change - and make sure speed_parameter is in here if doing multiple speeds


# generate sequence data:
png(file= paste(filename, ".png", sep = ""),
    width=700, height=650)
seq_dats <- seq_dat(speed_parameter, step_no = 5e5, size = 0, xlim = c(0,40), speedSD = speedSD, pTurn = pTurn, speedCor = speedCor, kTurn= kTurn, x = x, y = y, r = r, th = th, plot_path = TRUE)
dev.off()

save(seq_dats, file = paste(filename, ".RData", sep = ""))

