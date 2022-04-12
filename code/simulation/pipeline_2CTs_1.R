## PIPELINE for HPC use##

rm(list = ls())
#dev.off()

source("CamtrapSimulation.R", echo=TRUE)
source("point_in_poly.R", echo = TRUE)

require(ggplot2)
require(gridExtra)
require(ggpubr)
require(parallel)


Sys.getenv()

# if simulating the same speed 10 times:
speed_parameter <- rep(log(0.05), times = 10)

# # if simulating multiple different speeds:
# starting_sp <- log(0.045) # set lowest starting speed parameter - somewhere so that get speeds of 0.05-0.2
# n_sp <- 10 # set number of speed parameters to simulate
# speed_parameter <- c()
# for (i in 1:n_sp){
#   sp <- exp(starting_sp) + i*0.005
#   speed_parameter <- c(speed_parameter, log(sp))
# }

speedSD = 1
pTurn = 0.5
speedCor = 0.9
kTurn = 2

# dz dimensions:
x = 15
y = 15
r = 4 
th = 0.5


# generate sequence data:
seq_dats <- sapply(speed_parameter, seq_dat_2CTs_1, step_no = 5e5, size = 0, xlim = c(0,40), speedSD = speedSD, pTurn = pTurn, speedCor = speedCor, kTurn= kTurn, x = x, y = y, r = r, th = th)

# save results:
filename <- paste("2CTs_1_x", x, "_y", y, "_r", r, "_th", th, "_run", i, sep = "")

save(seq_dats, file = paste(filename, ".RData", sep = ""))

png(file= paste(filename, ".png", sep = ""),
    width=700, height=650)
plot_sim_2CTs_1(speed_parameter[1], step_no = 5e5, size = 0, xlim = c(0,40), speedSD = speedSD, pTurn = pTurn, speedCor = speedCor, kTurn= kTurn, x = x, y = y, r = r, th = th) # plot just the first one
dev.off()
