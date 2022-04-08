## PIPELINE for HPC use##

rm(list = ls())
#dev.off()

source("CamtrapSimulation.R", echo=TRUE)
source("point_in_poly.R", echo = TRUE)

require(ggplot2)
require(gridExtra)
require(ggpubr)
require(parallel)


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

speedSD = 0.5
pTurn = 0.5
speedCor = 0
kTurn = 2

# dz dimensions:
r = 10 # try 6 
th = 1


# generate sequence data:
seq_dats <- mcsapply(speed_parameter, seq_dat, mc.cores = 4, step_no = 5e5, size = 0, xlim = c(0,40), speedSD = speedSD, pTurn = pTurn, speedCor = speedCor, kTurn= kTurn, r = r, th = th)

# save results:
folder_name <- paste("results1/5e5_0,40_0", exp(speed_parameter[1]), "_SD", speedSD, "_pTurn", pTurn, "_Cor", speedCor, "_kTurn", kTurn, sep = "")
dir.create(folder_name)

save(seq_dats, file = paste(folder_name, "/seq_dats.RData", sep = ""))

png(file= paste(folder_name, "/plot", sep = ""),
    width=700, height=650)
plot_sim(speed_parameter[1], step_no = 5e5, size = 0, xlim = c(0,40), speedSD = speedSD, pTurn = pTurn, speedCor = speedCor, kTurn= kTurn, r = r, th = th) # plot just the first one
dev.off()
