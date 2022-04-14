## PIPELINE for HPC use ##

rm(list = ls())

source("CamtrapSimulation.R", echo=TRUE)
source("point_in_poly.R", echo = TRUE)

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX")) # and submit as qsub -J 1-10 - but don't need to change bash scripts

# set speed parameter
speeds <- c(0.05) # list whatever speeds you want (can be multiple) - and if you want 10 of each need -J 1-40 (needs to be a multiple of number of speeds)
# for muliple speeds: seq(from = 0.05, to = 0.2, by = 0.01)
speed_parameter <- speeds[(iter - 1)%%length(speeds) + 1] # then use this as the speed parameter

# set parameters
# path: 
step_no = 5e5
size = 0
xlim = c(0,40)
speedSD = 1
pTurn = 0.5
speedCor = 0.9
kTurn = 2
kCor = TRUE
# dz dimensions:
x = 20
y = 10
r = 9
th = 0.7
# other:
plot_path = TRUE
twoCTs = FALSE

filename <- paste0(speed_parameter, "_y", y, "_r", r, "_th", th, "_run", iter) # to change - and make sure speed_parameter is in here if doing multiple speeds

png(file= paste0(filename, ".png"),
    width=700, height=650)
seq_dats <- seq_dat(speed_parameter, step_no = step_no, size = size, xlim = xlim, speedSD = speedSD, pTurn = pTurn, speedCor = speedCor, kTurn= kTurn, kCor = kCor, x = x, y = y, r = r, th = th, plot_path = plot_path, twoCTs = twoCTs)
dev.off()

save(seq_dats, file = paste0(filename, ".RData"))
