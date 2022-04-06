## PIPELINE for HPC use##

rm(list = ls())
#dev.off()

source("CamtrapSimulation.R", echo=TRUE)
source("point_in_poly.R", echo = TRUE)

require(ggplot2)
require(gridExtra)
require(ggpubr)
require(parallel)


## SET PARAMETERS

starting_sp <- log(0.045) # set lowest starting speed parameter - somewhere so that get speeds of 0.05-0.2

n_sp <- 10 # set number of speed parameters to simulate

speed_parameter <- c()
for (i in 1:n_sp){
  sp <- exp(starting_sp) + i*0.005
  speed_parameter <- c(speed_parameter, log(sp))
}


seq_dats <- sapply(speed_parameter, seq_dat, 
                   step_no = 5e5, 
                   size = 0, 
                   xlim = c(0,40), 
                   speedSD = 0.5,
                   pTurn = 0.5,
                   speedCor = 0,
                   kTurn= 2)



## plot the first one:

plot_sim <- function(speed_parameter, step_no, size, xlim){
  xlim <- xlim
  path <- pathgen(n=step_no, 
                  kTurn=2, 
                  kCor=TRUE, 
                  pTurn=1, 
                  logspeed=speed_parameter, 
                  speedSD=0.05, 
                  speedCor=0, 
                  xlim=xlim,
                  wrap=TRUE)
  dz <- data.frame(x=20, y=10, r=10, th=1.65, dir=0) # set radius to 10m and theta to 1.65 - based on distributions of radii & angles in regent's park data
  
  if (size == 1){
    posdat <- sequence_data_large(path, dz)
  }
  if (size == 0){
    posdat <- sequence_data_small(path, dz)
  }
  
  plot_wrap(path, lineargs = list(col="grey"))
  plot_dzone(dz, border=2)
  points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
}

# store results:
folder_name <- paste("seq_dats/", exp(speed_parameter[1]), "-", exp(speed_parameter[length(speed_parameter)]), "_", step_no, "_", size, "_(", xlim[1], ",", xlim[2], ")", sep = "")

dir.create(folder_name)

png(file= paste(folder_name, "/plot", sep = ""),
    width=700, height=650)
plot_sim(speed_parameter[1], step_no, size, xlim) # plot just the first one
dev.off()

save(seq_dats, file = paste(folder_name, "/seq_dats.RData", sep = ""))
