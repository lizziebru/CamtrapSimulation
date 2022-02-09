## PIPELINE ##

# from simulated path to sampling by the CT to estimating average speed

# define input speeds:

speeds <- seq(-3, 2, by = 0.1)


# make a function to measure speeds for each 

seq_dat <- function(speeds) { 
  path <- pathgen(5e3, kTurn=2, kCor=TRUE, pTurn=1, 
                  logspeed=speeds, speedSD=1, speedCor=0, 
                  xlim=c(0,10), wrap=TRUE)
  
  dz <- data.frame(x=5, y=2, r=6, th=1, dir=0)
  
  # make plot
  p <- plot_wrap(path, lineargs = list(col="grey"))
  plot_dzone(dz, border=2)

  # generate position data
  posdat <- sequence_data(path, dz)
  
  # add points to plot
  points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
  
  # work out speeds of sequences
  calc_speed(posdat)

}

# define speeds and apply the function

# make vector of some speeds to compare:
speeds <- seq(from = -3, to = 2, by = 0.25) # upper limit here is a bit under the max for foxes

# run the simulation on each speed
seq_dats <- sapply(speeds, seq_dat)
# (seq_data are in a[,1], a[,2] etc)



## calculate average speed

# need to do it for every column in a




# using Marcus' sbd functions

# using Pablo Valencia's method -- need to check that what he's doing is theoretically & empirically robust

