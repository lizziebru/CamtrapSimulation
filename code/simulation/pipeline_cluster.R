## PIPELINE for HPC use##

rm(list = ls())
#dev.off()


starting_sp <- log(0.05) # set lowest starting speed parameter - somewhere so that get speeds of 0.05-0.2

n_sp <- 20 # set number of speed parameters to simulate

speed_parameter <- c()
for (i in 1:n_sp){
  sp <- exp(starting_sp) + i*0.01
  speed_parameter <- c(speed_parameter, log(sp))
}


step_no <- 5e4 # number of steps per path

size <- 0 # small or large

xlim <- c(0,40)


source("CamtrapSimulation.R", echo=TRUE)
#source("sbd_functions.R", echo=TRUE)


require(ggplot2)
require(gridExtra)
require(ggpubr)
require(parallel)


#### function for running the simulation on various input speeds and measuring output speeds ####

# seq_dat
# runs the simulation: generates a path and dz, then position data, then observed speeds of each sequence (sequence = one path which crosses the CT dz and is captured at at least 2 points)
# INPUT:
# speeds: vector of the speed parameter repeated n times (n = no. of simulation runs)
seq_dat <- function(speed_parameter, step_no, size, xlim) { 
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
  
  v <- calc_speed(posdat) # speeds of sequences
  

  ## realised speeds:
  
  obs_lengths <- c() # lengths of the observed speed sequences
  for (i in 1:length(unique(posdat$sequenceID))){
    p <- posdat[posdat$sequenceID==i,]
    obs_lengths <- c(obs_lengths, nrow(p))
  }
  
  r_lengths <- round(mean(obs_lengths))   # use mean of lengths of observed speed sequences as the number of position data points to use in realised speed segments
  
  # in path$speed, select sets of speeds of length r_lengths:
  extract_realised <- function(realised_speeds, r_lengths){ # function to extract one set of realised speeds
    firstIndex <- sample(seq(length(realised_speeds) - r_lengths + 1), 1)
    realised_speeds[firstIndex:(firstIndex + r_lengths -1)]
  }
  realised_spds <- replicate(length(v$speed),{
    mean(extract_realised(path$speed, r_lengths))
  })
  
  ### number of single-frame sequences:
  t <- data.frame(table(posdat$sequenceID))
  n_singles <- nrow(t[t$Freq==1,]) # count the number of single-occurring numbers in the sequenceID column of posdat
  
  
  ### number of zero-frame sequences:
  # select pairs of consecutive points on the path that aren't in posdat (i.e. don't fall into the detection zone) - but also make sure they're not points in between which the animal went round the back of the torus (define this as having a distance between them that's greater than half of the width of the space?)
  # sample points on 1/4, 1/2, and 3/4 of the length of the line between each pair of points
  # if the coordinates of any one of those points lies in the dz: count it as a zero-frame sequence
  xy_path <- path$path[,1:2]
  rows_path <- as.numeric(rownames(xy_path))
  rows_posdat <- as.numeric(rownames(posdat))
  rows_not_in_posdat <- setdiff(rows_path, rows_posdat)
  coords_not_in_posdat <- xy_path[rows_not_in_posdat,]
  n_zeros <- 0
  for (i in (1:(nrow(coords_not_in_posdat)-1))){
    c1 <- coords_not_in_posdat[i,]
    c2 <- coords_not_in_posdat[i+1,]
    d <- sqrt((c2$x-c1$x)^2 + (c2$y-c1$y)^2)
    if (d > xlim[2]/2){
      next
    }
    
    up <- c2$y-c1$y
    across <- c2$x-c1$x
    x3 <- c1$x + across/2
    y3 <- c1$y + up/2
    c3 <- data.frame(x = x3,
                     y = y3)
    if (is_in_dz(c3,dz)==TRUE){ # if these coords are in the dz, add 1 to the count of zero frame numbers
      n_zeros <- n_zeros + 1
    }
    else{
      x4 <- c1$x + across/4
      y4 <- c1$y + up/4
      c4 <- data.frame(x = x4,
                       y = y4)
      if (is_in_dz(c4,dz)==TRUE){
        n_zeros <- n_zeros + 1
      }
      else{
        x5 <- c1$x + 3*across/4
        y5 <- c1$y + 3*up/4
        c5 <- data.frame(x = x5,
                         y = y5)
        if(is_in_dz(c5,dz)==TRUE){
          n_zeros <- n_zeros + 1
        }
      }
    }
  }
  
  ### return realised speeds, observed speeds, no. of single frames, no. of zero frames, and total no. of position datapoints
  df <- data.frame(realised = realised_spds,
                   observed = v$speed,
                   n_singles = c(rep(n_singles, length(v$speed))),
                   n_zeros = c(rep(n_zeros, length(v$speed))),
                   n_points = c(rep(nrow(posdat), length(v$speed))))
  
  return(df)
}

seq_dats <- sapply(speed_parameter, seq_dat, step_no = step_no, size = size, xlim = xlim) 



## plot the first and last ones:

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