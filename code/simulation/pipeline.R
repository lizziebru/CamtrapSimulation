## PIPELINE ##

# from simulated path to sampling by the CT to estimating average speed

# define input speeds:

speeds <- seq(-3, 2, by = 0.1)


# make a function to measure speeds for each 

seq_dat <- function(speeds) { 
  path <- pathgen(5e3, 
                  kTurn=2, 
                  kCor=TRUE, 
                  pTurn=1, 
                  logspeed=speeds, 
                  speedSD=1, 
                  speedCor=0, 
                  xlim=c(0,10), 
                  wrap=TRUE)
  
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
speeds <- seq(from = -3, to = 2, by = 0.25) # upper limit here is a bit under the max for foxes (if speeds are in m/s)

# run the simulation on each speed
seq_dats <- sapply(speeds, seq_dat) # this step takes quite a while (under a minute though still - but might be problematic when inputting more and larger speeds)
# (seq_data are in seq_dats[,1], seq_dats[,2] etc)

# make df with these speeds:
inputs <- c()
outputs <- c()
for (i in 1:length(speeds)){
  inputs <- c(inputs, rep(speeds[i], length(seq_dats[,i]$speed)))
  outputs <- c(outputs, seq_dats[,i]$speed)
}
seq_dats_df <- data.frame(input_average = exp(inputs),
                          outputs = outputs)



## calculate average speed

## HARMONIC MEAN:

# want to apply hmean to every set of speeds corresponding to each unique input_average value

calc_hmean <- function(input_speed){
  harmonic <- c(harmonic, hmean(seq_dats_df[seq_dats_df$input_average==input_speed,]$outputs))
  return(harmonic)
}
harmonics <- sapply(unique(seq_dats_df$input_average), calc_hmean)


## SIZE-BIASED MODELS:

# 1. fit all the models

mods_all_fit <- function(input_speed){
  # subset by input speed:
  d <- seq_dats_df[seq_dats_df$input_average==input_speed,]
  
  
  d <- seq_dats_df[which(seq_dats_df$input_average == input_speed),]
  
}

mods_all <- sbm3(speed ~ 1, data)




# 2. plot the models


# 3. predict average speed using the models



## PABLO VALENCIA'S METHOD:

# using Pablo Valencia's method -- need to check that what he's doing is theoretically & empirically robust




## output: input average speed and each calculated average speed

simulation_speeds <- data.frame(input = exp(speeds),
                                hmean = harmonics[1,],
                                lnorm = ,
                                lnormAIC = ,
                                gamma = ,
                                gammaAIC = ,
                                weibull = ,
                                weibullAIC = )

# do we want more info in this dataframe too? e.g. maybe some of the input parameters which we can vary
