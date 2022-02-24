## PIPELINE ##

# from simulating the path to sampling by the CT to estimating average speed

library(gridExtra)



## Run simulation on various input speeds and measure output speeds

# seq_dat
# runs the simulation: generates a path and dz, then position data, then measures speeds of each sequence
# INPUT:
# speeds: vector of average input speeds for the animal movement path - can just be one though
seq_dat <- function(speeds) { 
  path <- pathgen(5e3, 
                  kTurn=2, 
                  kCor=TRUE, 
                  pTurn=1, 
                  logspeed=speeds, 
                  speedSD=1, 
                  speedCor=0, 
                  xlim=c(2,16), 
                  wrap=TRUE) ## now need to find a way to make the path cross the dz more - and maybe fix speeds
  
  dz <- data.frame(x=10, y=5, r=10, th=1.65, dir=0) # set radius to 10m and theta to 1.65 - based on distributions of raddi & angles in regent's park data
  
  # make plot
  p <- plot_wrap(path, lineargs = list(col="grey"))
  plot_dzone(dz, border=2)
  # Q: was considering storing these plots somewhere if it might be helpful?
  
  # generate position data
  posdat <- sequence_data(path, dz)
  
  # add points to plot
  points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
  
  # work out speeds of sequences
  calc_speed(posdat)

}

# define speeds and apply the function:

#### run for 1 speed once through ####

# define one input average speed:
#seq_dats <- sapply(-3, seq_dat)
# when apply it to just one speed: speeds are in seq_dats[4] (although there are NaNs!)



#### run for multiple different speeds once through each ####

# # can also apply to a vector of multiple speeds:
# speeds <- seq(from = -3, to = 2, by = 0.25) # upper limit here is a bit under the max for foxes (if speeds are in m/s)
# seq_dats <- sapply(speeds, seq_dat) # this step takes quite a while (under a minute though still - but might be problematic when inputting more and larger speeds)
# # (seq_data are in seq_dats[,1], seq_dats[,2] etc)

# # make df with these speeds:
# inputs <- c()
# outputs <- c()
# for (i in 1:length(speeds)){
#   inputs <- c(inputs, rep(speeds[i], length(seq_dats[,i]$speed)))
#   outputs <- c(outputs, seq_dats[,i]$speed)
# }
# seq_dats_df <- data.frame(input_average = exp(inputs),
#                           speed = outputs)
# 
# # remove rows containing NaNs:
# seq_dats_df <- na.omit(seq_dats_df)

#### run on 1 speed 100 times through ####

# to repeat the simulation on the same input average speed multiple times:
# could make a vector of the same speed repeated, then sapply to that vector - unless M can think of a better way to do this??
speeds <- rep(-3, length = 100)
seq_dats <- sapply(speeds, seq_dat)
# took around 30min to run

# make vector of measured speeds:

measured_speeds <- c()

for (i in c(1:length(speeds))){
  measured_speeds <- c(measured_speeds, seq_dats[,i]$speed)
}













## calculate average speed - using hmean and size-biased models


## HARMONIC MEAN:

# want to apply hmean to every set of speeds corresponding to each unique input_average value

harmonic <- c()
calc_hmean <- function(input_speed){
  harmonic <- c(harmonic, hmean(seq_dats_df[seq_dats_df$input_average==input_speed,]$speed))
  return(harmonic)
}
harmonics <- sapply(unique(seq_dats_df$input_average), calc_hmean)


## SIZE-BIASED MODELS:

# 1. fit all the models

mods_all_fit <- function(input_speed){
  # subset by input speed:
  d <- seq_dats_df[seq_dats_df$input_average==input_speed,]
  sbm3(speed~1, d)
}

mods <- sapply(unique(seq_dats_df$input_average), mods_all_fit)
# what the outputs look like:
# mods[1,1] == mods[1] == models for input_speed[1]
# mods[2,1] == mods[2] == AICs for input_speed[1]
# mods[1,2] == mods[3] == models for input_speed[2]
# mods[2,2] == mods[4] == AICs for input_speed[2]

# both the models and AICs for each input_speed are in the columns:
# -- mods[,1] = models & AICs for input_speed[1]
# -- mods[,2] = models & AICs for input_speed[2]


# 2. plot the models

# want to do this for each mods[1], mods[3] etc - multipanel plot with all 3 models for each

plot_all <- function(input_speed_no){
  layout(matrix(1:3, ncol=3))
  par(oma=c(4, 0, 4, 0), mar=c(4, 4, 4, 4))
  plot.sbm(mods[[1,input_speed_no]]$lnorm, title = "lnorm") # default in plot.sbm is to plot the distribution on a log scale - Q: could we discuss this to better get my head around it please
  plot.sbm(mods[[1,input_speed_no]]$gamma, title = "gamma")
  plot.sbm(mods[[1,input_speed_no]]$weibull, title = "Weibull")
  title(main=paste("Input speed = ", exp(speeds[input_speed_no]), sep = ""), outer=TRUE, cex.main=2)
}

input_speed_numbers <- seq(1, length(speeds), by = 1)

plots_each_input_speed <- lapply(input_speed_numbers, plot_all) 
# --> the higher the speed, the more bins and the nicer the fit looks


# just having issues with putting them all together in a panel and saving that...

# arrange all the plots in one panel
plots_each_input_speed_panel <- marrangeGrob(plots_each_input_speed, ncol = 1, nrow = 21, top = quote(paste("page", g, "of", npages)))
ggsave(filename = "sbm_plots.png", plot = m1, path = "plots", width = 10, height = 20)
# --> to fix

# jpeg(filename = "/plots/sbm_plots.png")
# plots_each_input_speed_panel <- arrangeGrob(plots_each_input_speed, nrow = 21, ncol = 1)
# #plots_each_input_speed_panel <- grid.arrange(plots_each_input_speed, nrow = 21, ncol = 1)
# dev.off()



# 3. predict average speed using the models

predict_lnorm <- function(input_speed_no){
  predict.sbm(mods[[1,input_speed_no]]$lnorm)[1] # selects just the estimate of speed
  # Q: default is newdata = NULL - could we discuss what this is please?
  # Q: would it be a good idea to save other info e.g. standard error?
}

predict_gamma <- function(input_speed_no){
  predict.sbm(mods[[1,input_speed_no]]$gamma)[1]
}

predict_weibull <- function(input_speed_no){
  predict.sbm(mods[[1,input_speed_no]]$weibull)[1]
}

mods_predict_lnorm <- sapply(input_speed_numbers, predict_lnorm)
mods_predict_gamma <- sapply(input_speed_numbers, predict_gamma)
mods_predict_weibull <- sapply(input_speed_numbers, predict_weibull)


# 4. extract and store the AICs for each model

# mods is in such a bizarre format that the best way I could come up with was this function: (couldn't get the AIC.sbm function working)

lnorm_AIC_extract <- function(input_speed_no){
  a1 <- mods[2,input_speed_no]
  a2 <- a1$AICtab[1]
  a2["lnorm",]
}

gamma_AIC_extract <- function(input_speed_no){
  a1 <- mods[2,input_speed_no]
  a2 <- a1$AICtab[1]
  a2["gamma",]
}

weibull_AIC_extract <- function(input_speed_no){
  a1 <- mods[2,input_speed_no]
  a2 <- a1$AICtab[1]
  a2["weibull",]
}

lnorm_AICs <- sapply(input_speed_numbers, lnorm_AIC_extract)
gamma_AICs <- sapply(input_speed_numbers, gamma_AIC_extract)
weibull_AICs <- sapply(input_speed_numbers, weibull_AIC_extract)



## output: input average speed and each calculated average speed

simulation_speeds <- data.frame(input = exp(speeds),
                                hmean = harmonics[1,],
                                lnorm = as.numeric(mods_predict_lnorm),
                                gamma = as.numeric(mods_predict_gamma),
                                weibull = as.numeric(mods_predict_weibull))

# Q: do we maybe want more info in this dataframe too? e.g. maybe some of the input parameters which we can vary

# make separate df with AICs for the models - thought for now it made things neater
model_AICs <- data.frame(input = exp(speeds),
                         lnorm = as.numeric(mods_predict_lnorm),
                         lnormAIC = lnorm_AICs,
                         gamma = as.numeric(mods_predict_gamma),
                         gammaAIC = gamma_AICs,
                         weibull = as.numeric(mods_predict_weibull),
                         weibullAIC = weibull_AICs)



# want to compare distributions of speeds though:
# compare:
# - distribution of simulated input speeds - so should have around 20 (varies depending on how many times the path crosses the dz) x 1000 values
# - distribution of measured average speeds - when simulation is repeated 1000 times on 1 input speed
























# Pablo's behavioural states addition -------------------------------------

# need to check that what he's doing is theoretically & empirically robust



