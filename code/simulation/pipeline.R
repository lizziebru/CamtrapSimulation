## PIPELINE ##

library(gridExtra)

# from simulated path to sampling by the CT to estimating average speed

# define input speeds:

speeds <- seq(-3, 2, by = 0.1)


# run simulation on each speed and measure output speeds

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
  # Q: was considering storing these plots somewhere if it might be helpful?
  
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
                          speed = outputs)

# remove rows containing NaNs:
seq_dats_df <- na.omit(seq_dats_df)


## calculate average speed - using hmean, size-biased models, and Palencia's method

# Q: any other methods of calculating average speed to try out?

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

# both the models and AICs for each input_speed are in the columns though:
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
  # Q: default is newdata = NULL - could we discuss what this is please? - and generally what the deal is with covariates in all of this
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



## PABLO PALENCIA'S METHOD:

# using Pablo Palencia's method -- need to check that what he's doing is theoretically & empirically robust

# BUT: as far as I can tell he just did the exact same as Marcus did in 2016 paper (use lnorm, gamma, & Weibull and compare AICs)


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