## PIPELINE ##

# pipeline for running the simulation from simulating the path to sampling by the CT to estimating average speed using multiple different methods

source("~/Documents/Project/CamtrapSimulation/code/simulation/CamtrapSimulation.R", echo=TRUE)
source("~/Documents/Project/CamtrapSimulation/code/simulation/sbd_functions.R", echo=TRUE)


require(ggplot2)
require(gridExtra)
require(ggpubr)


#### function for running the simulation on various input speeds and measuring output speeds ####

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
                  xlim=c(0,20),
                  ylim=c(2,16),
                  wrap=TRUE)
  
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
  v <- calc_speed(posdat)
  
  # return df with input and measured speed for each step
  df_input <- data.frame(input = path$speed)
  
  df_measured <- data.frame(measured = v$speed)
    
  return(df_measured)

  ## Q: do we want to use input or measured to compare to our averaged output speeds?
  
}


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

# which speeds to choose:
# hedgehog speeds: roughly up to 2m/s
# fox speeds: roughly up to 13.9m/s


# to repeat the simulation on the same input average speed multiple times:
# could make a vector of the same speed repeated, then sapply to that vector - unless M can think of a better way to do this??
speeds <- rep(simulate_speed, length = n) # == 0.37 m/s
seq_dats <- sapply(speeds, seq_dat)

# measured speeds are in seq_dats[1] through to seq_dats[100]


##### calculate average speed - using hmean and SBMs #####

# apply each method (hmean, SBMlog, SBMgamma, SBMWeibull) to every set of speeds - 100 sets of them currently - so should get a distribution of 100 average values for each of the 4 methods

# measured speeds are in seq_dats[1] etc


## HARMONIC MEAN:


## calc_hmean
# work out harmonic mean of a set of measured speeds (i.e. each simulation rep)
# returns a harmonic mean and standard error for each set of speeds
# INPUT:
# number of reps of the simulation - so that can loop through every set of measured speeds
harmonic <- c()
calc_hmean <- function(rep_no){
  
  # format speeds per input rep number
  s1 <- seq_dats[rep_no]
  s2 <- s1$measured
  s3 <- s2[!is.nan(s2)]
  
  # work out hmean for the set of speeds for this input rep number
  harmonic <- c(harmonic, hmean(s3))
  
  return(harmonic)
}

harmonics <- sapply(c(1:length(speeds)), calc_hmean)




## SIZE-BIASED MODELS:

# 1. fit all the models

## mods_all_fit
# fits all 3 SBMs (lognormal, gamma, Weibull) to each set of measured speeds (i.e. each simulation rep)
# return three models for each set of measured speeds
# INPUT:
# number of reps of the simulation - so that can loop through every set of measured speeds
mods_all_fit <- function(rep_no){
  
  # format speeds per input rep number
  s1 <- seq_dats[rep_no]
  s2 <- s1$measured
  s3 <- s2[!is.nan(s2)]
  
  # make df:
  df <- data.frame(speed = s3)
  
  # fit all three models:
  sbm3(speed~1, df)
}

mods <- sapply(c(1:length(speeds)), mods_all_fit)
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

## plot_all
# plots all three models in one multi-panel plot for each set of measured speeds (i.e. for each rep of the simulation)
# INPUT:
# number of reps of the simulation - so that can loop through every set of measured speeds
plot_all <- function(rep_no){
  layout(matrix(1:3, ncol=3))
  par(oma=c(4, 0, 4, 0), mar=c(4, 4, 4, 4))
  plot.sbm(mods[[1,rep_no]]$lnorm, title = "lnorm") # default in plot.sbm is to plot the distribution on a log scale - Q: could we discuss this to better get my head around it please
  plot.sbm(mods[[1,rep_no]]$gamma, title = "gamma")
  plot.sbm(mods[[1,rep_no]]$weibull, title = "Weibull")
  title(main=paste("Input speed = ", exp(speeds[rep_no]), sep = ""), outer=TRUE, cex.main=2)
}

sbm_plots <- lapply(c(1:length(speeds)), plot_all) 
# --> the higher the speed, the more bins and the nicer the fit looks

# just having issues with putting them all together in a panel and saving that... - TO ASK FRANCIS ABOUT


# arrange all the plots in one panel
# m <- marrangeGrob(grobs = sbm_plots, nrow = 100, ncol = 1, newpage = TRUE)
# m <- arrangeGrob(grobs = sbm_plots, nrow = 100, ncol = 1, newpage = TRUE)
# m <- grid.arrange(grobs = sbm_plots, nrow = 100, ncol=1, newpage = TRUE)


# ggsave(filename = "fitted_SBMs.png", plot = m, path = "plots", width = 10, height = 18)

# arrange all the plots in one panel
# plots_each_input_speed_panel <- marrangeGrob(plots_each_input_speed, ncol = 1, nrow = 21, top = quote(paste("page", g, "of", npages)))
# ggsave(filename = "sbm_plots.png", plot = m1, path = "plots", width = 10, height = 20)
# --> to fix

# jpeg(filename = "/plots/sbm_plots.png")
# plots_each_input_speed_panel <- arrangeGrob(plots_each_input_speed, nrow = 21, ncol = 1)
# #plots_each_input_speed_panel <- grid.arrange(plots_each_input_speed, nrow = 21, ncol = 1)
# dev.off()



# 3. predict average speed using the models

## predict_lnorm
# predict average speed using a fitted lognormal model for each set of measured speeds (i.e. each simulation rep)
# INPUT:
# number of reps of the simulation - so that can loop through every set of measured speeds
predict_lnorm <- function(rep_no){
  predict.sbm(mods[[1,rep_no]]$lnorm)[1] # selects just the estimate of speed
  # Q: default is newdata = NULL - could we discuss what this is please?
  # Q: would it be a good idea to save other info e.g. standard error?
}

## predict_gamma
# predict average speed using a fitted gamma model for each set of measured speeds (i.e. each simulation rep)
# INPUT:
# number of reps of the simulation - so that can loop through every set of measured speeds
predict_gamma <- function(rep_no){
  predict.sbm(mods[[1,rep_no]]$gamma)[1]
}

## predict_weibull
# predict average speed using a fitted Weibull model for each set of measured speeds (i.e. each simulation rep)
# INPUT:
# number of reps of the simulation - so that can loop through every set of measured speeds
predict_weibull <- function(rep_no){
  predict.sbm(mods[[1,rep_no]]$weibull)[1]
}

mods_predict_lnorm <- sapply(c(1:length(speeds)), predict_lnorm)
mods_predict_gamma <- sapply(c(1:length(speeds)), predict_gamma)
mods_predict_weibull <- sapply(c(1:length(speeds)), predict_weibull)


# 4. extract and store the AICs for each model

# mods is in such a bizarre format that the best way I could come up with was this function: (couldn't get the AIC.sbm function working)

## lnorm_AIC_extract
# return the AIC for the fitted lognormal model for each set of measured speeds
# INPUT:
# number of reps of the simulation - so that can loop through every set of measured speeds
lnorm_AIC_extract <- function(rep_no){
  a1 <- mods[2,rep_no]
  a2 <- a1$AICtab[1]
  a2["lnorm",]
}

## gamma_AIC_extract
# return the AIC for the fitted gamma model for each set of measured speeds
# INPUT:
# number of reps of the simulation - so that can loop through every set of measured speeds
gamma_AIC_extract <- function(rep_no){
  a1 <- mods[2,rep_no]
  a2 <- a1$AICtab[1]
  a2["gamma",]
}

## weibull_AIC_extract
# return the AIC for the fitted Weibull model for each set of measured speeds
# INPUT:
# number of reps of the simulation - so that can loop through every set of measured speeds
weibull_AIC_extract <- function(rep_no){
  a1 <- mods[2,rep_no]
  a2 <- a1$AICtab[1]
  a2["weibull",]
}

lnorm_AICs <- sapply(c(1:length(speeds)), lnorm_AIC_extract)
gamma_AICs <- sapply(c(1:length(speeds)), gamma_AIC_extract)
weibull_AICs <- sapply(c(1:length(speeds)), weibull_AIC_extract)



## output: input average speed and each calculated average speed

speeds_df <- data.frame(input = exp(speeds),
                                hmean = harmonics[1,],
                                lnorm = as.numeric(mods_predict_lnorm),
                                gamma = as.numeric(mods_predict_gamma),
                                weibull = as.numeric(mods_predict_weibull))


# make separate df with AICs for the models - for now it feels like it makes things neater
model_AICs <- data.frame(input = exp(speeds),
                         lnorm = as.numeric(mods_predict_lnorm),
                         lnormAIC = lnorm_AICs,
                         gamma = as.numeric(mods_predict_gamma),
                         gammaAIC = gamma_AICs,
                         weibull = as.numeric(mods_predict_weibull),
                         weibullAIC = weibull_AICs)



#### compare distributions of input & output speeds ####

# compare:
# - distribution of simulated input speeds - so should have around 20 (varies depending on how many times the path crosses the dz) x 1000 values
# - distribution of measured average speeds - when simulation is repeated 1000 times on 1 input speed


# simulated input speeds:
# concatenate all 100 sets of measured speeds:

measured_speeds <- c()
# format speeds per input rep number

for (i in 1:length(speeds)){
  s1 <- seq_dats[i]
  s2 <- s1$measured
  s3 <- s2[!is.nan(s2)]
  
  measured_speeds <- c(measured_speeds, s3)
}


# make box and whisker plot comparing measured_speeds, hmean averages, and each SBM average:

# make df:
# bc of different lengths: need to fill the tails of some columns with NAs
list_all <- list(measured_speeds, harmonics[1,], as.numeric(mods_predict_lnorm), as.numeric(mods_predict_gamma), SBM_weibull = as.numeric(mods_predict_weibull))
len <- max(lengths(list_all))
cols <- lapply(list_all, function(l) c(l, rep(NA, len - length(l))))
boxplot_df <- as.data.frame(Reduce(cbind, cols, init = NULL))
colnames(boxplot_df) <- c("measured", "hmean", "SBM_lnorm", "SBM_gamma", "SBM_weibull")

# make it in a different format - better for plotting
box_df <- data.frame(measure = c(rep("measured", length(measured_speeds)), 
                                 rep("hmean", length(harmonics[1,])), 
                                 rep("SBM_lnorm", length(as.numeric(mods_predict_lnorm))), 
                                 rep("SBM_gamma", length(as.numeric(mods_predict_gamma))), 
                                 rep("SBM_weibull", length(as.numeric(mods_predict_weibull)))),
                     speed = c(measured_speeds, 
                               harmonics[1,], 
                               as.numeric(mods_predict_lnorm), 
                               as.numeric(mods_predict_gamma), 
                               as.numeric(mods_predict_weibull)))


# set specific order to make plot clearer
box_df$measure <- factor(box_df$measure , levels=c("SBM_weibull", "SBM_gamma", "SBM_lnorm","hmean", "measured"))

# plot:
box <- ggplot(box_df, aes(x = measure, y = speed, fill = measure))+ 
  geom_boxplot(notch = TRUE)+
  coord_flip()+
  theme_minimal()+
  theme(legend.position="none")+
  labs(y = "speed (m/s)")
  

## -- looks like high speeds get missed by all the methods?


# remove some of the high measured values to better compare the distributions:
box_df2 <- box_df[box_df$speed < 4*exp(simulate_speed), ]

box_minus_high_measured_speeds <- ggplot(box_df2, aes(x = measure, y = speed, fill = measure))+ 
  geom_boxplot(notch = TRUE)+
  coord_flip()+
  theme_minimal()+
  theme(legend.position="none")+
  labs(y = "speed (m/s)")



# plot comparing amongst medians and means
# averages_df <- data.frame(measure = c("measured", "hmean", "SBM_lnorm","SBM_gamma", "SBM_weibull"),
#                           median = c(median(measured_speeds), 
#                                     median(harmonics[1,]), 
#                                     median(as.numeric(mods_predict_lnorm)), 
#                                     median(as.numeric(mods_predict_gamma)), 
#                                     median(as.numeric(mods_predict_weibull))),
#                           mean = c(mean(measured_speeds), 
#                                    mean(harmonics[1,]), 
#                                    mean(as.numeric(mods_predict_lnorm)), 
#                                    mean(as.numeric(mods_predict_gamma)), 
#                                    mean(as.numeric(mods_predict_weibull)))) 

averages_df <- data.frame(speed = c(median(measured_speeds),
                                    mean(measured_speeds),
                                    median(harmonics[1,]),
                                    mean(harmonics[1,]),
                                    median(as.numeric(mods_predict_lnorm)),
                                    mean(as.numeric(mods_predict_lnorm)),
                                    median(as.numeric(mods_predict_gamma)),
                                    mean(as.numeric(mods_predict_gamma)),
                                    median(as.numeric(mods_predict_weibull)),
                                    mean(as.numeric(mods_predict_weibull))),
                          measure = c(rep("measured", length(2)), rep("hmean", length(2)), rep("SBM_lnorm", length(2)), rep("SBM_gamma", length(2)), rep("SBM_weibull", length(2))),
                          average = c(rep(c("median", "mean"), length(5))))

averages_df$measure <- factor(averages_df$measure , levels=c("measured", "hmean", "SBM_lnorm", "SBM_gamma", "SBM_weibull"))

averages_plot <- ggplot(averages_df, aes(x = measure, y = speed))+
  geom_point()+
  facet_grid(rows = vars(average))


# -- to think about: should we be comparing medians or means or something else?




#### save the three main plots ####

png(file = paste("plots/simulated_speed_exp(", simulate_speed, ").png", sep = ""),
    width = 600, height = 1000)
plots_arranged <- ggarrange(box, box_minus_high_measured_speeds, averages_plot, labels = c("A", "B", "C"), nrow = 3)
annotate_figure(plots_arranged, top = paste("Simulated speed = ", exp(simulate_speed), " m/s", " (exp(", simulate_speed, "))", sep = ""))
dev.off()


# Pablo's behavioural states addition -------------------------------------

# need to check that what he's doing is theoretically & empirically robust



