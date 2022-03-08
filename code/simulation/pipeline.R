## PIPELINE ##

# pipeline for running the simulation from simulating the path to sampling by the CT to estimating average speed using multiple different methods

source("~/Documents/Project/CamtrapSimulation/code/simulation/CamtrapSimulation.R", echo=TRUE)
source("~/Documents/Project/CamtrapSimulation/code/simulation/sbd_functions.R", echo=TRUE)


require(ggplot2)
require(gridExtra)
require(ggpubr)
require(parallel)


#### function for running the simulation on various input speeds and measuring output speeds ####

# seq_dat
# runs the simulation: generates a path and dz, then position data, then observed speeds of each sequence (sequence = one path which crosses the CT dz and is captured at at least 2 points)
# INPUT:
# speeds: vector of the speed parameter repeated n times (n = no. of simulation runs)
seq_dat <- function(speeds, step_no) { 
  path <- pathgen(n=step_no,
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
  #p <- plot_wrap(path, lineargs = list(col="grey"))
  #plot_dzone(dz, border=2)
  
  # generate position data
  posdat <- sequence_data(path, dz)
  
  # add points to plot
  #points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
  
  # work out speeds of sequences
  v <- calc_speed(posdat)
  
  # return realised and observed speeds
  
  df <- data.frame(realised = rep(mean(path$speed), length(v$speed)), # -- will just be using the first value of this column - they don't correspond to the observed speeds in the neighboring column but felt like the easiest way to return both realised and observed speeds in one function
                   observed = v$speed)
    
  return(df)
}



#### run on using the speed_parameter repeated n times ####

speeds <- rep(speed_parameter, length = n)

#seq_dats <- sapply(speeds, seq_dat)

# --> need to parallelise this sapply - takes ages

# use forking with the mclapply family (other option is via sockets instead of forking)

# mcsapply
# mc-version of sapply: (mclapply is the parallel version for lapply but there isn't an equivalent for sapply
# INPUTS: 
# - same as usual for sapply: the vector of values on which to apply the function, the function to apply, and the value of any additional parameters needed for the function
# - also add in mc.cores = (number of cores in your laptop)
mcsapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
  FUN <- match.fun(FUN)
  answer <- parallel::mclapply(X = X, FUN = FUN, ...)
  if (USE.NAMES && is.character(X) && is.null(names(answer)))
    names(answer) <- X
  if (!isFALSE(simplify) && length(answer))
    simplify2array(answer, higher = (simplify == "array"))
  else answer
}

seq_dats <- mcsapply(speeds, seq_dat, mc.cores = 4, step_no = step_no)


# realised and observed speeds are in seq_dats[,1] through to seq_dats[,100]


##### calculate estimated speeds - using hmean and SBMs #####

# apply each method (hmean, SBMlog, SBMgamma, SBMWeibull) to n sets of speeds


## HARMONIC MEAN:

## calc_hmean
# work out harmonic mean of a set of observed speeds (i.e. each simulation rep)
# returns a harmonic mean and standard error for each set of speeds
# INPUT:
# number of reps of the simulation - so that can loop through every set of observed speeds
harmonic <- c()
calc_hmean <- function(rep_no){
  # format speeds per input rep number
  s1 <- seq_dats[,rep_no]
  s2 <- s1$observed
  s3 <- s2[!is.nan(s2)]
  # work out hmean for the set of speeds for this input rep number
  harmonic <- c(harmonic, hmean(s3))
  return(harmonic)
}

harmonics <- sapply(c(1:length(speeds)), calc_hmean)




## SIZE-BIASED MODELS:

# 1. fit all the models

## mods_all_fit
# fits all 3 SBMs (lognormal, gamma, Weibull) to each set of observed speeds (i.e. each simulation rep)
# return three models for each set of measured speeds
# INPUT:
# number of reps of the simulation
mods_all_fit <- function(rep_no){
  # format speeds per input rep number
  s1 <- seq_dats[,rep_no]
  s2 <- s1$observed
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

# -- not needed currently
# -- see extra code at bottom for this

# 3. predict average speed using the models

## predict_lnorm
# predict average speed using a fitted lognormal model for each set of observed speeds (i.e. each simulation rep)
# INPUT:
# number of reps of the simulation
predict_lnorm <- function(rep_no){
  predict.sbm(mods[[1,rep_no]]$lnorm)[1] # selects just the estimate of speed
  # Q: default is newdata = NULL - what does this actually mean?
}

## predict_gamma
# predict average speed using a fitted gamma model for each set of observed speeds (i.e. each simulation rep)
# INPUT:
# number of reps of the simulation
predict_gamma <- function(rep_no){
  predict.sbm(mods[[1,rep_no]]$gamma)[1]
}

## predict_weibull
# predict average speed using a fitted Weibull model for each set of observed speeds (i.e. each simulation rep)
# INPUT:
# number of reps of the simulation
predict_weibull <- function(rep_no){
  predict.sbm(mods[[1,rep_no]]$weibull)[1]
}

mods_predict_lnorm <- sapply(c(1:length(speeds)), predict_lnorm)
mods_predict_gamma <- sapply(c(1:length(speeds)), predict_gamma)
mods_predict_weibull <- sapply(c(1:length(speeds)), predict_weibull)


# 4. extract and store the AICs for each model

# mods is in such a bizarre format that the best way I could come up with was this function: (couldn't get the AIC.sbm function working)

## lnorm_AIC_extract
# return the AIC for the fitted lognormal model for each set of observed speeds
# INPUT:
# number of reps of the simulation
lnorm_AIC_extract <- function(rep_no){
  a1 <- mods[2,rep_no]
  a2 <- a1$AICtab[1]
  a2["lnorm",]
}

## gamma_AIC_extract
# return the AIC for the fitted gamma model for each set of observed speeds
# INPUT:
# number of reps of the simulation
gamma_AIC_extract <- function(rep_no){
  a1 <- mods[2,rep_no]
  a2 <- a1$AICtab[1]
  a2["gamma",]
}

## weibull_AIC_extract
# return the AIC for the fitted Weibull model for each set of observed speeds
# INPUT:
# number of reps of the simulation
weibull_AIC_extract <- function(rep_no){
  a1 <- mods[2,rep_no]
  a2 <- a1$AICtab[1]
  a2["weibull",]
}

lnorm_AICs <- sapply(c(1:length(speeds)), lnorm_AIC_extract)
gamma_AICs <- sapply(c(1:length(speeds)), gamma_AIC_extract)
weibull_AICs <- sapply(c(1:length(speeds)), weibull_AIC_extract)



## dataframe with speed parameter and estimated speeds
speeds_df <- data.frame(speed_parameter = exp(speeds),
                        hmean = harmonics[1,],
                        lnorm = as.numeric(mods_predict_lnorm),
                        gamma = as.numeric(mods_predict_gamma),
                        weibull = as.numeric(mods_predict_weibull))


# separate df with model AICs
model_AICs <- data.frame(input = exp(speeds),
                         lnorm = as.numeric(mods_predict_lnorm),
                         lnormAIC = lnorm_AICs,
                         gamma = as.numeric(mods_predict_gamma),
                         gammaAIC = gamma_AICs,
                         weibull = as.numeric(mods_predict_weibull),
                         weibullAIC = weibull_AICs)



#### PLOTS ####

# calculate errors between realised and estimated speeds:
hmean_error_real_calc <- function(rep_no){
  as.numeric(harmonics[1, rep_no]) - seq_dats[,rep_no]$realised[1] #-- negative == means the estimated speed is smaller than the realised speed
}
hmean_error_real <- sapply(c(1:length(speeds)), hmean_error_real_calc)

lnorm_error_real_calc <- function(rep_no){
  as.numeric(mods_predict_lnorm[rep_no]) - seq_dats[,rep_no]$realised[1]
}
lnorm_error_real <- sapply(c(1:length(speeds)), lnorm_error_real_calc)

gamma_error_real_calc <- function(rep_no){
  as.numeric(mods_predict_gamma[rep_no]) - seq_dats[,rep_no]$realised[1]
}
gamma_error_real <- sapply(c(1:length(speeds)), gamma_error_real_calc)

weibull_error_real_calc <- function(rep_no){
  as.numeric(mods_predict_weibull[rep_no]) - seq_dats[,rep_no]$realised[1]
}
weibull_error_real <- sapply(c(1:length(speeds)), weibull_error_real_calc)


# error between speed parameter and estimated speeds:
hmean_error_sp_calc <- function(rep_no){
  as.numeric(harmonics[1, rep_no]) - exp(speed_parameter)
}
hmean_error_sp <- sapply(c(1:length(speeds)), hmean_error_sp_calc)

lnorm_error_sp_calc <- function(rep_no){
  as.numeric(mods_predict_lnorm[rep_no]) - exp(speed_parameter)
}
lnorm_error_sp <- sapply(c(1:length(speeds)), lnorm_error_sp_calc)

gamma_error_sp_calc <- function(rep_no){
  as.numeric(mods_predict_gamma[rep_no]) - exp(speed_parameter)
}
gamma_error_sp <- sapply(c(1:length(speeds)), gamma_error_sp_calc)

weibull_error_sp_calc <- function(rep_no){
  as.numeric(mods_predict_weibull[rep_no]) - exp(speed_parameter)
}
weibull_error_sp <- sapply(c(1:length(speeds)), weibull_error_sp_calc)


# PLOT A: errors between observed speeds and either realised speed or the speed parameter

# work out errors between observed and realised speeds:
obs_realised_error_calc <- function(rep_no){
  realised <- seq_dats[,rep_no]$realised[1]
  work_out_diff <- function(observed){
    observed - realised
  }
  sapply(seq_dats[,rep_no]$observed, work_out_diff)
}
obs_realised_errors <- sapply(c(1:length(speeds)), obs_realised_error_calc)
obs_realised_errors_vector <- c()
for (i in c(1:n)){
  x <- as.numeric(unlist(obs_realised_errors[i]))
  obs_realised_errors_vector <- c(obs_realised_errors_vector, x)
}
obs_realised_errors_vector <- obs_realised_errors_vector[!is.nan(obs_realised_errors_vector)]


# work out errors between observed speeds and the speed parameter:
obs_param_error_calc <- function(rep_no){
  work_out_diff <- function(observed){
    observed - exp(speed_parameter)
  }
  sapply(seq_dats[,rep_no]$observed, work_out_diff)
}
obs_param_errors <- sapply(c(1:length(speeds)), obs_param_error_calc)
obs_param_errors_vector <- c()
for (i in c(1:n)){
  x <- as.numeric(unlist(obs_param_errors[i]))
  obs_param_errors_vector <- c(obs_param_errors_vector, x)
}
obs_param_errors_vector <- obs_param_errors_vector[!is.nan(obs_param_errors_vector)]


# make a vector with all the observed speeds:
observed_all <- c()
for (i in c(1:n)){
  x <- seq_dats[,i]$observed
  observed_all <- c(observed_all, x)
}
observed_all <- observed_all[!is.nan(observed_all)]
# observed_all_df <- data.frame(speed = observed_all)
# 
# observed_distr <- ggplot(observed_all_df, aes(x = speed))+
#   geom_density()+
#   theme_minimal()+
#   geom_vline(xintercept = speed_parameter, colour = "blue", linetype = "dashed")+
#   labs(title = paste("Distribution of observed speeds relative to the speed parameter\n(dashed blue line = speed parameter = ", exp(speed_parameter), "m/s)"))
# #observed_distr
# 
# # - would be nice to add the distribution of realised speeds into this: make separate plot in case it makes everything look too messy

# make vector of all the realised speeds
realised_all <- c()
for (i in c(1:n)){
  x <- seq_dats[,i]$realised[1]
  realised_all <- c(realised_all, x)
}
realised_all <- realised_all[!is.nan(realised_all)]


# obs_real_all_df <- data.frame(speed = c(observed_all, realised_all),
#                               measure = c(rep("observed", length(observed_all)), rep("realised", length(realised_all))))
# 
# obs_real_plot <- ggplot(obs_real_all_df, aes(x = speed, fill = measure))+
#   geom_density()+
#   theme_minimal()+
#   geom_vline(xintercept = speed_parameter, linetype = "dashed", colour = "blue")+
#   labs(title = paste("Distribution of observed speeds relative to realised speeds and the speed parameter\n(dashed blue line = speed parameter = ", exp(speed_parameter), "m/s)"))
# obs_real_plot
# 
# 
# #### save the main plots ####
# 
# png(file = paste("plots/simulation_outputs/sp_", exp(speed_parameter), "_n_", n, ".png", sep = ""),
#     width = 600, height = 1000)
# plots_arranged <- ggarrange(errors_plot, box, medians_plot, labels = c("A", "B", "C"), nrow = 3)
# annotate_figure(plots_arranged, top = paste("Speed parameter = ", exp(speed_parameter), " m/s", sep = ""))
# dev.off()
# 
# png(file = paste("plots/obs_bias/sp_", exp(speed_parameter), "_n_", n, ".png", sep = ""),
#     width = 600, height = 1000)
# obs_arranged <- ggarrange(obs_error_plot, obs_real_plot, labels = c("A", "B"), nrow = 2)
# annotate_figure(obs_arranged, top = paste("Speed parameter = ", exp(speed_parameter), " m/s", sep = ""))
# dev.off()







### 2 plots showing observed speeds relative to realised speeds and the speed_parameter

obs_errors_df <- data.frame(error = c(obs_realised_errors_vector, obs_param_errors_vector),
                            measure = c(rep("realised", length(obs_realised_errors_vector)), rep("speed parameter", length(obs_param_errors_vector))))


# plot showing the error between observed speeds and either realised speeds or speed_parameter
obs_error_plot <- ggplot(obs_errors_df, aes(x = error, fill = measure))+
  geom_density(size = 0.6)+
  facet_grid(rows = vars(measure))+
  #theme_minimal()+
  geom_vline(xintercept = 0)+
  labs(title = "Errors between observed speeds and either\nrealised speeds or the speed parameter",
       x = "speed (m/s)")+
  theme(legend.position = "none")


obs_real_all_df <- data.frame(speed = c(observed_all, realised_all),
                              measure = c(rep("observed", length(observed_all)), rep("realised", length(realised_all))))

# plot showing the speed parameter, realised speeds, and observed speeds relative to each other:
obs_real_plot <- ggplot(obs_real_all_df, aes(x = speed, fill = measure))+
  geom_density()+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_vline(xintercept = exp(speed_parameter), linetype = "dashed", colour = "blue")+
  labs(title = paste("Distribution of observed speeds relative to\nrealised speeds and the speed parameter\n(dashed blue line = speed parameter = ", exp(speed_parameter), "m/s)"),
       x = "speed (m/s)")



### 2 plots showing estimated speeds relative to realised speeds and the speed parameter:

estimated_error_df <- data.frame(error = c(hmean_error_real, lnorm_error_real, gamma_error_real, weibull_error_real, hmean_error_sp, lnorm_error_sp, gamma_error_sp, weibull_error_sp),
                                 method = c(rep("hmean", length(hmean_error_real)), rep("SBM_lnorm", length(lnorm_error_real)), rep("SBM_gamma", length(gamma_error_real)), rep("SBM_weibull", length(weibull_error_real)), rep("hmean", length(hmean_error_sp)), rep("SBM_lnorm", length(lnorm_error_sp)), rep("SBM_gamma", length(gamma_error_sp)), rep("SBM_weibull", length(weibull_error_sp))),
                                 measure = c(rep("realised", length(c(hmean_error_real, lnorm_error_real, gamma_error_real, weibull_error_real))), rep("speed parameter", length(c(hmean_error_sp, lnorm_error_sp, gamma_error_sp, weibull_error_sp)))))

# plot showing the errors between estimated speeds and either realised speeds or the speed_parameter
estimated_error_plot <- ggplot(estimated_error_df, aes(x = error, fill = method))+
  geom_density(alpha = 0.3)+
  theme(legend.position = "bottom")+
  facet_grid(rows = vars(measure))+
  labs(title = "Error between estimated speeds and either\nrealised speeds or the speed parameter",
       x = "speed (m/s)")

box_df <- data.frame(speed = c(harmonics[1,], as.numeric(mods_predict_lnorm), as.numeric(mods_predict_gamma), as.numeric(mods_predict_weibull)),
                     method = c(rep("hmean", length(harmonics[1,])), rep("SBM_lnorm", length(as.numeric(mods_predict_lnorm))), rep("SBM_gamma", length(as.numeric(mods_predict_gamma))), rep("SBM_weibull", length(mods_predict_weibull))))

# plot showing estimated speeds relative to the speed_parameter
box <- ggplot(box_df, aes(x = method, y = speed))+ 
  geom_boxplot()+
  geom_hline(yintercept = exp(speed_parameter), colour = "blue", linetype = "dashed")+
  #geom_text(aes(0.3, (exp(speed_parameter)), label = paste("speed parameter = ", signif(exp(speed_parameter), digits = 3), "m/s"), vjust = -1), size = 3, colour = "blue")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position="none")+
  labs(y = "speed (m/s)", title = paste("Estimated speeds relative to speed parameter\n(dashed blue line = speed parameter = ", exp(speed_parameter), "m/s)"))



## save all 4 plots in one png:

png(file = paste("plots/sim_4outputs/sp_", exp(speed_parameter), "_n_", n, "_nstep_", step_no, ".png", sep = ""),
    width = 900, height = 1000)
plots_arranged <- ggarrange(obs_error_plot, estimated_error_plot, obs_real_plot, box, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
annotate_figure(plots_arranged, top = paste("Speed parameter = ", exp(speed_parameter), " m/s, n = ", n, ", step_no = ", step_no, sep = ""))
dev.off()




# extra code not currently needed -----------------------------------------

### run for 1 speed once through

# define one input average speed:
#seq_dats <- sapply(-3, seq_dat)
# when apply it to just one speed: speeds are in seq_dats[4] (although there are NaNs!)



### run for multiple different speeds once through each 

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








##### plotting the SBM fits

# want to do this for each mods[1], mods[3] etc - multipanel plot with all 3 models for each

## plot_all
# plots all three models in one multi-panel plot for each set of measured speeds (i.e. for each rep of the simulation)
# INPUT:
# number of reps of the simulation - so that can loop through every set of measured speeds
# plot_all <- function(rep_no){
#   layout(matrix(1:3, ncol=3))
#   par(oma=c(4, 0, 4, 0), mar=c(4, 4, 4, 4))
#   plot.sbm(mods[[1,rep_no]]$lnorm, title = "lnorm") # default in plot.sbm is to plot the distribution on a log scale - Q: could we discuss this to better get my head around it please
#   plot.sbm(mods[[1,rep_no]]$gamma, title = "gamma")
#   plot.sbm(mods[[1,rep_no]]$weibull, title = "Weibull")
#   title(main=paste("Input speed = ", exp(speeds[rep_no]), sep = ""), outer=TRUE, cex.main=2)
# }
# 
# sbm_plots <- lapply(c(1:length(speeds)), plot_all) 


# --> the higher the speed, the more bins and the nicer the fit looks






#### previous plotting stuff:

# simulated input speeds:
# concatenate all 100 sets of measured speeds:

# measured_speeds <- c()
# # format speeds per input rep number
# 
# for (i in 1:length(speeds)){
#   s1 <- seq_dats[i]
#   s2 <- s1$measured
#   s3 <- s2[!is.nan(s2)]
#   
#   measured_speeds <- c(measured_speeds, s3)
# }
# 
# 
# # make box and whisker plot comparing measured_speeds, hmean averages, and each SBM average:
# 
# # make df:
# # bc of different lengths: need to fill the tails of some columns with NAs
# list_all <- list(measured_speeds, harmonics[1,], as.numeric(mods_predict_lnorm), as.numeric(mods_predict_gamma), SBM_weibull = as.numeric(mods_predict_weibull))
# len <- max(lengths(list_all))
# cols <- lapply(list_all, function(l) c(l, rep(NA, len - length(l))))
# boxplot_df <- as.data.frame(Reduce(cbind, cols, init = NULL))
# colnames(boxplot_df) <- c("measured", "hmean", "SBM_lnorm", "SBM_gamma", "SBM_weibull")
# 
# # make it in a different format - better for plotting
# box_df <- data.frame(measure = c(rep("measured", length(measured_speeds)), 
#                                  rep("hmean", length(harmonics[1,])), 
#                                  rep("SBM_lnorm", length(as.numeric(mods_predict_lnorm))), 
#                                  rep("SBM_gamma", length(as.numeric(mods_predict_gamma))), 
#                                  rep("SBM_weibull", length(as.numeric(mods_predict_weibull)))),
#                      speed = c(measured_speeds, 
#                                harmonics[1,], 
#                                as.numeric(mods_predict_lnorm), 
#                                as.numeric(mods_predict_gamma), 
#                                as.numeric(mods_predict_weibull)))
# 
# 
# # set specific order to make plot clearer
# box_df$measure <- factor(box_df$measure , levels=c("SBM_weibull", "SBM_gamma", "SBM_lnorm","hmean", "measured"))
# 
# # plot:
# box <- ggplot(box_df, aes(x = measure, y = speed, fill = measure))+ 
#   geom_boxplot(notch = TRUE)+
#   coord_flip()+
#   theme_minimal()+
#   theme(legend.position="none")+
#   labs(y = "speed (m/s)")
#   
# 
# ## -- looks like high speeds get missed by all the methods?
# 
# 
# # remove some of the high measured values to better compare the distributions:
# box_df2 <- box_df[box_df$speed < 4*exp(speed_parameter), ]
# 
# box_minus_high_measured_speeds <- ggplot(box_df2, aes(x = measure, y = speed, fill = measure))+ 
#   geom_boxplot(notch = TRUE)+
#   coord_flip()+
#   theme_minimal()+
#   theme(legend.position="none")+
#   labs(y = "speed (m/s)")



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
# 
# averages_df <- data.frame(speed = c(median(measured_speeds),
#                                     mean(measured_speeds),
#                                     median(harmonics[1,]),
#                                     mean(harmonics[1,]),
#                                     median(as.numeric(mods_predict_lnorm)),
#                                     mean(as.numeric(mods_predict_lnorm)),
#                                     median(as.numeric(mods_predict_gamma)),
#                                     mean(as.numeric(mods_predict_gamma)),
#                                     median(as.numeric(mods_predict_weibull)),
#                                     mean(as.numeric(mods_predict_weibull))),
#                           measure = c(rep("measured", length(2)), rep("hmean", length(2)), rep("SBM_lnorm", length(2)), rep("SBM_gamma", length(2)), rep("SBM_weibull", length(2))),
#                           average = c(rep(c("median", "mean"), length(5))))
# 
# averages_df$measure <- factor(averages_df$measure , levels=c("measured", "hmean", "SBM_lnorm", "SBM_gamma", "SBM_weibull"))
# 
# averages_plot <- ggplot(averages_df, aes(x = measure, y = speed))+
#   geom_point()+
#   facet_grid(rows = vars(average))






### previous plotting attempts:

# 
# errors_df <- data.frame(error = c(hmean_error_real, lnorm_error_real, gamma_error_real, weibull_error_real),
#                         method = c(rep("hmean", length(hmean_error_real)), rep("SBM_lnorm", length(lnorm_error_real)), rep("SBM_gamma", length(gamma_error_real)), rep("SBM_weibull", length(weibull_error_real))))
# 
# errors_plot <- ggplot(errors_df, aes(x = error, fill = method))+
#   geom_density(alpha = 0.3)+
#   theme_minimal()+
#   labs(title = paste("Error between realised and estimated speeds\n(speed parameter =", signif(exp(speed_parameter), digits = 3), "m/s)"),
#        x = "error (m/s)")
# errors_plot

# 
# # boxplot of all 100 of the estimated speeds with line showing speed parameter:
# 
# box_df <- data.frame(speed = c(harmonics[1,], as.numeric(mods_predict_lnorm), as.numeric(mods_predict_gamma), as.numeric(mods_predict_weibull)),
#                      method = c(rep("hmean", length(harmonics[1,])), rep("SBM_lnorm", length(as.numeric(mods_predict_lnorm))), rep("SBM_gamma", length(as.numeric(mods_predict_gamma))), rep("SBM_weibull", length(mods_predict_weibull))))
# 
# box <- ggplot(box_df, aes(x = method, y = speed))+ 
#   geom_boxplot()+
#   geom_hline(yintercept = exp(speed_parameter), colour = "blue", linetype = "dashed")+
#   #geom_text(aes(0.3, (exp(speed_parameter)), label = paste("speed parameter = ", signif(exp(speed_parameter), digits = 3), "m/s"), vjust = -1), size = 3, colour = "blue")+
#   coord_flip()+
#   theme_minimal()+
#   theme(legend.position="none")+
#   labs(y = "speed (m/s)", title = paste("Estimated speeds relative to speed parameter\n(dashed blue line = speed parameter = ", exp(speed_parameter), "m/s)"))
# #box
# 
# 
# # comparison of medians of estimated speeds with the speed parameter
# 
# medians_df <- data.frame(speed = c(median(harmonics[1,]),
#                                           median(as.numeric(mods_predict_lnorm)),
#                                           median(as.numeric(mods_predict_gamma)),
#                                           median(as.numeric(mods_predict_weibull))),
#                          method = c("hmean", "SBM_lnorm", "SBM_gamma", "SBM_weibull"))
# 
# medians_df$method <- factor(medians_df$method , levels=c("hmean", "SBM_lnorm", "SBM_gamma", "SBM_weibull"))
# 
# medians_plot <- ggplot(medians_df, aes(x = method, y = speed))+
#   geom_point(shape = 18, size = 3)+
#   theme_minimal()+
#   ylab("speed (m/s)")+
#   coord_flip()+
#   geom_hline(yintercept = exp(speed_parameter), colour = "blue", linetype = "dashed")+
#   #geom_text(aes(0.5, (exp(speed_parameter)-0.02), label = paste("speed parameter = ", signif(exp(speed_parameter), digits = 3), "m/s"), vjust = -1), size = 3, colour = "blue")+
#   labs(title = paste("Medians of estimated speeds relative to speed parameter\n(dashed blue line = speed parameter = ", exp(speed_parameter), "m/s)"))
# #medians_plot
# 
# #ggarrange(errors_plot, box, medians_plot, nrow = 3)
# 


# # plot with just realised errors
# 
# # obs_realised_errors_df <- data.frame(error = obs_realised_errors_vector)
# # 
# # obs_realised_error_plot <- ggplot(obs_realised_errors_df, aes(x = error))+
# #   geom_density(size = 0.6)+
# #   theme_minimal()+
# #   geom_vline(xintercept = 0, colour = "red")+
# #   labs(title = "Errors between observed and realised speeds")
# # obs_realised_error_plot  
# 


# # plot for both realised and speed_parameter
# 
# obs_errors_df <- data.frame(error = c(obs_realised_errors_vector, obs_param_errors_vector),
#                             measure = c(rep("realised_speed", length(obs_realised_errors_vector)), rep("speed_parameter", length(obs_param_errors_vector))))
# 
# obs_error_plot <- ggplot(obs_errors_df, aes(x = error, fill = measure))+
#   geom_density(size = 0.6)+
#   #facet_grid(rows = vars(measure))+
#   theme_minimal()+
#   geom_vline(xintercept = 0, colour = "red")+
#   labs(title = "Errors between observed speeds and either realised speeds or the speed parameter")
# #obs_error_plot 
# 