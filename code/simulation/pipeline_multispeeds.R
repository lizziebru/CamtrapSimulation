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
seq_dat <- function(speed_parameter, step_no) { 
  path <- pathgen(n=step_no, kTurn=2, kCor=TRUE, pTurn=1, logspeed=speed_parameter, speedSD=1, speedCor=0, xlim=c(0,20), ylim=c(2,16), wrap=TRUE)
  dz <- data.frame(x=10, y=5, r=10, th=1.65, dir=0) # set radius to 10m and theta to 1.65 - based on distributions of radii & angles in regent's park data
  # make plot
  #p <- plot_wrap(path, lineargs = list(col="grey"))
  #plot_dzone(dz, border=2)
  
  # generate position data
  posdat <- sequence_data(path, dz)
  
  # add points to plot
  #points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
  
  # work out speeds of sequences
  v <- calc_speed(posdat)
  
  
  # work out trap rate: --> DON'T USE FOR NOW 
  # ## = no. of points captured / total distance travelled
  # n_points <- nrow(posdat)
  # # total distance: sum of distances between each set of x and y coords:
  # dists <- c()
  # for (i in 1:(nrow(path$path)-1)){
  #   p <- path$path
  #   d <- sqrt((p$x[i+1]-p$x[i])^2 + (p$y[i+1]-p$y[i])^2)
  #   dists <- c(dists, d)
  # }
  # total_dist <- sum(dists)
  # # trap rate:
  # trap_rate <- n_points/total_dist
  
  
  ## realised speeds: get a set of them that are equivalent to the average lengths of the sequence IDs
  
  # store the length of the observed speed sequences:
  obs_lengths <- c()
  for (i in 1:length(unique(posdat$sequenceID))){
    p <- posdat[posdat$sequenceID==i,]
    obs_lengths <- c(obs_lengths, nrow(p))
  }
  
  # use the mean of these lengths as the number of position data points to use in realised speed segments
  r_lengths <- round(mean(obs_lengths))
  
  # in path$speed, select sets of speeds of length r_lengths:
  extract_realised <- function(realised_speeds, r_lengths){ # function to extract one set of realised speeds
    firstIndex <- sample(seq(length(realised_speeds) - r_lengths + 1), 1)
    realised_speeds[firstIndex:(firstIndex + r_lengths -1)]
  }
  
  realised_speeds <- c()
  realised_spds <- replicate(length(v$speed),{
    realised_speeds <- c(realised_speeds, mean(extract_realised(path$speed, r_lengths)))
  })
  rs <- replicate(length(v$speed),{
    mean(extract_realised(path$speed, r_lengths))
  })
  
  # problem: some of the realised speeds are randomly really high...
  # maybe get rid of those and resample
  
  
  
  ### number of single-frame sequences:
  # count the number of single-occurring numbers in the sequenceID column of posdat:
  t <- data.frame(table(posdat$sequenceID))
  n_singles <- nrow(td[td$Freq==1,])
  
  
  ### number of zero-frame sequences:
  # select pairs of consecutive points on the path that aren't in posdat (i.e. don't fall into the detection zone) - but also make sure they're not points in between which the animal went round the back of the torus (define this as having a distance between them that's greater than half of the width of the space?)
  # draw a straight line between them and sample 100 points on that line
  # if the coordinates of any one of those points lies in the coordinate space of the detection zone: count it as a zero-frame sequence
  
  
  
  
  
  ### return realised speeds, observed speeds, no. of single frames, and no. of zero frames
  df <- data.frame(realised = realised_spds,
                   observed = v$speed,
                   n_singles = c(rep(n_singles, length(v$speed))))
                   #trap_rate = rep(trap_rate, length(v$speed)))

  return(df)
}



#### run on all the speeds in the speed_parameter ####


seq_dats <- sapply(speed_parameter, seq_dat, step_no = step_no)

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

#seq_dats <- mcsapply(speed_parameter, seq_dat, mc.cores = 4, step_no = step_no)





# PLOTS FOR DISTRIBUTION OF REALISED SPEEDS -------------------------------

real_spds_distr <- ggplot()+
  geom_density(aes(x = path$speed))+
  theme_minimal()+
  labs(title = "Distribution of realised speeds for input speed parameter = 0.5m/s",
       x = "Realised speed (m/s)")
real_spds_distr

png(file="plots/real_spds_distr.png",
    width=700, height=600)
real_spds_distr
dev.off()

# some of the realised speeds are way too high - have asked Marcus about these but probs just ignore


# PLOTS FOR BIAS #1  --------------------------------------

#--> NB: probs re-do these once have taken care of weirdly high realised speeds

## PLOT: bias1_1.png
# distribution of observed speeds vs realised speeds:

bias1_plots_list <- list()
for (i in 1:length(speed_parameter)){
  s <- seq_dats[,i]
  df <- data.frame(speed = c(s$realised, s$observed),
                   obs_real = c(rep("realised", length(s$realised)), rep("observed", length(s$observed))))
  bias1_plots_list[[i]] <- ggplot(df, aes(x = speed, colour = obs_real))+
    geom_density(size = 1)+
    theme_minimal()+
    theme(legend.title = element_blank(),
          legend.position = "bottom")+
    scale_colour_manual(values = c("blue", "red"))+
    xlab("speed (m/s)")+
    labs(title = paste("speed parameter = ", exp(speed_parameter[i]), " m/s"))
}



nrow_p <- length(speed_parameter)/5
ncol_p <- length(speed_parameter)/nrow_p 

bias1_arranged <- marrangeGrob(grobs=bias1_plots_list, nrow=nrow_p, ncol=ncol_p)

ggsave(filename = "plots/bias1_1.png", plot = bias1_arranged, height = 10, width = 30)


# PLOT: bias1_2.png
# error between observed & mean realised speed against mean realised speed:

obs_real_error <- c()
mean_realised_speeds <- c()
for (i in 1:length(speed_parameter)){
  o <- seq_dats[,i]$observed
  o <- o[!is.na(o)]
  r <- mean(seq_dats[,i]$realised)
  mean_realised_speeds <- c(mean_realised_speeds, r)
  errors <- c()
  for (j in 1:length(s)){
    e <- o[j] - r
    errors <- c(errors, e)
    errors_mean <- mean(errors)
  }
  obs_real_error <- c(obs_real_error, errors_mean)
}

bias1_errors_df <- data.frame(error = obs_real_error,
                              mean_realised = mean_realised_speeds)

bias1_errors_plot <- ggplot(bias1_errors_df, aes(x = mean_realised, y = error))+
  geom_point(size = 2, colour = "blue")+
  theme_minimal()+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15))+
  labs(x = "mean realised speed (m/s)", y = "error (m/s)")
bias1_errors_plot

png(file="plots/bias1_2.png",
    width=700, height=600)
bias1_errors_plot
dev.off()


## PLOT: bias1_4.png
# plot to show how the detection probability of a given speed varies depending on its value
# method to work out detection probability:
# for each set of realised speeds (1 set per input speed parameter):
# - take each realised speed
# - at that speed: what's the probability density of the observed speed that has the same value (need to interpolate in the observed speed density curve)
real_sp <- c()
det_probs <- c()
for (i in 1:length(speed_parameter)){
  s <- seq_dats[,i]
  r <- s$realised
  o <- s$observed
  o <- o[!is.na(o)]
  for (i in 1:length(r)){
    real_sp <- c(real_sp, r[i])
    o_interp <- approxfun(density(o)) # interpolation function from the density of observed speeds
    r_detect_prob <- o_interp(r[i]) # use this function to estimate detection probability of a given realised speed
    det_probs <- c(det_probs, r_detect_prob)
  }
}

det_probs_df <- data.frame(speed = real_sp,
                           detection_prob = det_probs)

det_probs_plot <- ggplot(det_probs_df, aes(x = speed, y = detection_prob))+
  geom_point()+
  theme_minimal()
det_probs_plot

# for now: get rid of speeds greater than 10m/s:
det_probs_df2 <- det_probs_df[det_probs_df$speed<10,]

det_probs_plot <- ggplot(det_probs_df2, aes(x = speed, y = detection_prob))+
  geom_point()+
  theme_minimal()+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15))+
  labs(y = "Detection probability",
       x = "speed (m/s)")
det_probs_plot

png(file="plots/bias1_4.png",
    width=700, height=600)
det_probs_plot
dev.off()




# make dataframe for all observed and realised speeds for each speed parameter - give up on this for now - but if you need it could just make separate vectors for each of obs, real, & sps and concatenate them together into a dataframe
# columns <- c("observed","realised", "speed_parameter") # main purpose of having the speed parameter in here is to section out the sets of realised and observed speeds
# # number of rows: select the seq_dats[,i]$realised that has the longest length and use that multiplied by the speed parameter:
# realised_lengths <- c()
# for (i in 1:length(speed_parameter)){
#   r <- seq_dats[,i]$realised
#   realised_lengths <- c(realised_lengths, length(r))
# }
# no_rows <- max(realised_lengths) * length(speed_parameter) 
# obs_real_df <- data.frame(matrix(nrow = 0, ncol = length(columns)))
# colnames(obs_real_df) <- columns
# 
# for (i in 1:length(speed_parameter)){
#   o <- seq_dats[,i]$observed
#   r <- seq_dats[,i]$realised
#   sp <- exp(speed_parameter[i])
#   sps <- c(rep(sp, length(o)))
#   obs_real_df$observed <- c(obs_real_df$observed, o)
#   obs_real_df$realised <- c(obs_real_df$realised, r)
#   obs_real_df$speed_parameter <- c(obs_real_df$speed_parameter, sps)
# }






## PLOT: bias1_3.png
# plot to see how this bias affects our overall ability to estimate speeds: plot error between obs and mean realised against error between mean realised and estimated 
# (i.e. how good are the SBMs at correcting for this bias?)

# different coloured line for each type of speed estimation:

est_real_error <- c()
mean_realised_speeds <- c()
for (i in 1:length(speed_parameter)){
  o <- seq_dats[,i]$observed
  o <- o[!is.na(o)]
  r <- mean(seq_dats[,i]$realised)
  mean_realised_speeds <- c(mean_realised_speeds, r)
  errors <- c()
  for (j in 1:length(s)){
    e <- o[j] - r
    errors <- c(errors, e)
    errors_mean <- mean(errors)
  }
  obs_real_error <- c(obs_real_error, errors_mean)
}


bias1_effects_df <- data.frame(error_obs_real = c(rep(obs_real_error, length(4))),
                               error_est_real = c(hmean_error_real, lnorm_error_real, gamma_error_real, weibull_error_real),
                               method = c(rep("harmonic mean", length(hmean_error_real)), rep("SBM lnorm", length(lnorm_error_real)), rep("SBM gamma", length(gamma_error_real)), rep("SBM weibull", length(weibull_error_real))))

bias1_effects_plot <- ggplot(bias1_effects_df, aes(x = error_obs_real, y = error_est_real, colour = method))+
  geom_point(size = 2)+
  theme_minimal()+
  theme(axis.title = element_text(size=18),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17),
        axis.text = element_text(size = 15))+
  labs(x = "mean error between observed speeds and mean realised speeds (m/s)",
       y = "mean error between estimated speeds and mean realised speeds (m/s)")
bias1_effects_plot

png(file="plots/bias1_3.png",
    width=700, height=650)
bias1_effects_plot
dev.off()












# PLOTS FOR EFFECTS OF BIAS #1 --------------------------------------------

## PLOT: bias2_1.png
# number of single- and zero-frame sequences against mean realised speed - to see how many crossings get missed






## PLOT: bias2_2.png
# number of single- and zero-frame sequences against error between realised and estimated speed - to see the effect of this bias on our estimations of speed












# PLOTS FOR BIAS #2 -------------------------------------------------------

# NEXT STEPS:

# figure out how to extract the number of paths that cross without getting detected and the number of paths that just get detected by a single frame
# --> that'll help show high speeds getting missed by CTs

# then linking those two biases to the resulting discrepancy between estimated speed and speed parameter would be helpful to demonstrate how the biases are affecting our way of estimating average speed of movement 








##### calculate estimated speeds - using hmean and SBMs #####

# apply each method (hmean, SBMlog, SBMgamma, SBMWeibull) to each speed parameter

## HARMONIC MEAN:

## calc_hmean
# work out harmonic mean of a set of observed speeds (i.e. each simulation rep)
# returns a harmonic mean and standard error for each set of speeds
# INPUT:
# number of reps of the simulation - so that can loop through every set of observed speeds
harmonic <- c()
calc_hmean <- function(speed_no){
  # format speeds per input rep number
  s1 <- seq_dats[,speed_no]
  s2 <- s1$observed
  s3 <- s2[!is.nan(s2)]
  # work out hmean for the set of speeds for this input rep number
  harmonic <- c(harmonic, hmean(s3))
  return(harmonic)
}

harmonics <- sapply(c(1:length(speed_parameter)), calc_hmean)




## SIZE-BIASED MODELS:

# 1. fit all the models

## mods_all_fit
# fits all 3 SBMs (lognormal, gamma, Weibull) to each set of observed speeds (i.e. each simulation rep)
# return three models for each set of measured speeds
# INPUT:
# number of reps of the simulation
mods_all_fit <- function(speed_no){
  # format speeds per input rep number
  s1 <- seq_dats[,speed_no]
  s2 <- s1$observed
  s3 <- s2[!is.nan(s2)]
  # make df:
  df <- data.frame(speed = s3)
  # fit all three models:
  sbm3(speed~1, df)
}

mods <- sapply(c(1:length(speed_parameter)), mods_all_fit)
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
predict_lnorm <- function(speed_no){
  predict.sbm(mods[[1,speed_no]]$lnorm)[1] # selects just the estimate of speed
  # Q: default is newdata = NULL - what does this actually mean?
}

## predict_gamma
# predict average speed using a fitted gamma model for each set of observed speeds (i.e. each simulation rep)
# INPUT:
# number of reps of the simulation
predict_gamma <- function(speed_no){
  predict.sbm(mods[[1,speed_no]]$gamma)[1]
}

## predict_weibull
# predict average speed using a fitted Weibull model for each set of observed speeds (i.e. each simulation rep)
# INPUT:
# number of reps of the simulation
predict_weibull <- function(speed_no){
  predict.sbm(mods[[1,speed_no]]$weibull)[1]
}

mods_predict_lnorm <- sapply(c(1:length(speed_parameter)), predict_lnorm)
mods_predict_gamma <- sapply(c(1:length(speed_parameter)), predict_gamma)
mods_predict_weibull <- sapply(c(1:length(speed_parameter)), predict_weibull)


# 4. extract and store the AICs for each model

# mods is in such a bizarre format that the best way I could come up with was this function: (couldn't get the AIC.sbm function working)

## lnorm_AIC_extract
# return the AIC for the fitted lognormal model for each set of observed speeds
# INPUT:
# number of reps of the simulation
lnorm_AIC_extract <- function(speed_no){
  a1 <- mods[2,speed_no]
  a2 <- a1$AICtab[1]
  a2["lnorm",]
}

## gamma_AIC_extract
# return the AIC for the fitted gamma model for each set of observed speeds
# INPUT:
# number of reps of the simulation
gamma_AIC_extract <- function(speed_no){
  a1 <- mods[2,speed_no]
  a2 <- a1$AICtab[1]
  a2["gamma",]
}

## weibull_AIC_extract
# return the AIC for the fitted Weibull model for each set of observed speeds
# INPUT:
# number of reps of the simulation
weibull_AIC_extract <- function(speed_no){
  a1 <- mods[2,speed_no]
  a2 <- a1$AICtab[1]
  a2["weibull",]
}

lnorm_AICs <- sapply(c(1:length(speed_parameter)), lnorm_AIC_extract)
gamma_AICs <- sapply(c(1:length(speed_parameter)), gamma_AIC_extract)
weibull_AICs <- sapply(c(1:length(speed_parameter)), weibull_AIC_extract)



## dataframe with speed parameter and estimated speeds
speeds_df <- data.frame(speed_parameter = exp(speed_parameter),
                        hmean = harmonics[1,],
                        lnorm = as.numeric(mods_predict_lnorm),
                        gamma = as.numeric(mods_predict_gamma),
                        weibull = as.numeric(mods_predict_weibull))


# separate df with model AICs
model_AICs <- data.frame(input = exp(speed_parameter),
                         lnorm = as.numeric(mods_predict_lnorm),
                         lnormAIC = lnorm_AICs,
                         gamma = as.numeric(mods_predict_gamma),
                         gammaAIC = gamma_AICs,
                         weibull = as.numeric(mods_predict_weibull),
                         weibullAIC = weibull_AICs)



#### PLOTS ####

# calculate errors between realised and estimated speeds:
hmean_error_real_calc <- function(speed_no){
  as.numeric(harmonics[1, speed_no]) - mean(seq_dats[,speed_no]$realised) #-- negative == means the estimated speed is smaller than the realised speed
}
hmean_error_real <- sapply(c(1:length(speed_parameter)), hmean_error_real_calc)

lnorm_error_real_calc <- function(speed_no){
  as.numeric(mods_predict_lnorm[speed_no]) - mean(seq_dats[,speed_no]$realised)
}
lnorm_error_real <- sapply(c(1:length(speed_parameter)), lnorm_error_real_calc)

gamma_error_real_calc <- function(speed_no){
  as.numeric(mods_predict_gamma[speed_no]) - mean(seq_dats[,speed_no]$realised)
}
gamma_error_real <- sapply(c(1:length(speed_parameter)), gamma_error_real_calc)

weibull_error_real_calc <- function(speed_no){
  as.numeric(mods_predict_weibull[speed_no]) - mean(seq_dats[,speed_no]$realised)
}
weibull_error_real <- sapply(c(1:length(speed_parameter)), weibull_error_real_calc)


# error between speed parameter and estimated speeds:
hmean_error_sp_calc <- function(speed_no){
  as.numeric(harmonics[1, speed_no]) - exp(speed_parameter[speed_no])
}
hmean_error_sp <- sapply(c(1:length(speed_parameter)), hmean_error_sp_calc)

lnorm_error_sp_calc <- function(speed_no){
  as.numeric(mods_predict_lnorm[speed_no]) - exp(speed_parameter[speed_no])
}
lnorm_error_sp <- sapply(c(1:length(speed_parameter)), lnorm_error_sp_calc)

gamma_error_sp_calc <- function(speed_no){
  as.numeric(mods_predict_gamma[speed_no]) - exp(speed_parameter[speed_no])
}
gamma_error_sp <- sapply(c(1:length(speed_parameter)), gamma_error_sp_calc)

weibull_error_sp_calc <- function(speed_no){
  as.numeric(mods_predict_weibull[speed_no]) - exp(speed_parameter[speed_no])
}
weibull_error_sp <- sapply(c(1:length(speed_parameter)), weibull_error_sp_calc)


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





## previous plotting for bias1:
# p <- list()
# for(i in 1:length(speed_parameter)) {
#   s <- seq_dats[,i]
#   df <- data.frame(observed = s$observed)
#   p[[i]] <- ggplot(df, aes(x = observed))+
#     geom_density(size = 2)+
#     geom_vline(xintercept = s$realised[1], col = "blue", size = 2)+
#     geom_vline(xintercept = exp(speed_parameter[i]), col = "red", size = 2)+
#     theme_minimal()+
#     xlab("observed speeds (m/s)")+
#     labs(title = paste("speed parameter = ", exp(speed_parameter[i])))
# }
# make and save a legend for this: - commented out now bc don't need to do it again - would be nice to figure out how to make a common legend in marrangeGrob at some point though
# legend_df <- data.frame(xvals = seq(0,10, length = 10),
#                         yvals = c(4,3,6,8,2,3,4, 7, 0, 2),
#                         cols = c(rep(c("realised speed", "speed parameter"), length(5))))
# legend_plot <- ggplot(legend_df, aes(x = xvals, y = yvals, colour = cols))+
#   geom_smooth()+
#   scale_colour_manual(values=c("blue", "red"))+
#   theme(legend.title = element_blank(),
#         legend.position = "bottom")
# legend_plot
#ggsave(filename = "plots/legend_plot.png", plot = legend_plot)





### previous plot for bias1: plot of trap rate against speed:

# define trap rate as no. of points captured by CT / total distance travelled

# and probably up the number of speeds to 20


# # trap rate for each speed:
# trap_rates <- c()
# for (i in 1:length(speed_parameter)){
#   s <- seq_dats[,i]
#   trap_rates <- c(trap_rates, s$trap_rate[1])
# }
# 
# trap_rates_df <- data.frame(trap_rate = trap_rates,
#                             speed_parameter = exp(speed_parameter))
# 
# trap_rates_plot <- ggplot(trap_rates_df, aes(x = speed_parameter, y = trap_rate))+
#   geom_point()+
#   theme_minimal()+
#   labs(x = "speed parameter (m/s)",
#        y = "trap rate (points/m)")+
#   theme(axis.title = element_text(size=18))
# 
# png(file="plots/bias1_2.png",
#     width=700, height=600)
# trap_rates_plot
# dev.off()
