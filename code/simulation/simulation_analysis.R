## SIMULATION RESULTS ANALYSIS ##

# load in data:
load("~/Documents/Project/CamtrapSimulation/code/simulation/seq_dats/1.01-1.1_5000_0_(0,40)/seq_dats.RData")


# make new folder to save plots in ----------------------------------------

folder_name <- "seq_dats_plots/1.01-1.1_5000_0_(0,40)"

dir.create(folder_name)


# plotting bias 1 ---------------------------------------------------------

## PLOT: bias1_1.png
# distribution of observed speeds vs realised speeds:
bias1_plots_list <- list()
for (i in 1:length(speed_parameter)){
  s <- seq_dats[,i]
  s$observed <- na.omit(s$observed)
  df <- data.frame(speed = c(s$realised, s$observed),
                   obs_real = c(rep("realised", length(s$realised)), rep("observed", length(s$observed))))
  bias1_plots_list[[i]] <- ggplot(df, aes(x = speed, colour = obs_real))+
    geom_density(size = 1)+
    theme_minimal()+
    theme(legend.title = element_blank(),
          legend.position = "bottom")+
    scale_colour_manual(values = c("blue", "red"))+
    xlab("speed (m/s)")+
    labs(title = paste("speed parameter = ", exp(speed_parameter[i]), " m/s"))+
    geom_vline(xintercept = density(s$realised)$x[which.max(density(s$realised)$y)], colour = "red")+
    geom_vline(xintercept = density(s$observed)$x[which.max(density(s$observed)$y)], colour = "blue")
}

nrow_p <- length(speed_parameter)/5
ncol_p <- length(speed_parameter)/nrow_p 

bias1_arranged <- marrangeGrob(grobs=bias1_plots_list, nrow=nrow_p, ncol=ncol_p)

ggsave(filename = "~/Documents/Project/CamtrapSimulation/code/simulation/seq_dats/1.01-1.1_5000_0_(0,40)/bias1_1.png", plot = bias1_arranged, height = 10, width = 30)


# # PLOT: bias1_2.png --> commented out for now to keep things simpler
# # mean error between observed & mean realised speed against mean realised speed:
# 
# obs_real_error <- c()
# mean_realised_speeds <- c()
# for (i in 1:length(speed_parameter)){
#   o <- seq_dats[,i]$observed
#   o <- o[!is.na(o)]
#   r <- mean(seq_dats[,i]$realised)
#   mean_realised_speeds <- c(mean_realised_speeds, r)
#   errors <- c()
#   for (j in 1:length(s)){
#     e <- o[j] - r
#     errors <- c(errors, e)
#     errors_mean <- mean(errors)
#   }
#   obs_real_error <- c(obs_real_error, errors_mean)
# }
# 
# bias1_errors_df <- data.frame(error = obs_real_error,
#                               mean_realised = mean_realised_speeds)
# 
# bias1_errors_plot <- ggplot(bias1_errors_df, aes(x = mean_realised, y = error))+
#   geom_point(size = 2, colour = "blue")+
#   theme_minimal()+
#   theme(axis.title = element_text(size=18),
#         axis.text = element_text(size = 15))+
#   labs(x = "mean realised speed (m/s)", y = "mean error (m/s)")
# bias1_errors_plot
# 
# png(file=paste(folder_name, "/bias1_2.png", sep = ""),
#     width=700, height=600)
# bias1_errors_plot
# dev.off()


# plotting bias 2  --------------------------------------------

real_mean <- c()
singles <- c()
zeros <- c()
singles_prop <- c()
zeros_prop <- c()
for (i in 1:length(speed_parameter)){
  s <- seq_dats[,i]
  real_mean <- c(real_mean, mean(s$realised))
  singles <- c(singles, s$n_singles[1])
  zeros <- c(zeros, s$n_zeros[1])
  singles_prop <- c(singles_prop, s$n_singles[1]/s$n_points[1])
  zeros_prop <- c(zeros_prop, s$n_zeros[1]/s$n_points[1])
}


## PLOT: bias2_2.png --> instead of just raw count
# number of single or zero frames divided by total no. of points for each mean realised speed
bias2_2_df <- data.frame(real_mean = c(rep(real_mean, length(2))),
                         count = c(singles_prop, zeros_prop),
                         type_of_count = c(rep("single frames", length(singles_prop)), rep("zero frames", length(zeros_prop))))

bias2_2_plot <- ggplot(bias2_2_df, aes(x = real_mean, y = count, colour = type_of_count))+
  geom_point(size = 2)+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.position = "none",
        strip.text.y = element_text(size = 15))+
  facet_grid(rows = vars(type_of_count))+
  labs(x = "mean realised speed (m/s)", y = "count / no. points captured")
#bias2_2_plot

png(file= "~/Documents/Project/CamtrapSimulation/code/simulation/seq_dats/1.01-1.1_5000_0_(0,40)/bias2_2.png",
    width=700, height=650)
bias2_2_plot
dev.off()




# calculate estimated speeds - using hmean and SBMs #####

##--> to put back in: highlight entire thing and uncomment

# # apply each method (hmean, SBMlog, SBMgamma, SBMWeibull) to each speed parameter
# 
# ## HARMONIC MEAN:
# 
# ## calc_hmean
# # work out harmonic mean of a set of observed speeds (i.e. each simulation rep)
# # returns a harmonic mean and standard error for each set of speeds
# # INPUT:
# # number of reps of the simulation - so that can loop through every set of observed speeds
# harmonic <- c()
# calc_hmean <- function(speed_no){
#   # format speeds per input rep number
#   s1 <- seq_dats[,speed_no]
#   s2 <- s1$observed
#   s3 <- s2[!is.nan(s2)]
#   # work out hmean for the set of speeds for this input rep number
#   harmonic <- c(harmonic, hmean(s3))
#   return(harmonic)
# }
# 
# harmonics <- sapply(c(1:length(speed_parameter)), calc_hmean)
# 
# 
# 
# 
# ## SIZE-BIASED MODELS:
# 
# # 1. fit all the models
# 
# ## mods_all_fit
# # fits all 3 SBMs (lognormal, gamma, Weibull) to each set of observed speeds (i.e. each simulation rep)
# # return three models for each set of measured speeds
# # INPUT:
# # number of reps of the simulation
# mods_all_fit <- function(speed_no){
#   # format speeds per input rep number
#   s1 <- seq_dats[,speed_no]
#   s2 <- s1$observed
#   s3 <- s2[!is.nan(s2)]
#   # make df:
#   df <- data.frame(speed = s3)
#   # fit all three models:
#   sbm3(speed~1, df)
# }
# 
# mods <- sapply(c(1:length(speed_parameter)), mods_all_fit)
# 
# # what the outputs look like:
# # mods[1,1] == mods[1] == models for input_speed[1]
# # mods[2,1] == mods[2] == AICs for input_speed[1]
# # mods[1,2] == mods[3] == models for input_speed[2]
# # mods[2,2] == mods[4] == AICs for input_speed[2]
# 
# # both the models and AICs for each input_speed are in the columns:
# # -- mods[,1] = models & AICs for input_speed[1]
# # -- mods[,2] = models & AICs for input_speed[2]
# 
# 
# # 2. plot the models
# 
# # -- not needed currently
# # -- see extra code at bottom for this
# 
# # 3. predict average speed using the models
# 
# ## predict_lnorm
# # predict average speed using a fitted lognormal model for each set of observed speeds (i.e. each simulation rep)
# # INPUT:
# # number of reps of the simulation
# predict_lnorm <- function(speed_no){
#   predict.sbm(mods[[1,speed_no]]$lnorm)[1] # selects just the estimate of speed
#   # Q: default is newdata = NULL - what does this actually mean?
# }
# 
# ## predict_gamma
# # predict average speed using a fitted gamma model for each set of observed speeds (i.e. each simulation rep)
# # INPUT:
# # number of reps of the simulation
# predict_gamma <- function(speed_no){
#   predict.sbm(mods[[1,speed_no]]$gamma)[1]
# }
# 
# ## predict_weibull
# # predict average speed using a fitted Weibull model for each set of observed speeds (i.e. each simulation rep)
# # INPUT:
# # number of reps of the simulation
# predict_weibull <- function(speed_no){
#   predict.sbm(mods[[1,speed_no]]$weibull)[1]
# }
# 
# mods_predict_lnorm <- sapply(c(1:length(speed_parameter)), predict_lnorm)
# mods_predict_gamma <- sapply(c(1:length(speed_parameter)), predict_gamma)
# mods_predict_weibull <- sapply(c(1:length(speed_parameter)), predict_weibull)
# 
# 
# # 4. extract and store the AICs for each model - not needed for now
# 
# # mods is in such a bizarre format that the best way I could come up with was this function: (couldn't get the AIC.sbm function working)
# 
# ## lnorm_AIC_extract
# # return the AIC for the fitted lognormal model for each set of observed speeds
# # INPUT:
# # number of reps of the simulation
# # lnorm_AIC_extract <- function(speed_no){
# #   a1 <- mods[2,speed_no]
# #   a2 <- a1$AICtab[1]
# #   a2["lnorm",]
# # }
# # 
# # ## gamma_AIC_extract
# # # return the AIC for the fitted gamma model for each set of observed speeds
# # # INPUT:
# # # number of reps of the simulation
# # gamma_AIC_extract <- function(speed_no){
# #   a1 <- mods[2,speed_no]
# #   a2 <- a1$AICtab[1]
# #   a2["gamma",]
# # }
# # 
# # ## weibull_AIC_extract
# # # return the AIC for the fitted Weibull model for each set of observed speeds
# # # INPUT:
# # # number of reps of the simulation
# # weibull_AIC_extract <- function(speed_no){
# #   a1 <- mods[2,speed_no]
# #   a2 <- a1$AICtab[1]
# #   a2["weibull",]
# # }
# # 
# # lnorm_AICs <- sapply(c(1:length(speed_parameter)), lnorm_AIC_extract)
# # gamma_AICs <- sapply(c(1:length(speed_parameter)), gamma_AIC_extract)
# # weibull_AICs <- sapply(c(1:length(speed_parameter)), weibull_AIC_extract)
# 
# 
# 
# ## dataframe with speed parameter and estimated speeds
# # speeds_df <- data.frame(speed_parameter = exp(speed_parameter),
# #                         hmean = harmonics[1,],
# #                         lnorm = as.numeric(mods_predict_lnorm),
# #                         gamma = as.numeric(mods_predict_gamma),
# #                         weibull = as.numeric(mods_predict_weibull))
# # 
# # 
# # # separate df with model AICs
# # model_AICs <- data.frame(input = exp(speed_parameter),
# #                          lnorm = as.numeric(mods_predict_lnorm),
# #                          lnormAIC = lnorm_AICs,
# #                          gamma = as.numeric(mods_predict_gamma),
# #                          gammaAIC = gamma_AICs,
# #                          weibull = as.numeric(mods_predict_weibull),
# #                          weibullAIC = weibull_AICs)



# ERRORS BETWEEN ESTIMATED AND REALISED SPEEDS --------

##--> same here: highlight everything and uncomment to put back in

# # calculate errors between realised and estimated speeds:
# hmean_error_real_calc <- function(speed_no){
#   as.numeric(harmonics[1, speed_no]) - mean(seq_dats[,speed_no]$realised) #-- negative == means the estimated speed is smaller than the realised speed
# }
# hmean_error_real <- sapply(c(1:length(speed_parameter)), hmean_error_real_calc)
# 
# lnorm_error_real_calc <- function(speed_no){
#   as.numeric(mods_predict_lnorm[speed_no]) - mean(seq_dats[,speed_no]$realised)
# }
# lnorm_error_real <- sapply(c(1:length(speed_parameter)), lnorm_error_real_calc)
# 
# gamma_error_real_calc <- function(speed_no){
#   as.numeric(mods_predict_gamma[speed_no]) - mean(seq_dats[,speed_no]$realised)
# }
# gamma_error_real <- sapply(c(1:length(speed_parameter)), gamma_error_real_calc)
# 
# weibull_error_real_calc <- function(speed_no){
#   as.numeric(mods_predict_weibull[speed_no]) - mean(seq_dats[,speed_no]$realised)
# }
# weibull_error_real <- sapply(c(1:length(speed_parameter)), weibull_error_real_calc)











# bias 1 effects ----------------------------------------------------------

# ## PLOT: bias1_4.png - commented out for now until fix issue with estimated speeds and get to that point
# # plot to see how this bias affects our overall ability to estimate speeds: plot mean error between obs and mean realised against error between mean realised and estimated 
# # (i.e. how good are the SBMs at correcting for this bias?)
# 
# # different coloured line for each type of speed estimation:
# 
# est_real_error <- c()
# mean_realised_speeds <- c()
# for (i in 1:length(speed_parameter)){
#   o <- seq_dats[,i]$observed
#   o <- o[!is.na(o)]
#   r <- mean(seq_dats[,i]$realised)
#   mean_realised_speeds <- c(mean_realised_speeds, r)
#   errors <- c()
#   for (j in 1:length(s)){
#     e <- o[j] - r
#     errors <- c(errors, e)
#     errors_mean <- mean(errors)
#   }
#   obs_real_error <- c(obs_real_error, errors_mean)
# }
# 
# 
# # bias1_effects_df <- data.frame(error_obs_real = c(rep(obs_real_error, length(4))),
# #                                error_est_real = c(hmean_error_real, lnorm_error_real, gamma_error_real, weibull_error_real),
# #                                method = c(rep("harmonic mean", length(hmean_error_real)), rep("SBM lnorm", length(lnorm_error_real)), rep("SBM gamma", length(gamma_error_real)), rep("SBM weibull", length(weibull_error_real))))
# 
# # version without gamma for now: until figure out what's going wrong with it:
# bias1_effects_df <- data.frame(error_obs_real = c(rep(obs_real_error, length(3))),
#                                error_est_real = c(hmean_error_real, lnorm_error_real, weibull_error_real),
#                                method = c(rep("harmonic mean", length(hmean_error_real)), rep("SBM lnorm", length(lnorm_error_real)), rep("SBM weibull", length(weibull_error_real))))
# 
# 
# bias1_effects_plot <- ggplot(bias1_effects_df, aes(x = error_obs_real, y = error_est_real, colour = method))+
#   geom_point(size = 2)+
#   theme_minimal()+
#   theme(axis.title = element_text(size=18),
#         legend.text = element_text(size = 17),
#         legend.title = element_text(size = 17),
#         axis.text = element_text(size = 15))+
#   labs(x = "mean error between observed speeds and mean realised speeds (m/s)",
#        y = "mean error between estimated speeds and mean realised speeds (m/s)")
# bias1_effects_plot
# 
# png(file= paste(folder_name, "/bias1_4.png", sep = ""),
#     width=700, height=650)
# bias1_effects_plot
# dev.off()


# bias 2 effects ----------------------------------------------------------


## PLOT: bias2_3.png
# number of single- and zero-frame sequences against error between realised and estimated speed - to see the effect of this bias on our estimations of speed



