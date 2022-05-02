## SIMULATION RESULTS ANALYSIS ##

setwd("~/Documents/Project/CamtrapSimulation/code/simulation")

source("sbd_functions.R", echo=TRUE)

require(ggplot2)
require(gridExtra)

# set which one to analyse:
setwd("seq_dats/0.1-0.19_5e+05_1_(0,40)") # only thing that needs changing each time

# load in data:
load("seq_dats.RData")


# work out things to use in plotting:
sim_length <- length(seq_dats)/5 # number of speeds inputted into the simulation
obs_real_error <- c() # mean error between observed and realised speeds for each simulated set of speeds
real_mean <- c() # mean realised speeds
singles <- c() # number of single frames for each simulation run
zeros <- c() # number of zero frames for each simulation run
singles_prop <- c() # number of single frames / total number of points recorded
zeros_prop <- c() # number of zero frames / total number of points recorded
obs_count <- c() # number of observed speeds captured
for (i in 1:sim_length){
  s <- seq_dats[,i]
  real_mean <- c(real_mean, mean(s$realised))
  singles <- c(singles, s$n_singles[1])
  zeros <- c(zeros, s$n_zeros[1])
  singles_prop <- c(singles_prop, s$n_singles[1]/s$n_points[1])
  zeros_prop <- c(zeros_prop, s$n_zeros[1]/s$n_points[1])
  obs_count <- c(obs_count, length(s$observed))
  o <- s$observed
  o <- o[!is.na(o)]
  r <- mean(seq_dats[,i]$realised)
  errors <- c()
  for (j in 1:length(o)){
    e <- o[j] - r
    errors <- c(errors, e)
    errors_mean <- mean(errors)
  }
  obs_real_error <- c(obs_real_error, errors_mean)
}


# plotting bias 1 ---------------------------------------------------------

## PLOT: bias1_1.png
# distribution of observed speeds vs realised speeds:
bias1_plots_list <- list()
for (i in 1:sim_length){
  s <- seq_dats[,i]
  s$observed <- na.omit(s$observed)
  if (length(s$observed) < 2){ # skip the ones where there aren't enough observed speeds
    next
  }
  df <- data.frame(speed = c(s$realised, s$observed),
                   obs_real = c(rep("realised", length(s$realised)), rep("observed", length(s$observed))))
  bias1_plots_list[[i]] <- ggplot(df, aes(x = speed, colour = obs_real))+
    geom_density(size = 1)+
    theme_minimal()+
    theme(legend.title = element_blank(),
          legend.position = "bottom")+
    scale_colour_manual(values = c("blue", "red"))+
    xlab("speed (m/s)")+
    labs(title = paste("mean realised speed = ", round_dp(real_mean[i], 3), "m/s, n = ", obs_count[i]))+ # change this to mean realised speed
    geom_vline(xintercept = density(s$realised)$x[which.max(density(s$realised)$y)], colour = "red")+
    geom_vline(xintercept = density(s$observed)$x[which.max(density(s$observed)$y)], colour = "blue")
}

bias1_plots_list2 <- bias1_plots_list[-which(sapply(bias1_plots_list, is.null))] # if there are null plots - use this instead
length(bias1_plots_list2)
# manually set row and column numbers based on length of plot list:
nrow_p <- 2
ncol_p <- 5

bias1_arranged <- marrangeGrob(grobs=bias1_plots_list, nrow=nrow_p, ncol=ncol_p)

ggsave(filename = "bias1_1.png", plot = bias1_arranged, height = 10, width = 30)


# PLOT: bias1_2.png
# mean error between observed & mean realised speed against mean realised speed:
bias1_2_df <- data.frame(error = obs_real_error,
                              mean_realised = real_mean)

bias1_2_plot <- ggplot(bias1_2_df, aes(x = mean_realised, y = error))+
  geom_point(size = 3, colour = "blue")+
  theme_minimal()+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_text(x = 0.14, y = 0.0005, label = "obs > real", size = 6)+
  geom_text(x = 0.14, y = -0.0005, label = "real > obs", size = 6)+
  labs(x = "mean realised speed (m/s)", y = "mean error between observed and realised speeds (m/s)")
bias1_2_plot

png(file="bias1_2.png",
    width=700, height=600)
bias1_2_plot
dev.off()



# plotting bias 2  --------------------------------------------

## PLOT: bias2_2.png
# number of single or zero frames divided by total no. of points for each mean realised speed
bias2_2_df <- data.frame(real_mean = c(rep(real_mean, length(2))),
                         count = c(singles_prop, zeros_prop),
                         type_of_count = c(rep("single frames", length(singles_prop)), rep("zero frames", length(zeros_prop))))

bias2_2_plot <- ggplot(bias2_2_df, aes(x = real_mean, y = count, colour = type_of_count))+
  geom_point(size = 2)+
  geom_line()+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.position = "none",
        strip.text.y = element_text(size = 15))+
  facet_grid(rows = vars(type_of_count))+
  labs(x = "mean realised speed (m/s)", y = "count / no. points captured")
bias2_2_plot

png(file= "bias2_2.png",
    width=700, height=650)
bias2_2_plot
dev.off()


# calculate estimated speeds - using hmean and SBMs #####

## HARMONIC MEAN:

harmonics <- sapply(c(1:sim_length), calc_hmean)


## SIZE-BIASED MODELS:

# 1. fit all the models

mods <- sapply(c(1:sim_length), mods_all_fit)

# what the outputs look like:
# mods[1,1] == mods[1] == models for input_speed[1]
# mods[2,1] == mods[2] == AICs for input_speed[1]
# mods[1,2] == mods[3] == models for input_speed[2]
# mods[2,2] == mods[4] == AICs for input_speed[2]

# both the models and AICs for each input_speed are in the columns:
# -- mods[,1] = models & AICs for input_speed[1]
# -- mods[,2] = models & AICs for input_speed[2]

# 2. predict average speed using the models

mods_predict_lnorm <- sapply(c(1:sim_length), predict_lnorm)
mods_predict_gamma <- sapply(c(1:sim_length), predict_gamma)
mods_predict_weibull <- sapply(c(1:sim_length), predict_weibull)


# 3. extract and store the AICs for each model

lnorm_AICs <- sapply(c(1:sim_length), lnorm_AIC_extract)
gamma_AICs <- sapply(c(1:sim_length), gamma_AIC_extract)
weibull_AICs <- sapply(c(1:sim_length), weibull_AIC_extract)



# ERRORS BETWEEN ESTIMATED AND REALISED SPEEDS --------

hmean_error_real <- sapply(c(1:sim_length), hmean_error_real_calc)
lnorm_error_real <- sapply(c(1:sim_length), lnorm_error_real_calc)
gamma_error_real <- sapply(c(1:sim_length), gamma_error_real_calc)
weibull_error_real <- sapply(c(1:sim_length), weibull_error_real_calc)

## PLOT: real_est_error_plot
# plot error between mean realised speed and estimated speeds against mean realised speed:
real_est_error_df <- data.frame(mean_real = rep(real_mean, length(4)),
                                error = c(hmean_error_real, lnorm_error_real, gamma_error_real, weibull_error_real),
                                method = c(rep("harmonic mean", length(hmean_error_real)), rep("SBM lnorm", length(lnorm_error_real)), rep("SBM gamma", length(gamma_error_real)), rep("SBM weibull", length(weibull_error_real))))

real_est_error_plot <- ggplot(real_est_error_df, aes(x = mean_real, y = error, colour = method))+
  geom_point(size = 2)+
  theme_minimal()+
  theme(axis.title = element_text(size=18),
        legend.text = element_text(size=17),
        legend.title = element_text(size=17),
        axis.text = element_text(size=15))+
  labs(x = "mean realised speed (m/s)",
       y = "error between mean realised speed and estimated speed (m/s)")
real_est_error_plot

png(file= "real_est_error.png",
    width=700, height=650)
real_est_error_plot
dev.off()


# bias 1 effects ----------------------------------------------------------

## PLOT: bias1_4.png
# plot to see how this bias affects our overall ability to estimate speeds: plot mean error between obs and mean realised against error between mean realised and estimated
# (i.e. how good are the SBMs at correcting for this bias?)
# different coloured line for each type of speed estimation:
est_real_error <- c()
for (i in 1:sim_length){
  o <- seq_dats[,i]$observed
  o <- o[!is.na(o)]
  r <- mean(seq_dats[,i]$realised)
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

png(file= "bias1_effects.png",
    width=700, height=650)
bias1_effects_plot
dev.off()


# bias 2 effects ----------------------------------------------------------


## PLOT: bias2_effects.png
# error between realised and estimated speed against no. of single- and zero-frame sequences - to see the effect of missed high speeds on our estimations of speed
bias2_effects_df <- data.frame(error = rep(c(hmean_error_real, lnorm_error_real, gamma_error_real, weibull_error_real), length(2)),
                               type_of_estimate = rep(c(rep("harmonic mean", length(hmean_error_real)), rep("SBM lnorm", length(lnorm_error_real)), rep("SBM gamma", length(gamma_error_real)), rep("SBM weibull", length(weibull_error_real))), length(2)),
                               singles_zeros = c(rep(singles, length(4)), rep(zeros, length(4))),
                               type_of_count = c(rep("singles", length(4*sim_length)), rep("zeros", length(4*sim_length))))

bias2_effects_plot <- ggplot(bias2_effects_df, aes(x = singles_zeros, y = error, colour = type_of_estimate))+
  geom_point(size = 2)+
  facet_grid(rows = vars(type_of_count))+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        legend.text = element_text(size = 17),
        legend.title = element_blank())+
  labs(x = "count",
       y = "mean error between estimated speeds and mean realised speeds (m/s)")
bias2_effects_plot

png(file= "bias2_effects.png",
    width=700, height=650)
bias2_effects_plot
dev.off()
