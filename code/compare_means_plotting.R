## COMPARING DIFF MEANS & MEDIANS PLOTTING ##

setwd("~/Documents/Project/CamtrapSimulation/code")

rm(list = ls())
source("CamtrapSimulation.R", echo=TRUE)

require(ggplot2)
require(ggpubr)


# one plot per simulation speed parameter (just one run for each - so just iter1)

# initialise dataframes to store realised and observed speeds
real_og <- data.frame(matrix(ncol=11, nrow=5e5)) # my original way of working out realised speeds (select chunks of the path of length same as mean observed sequence length - select the same number as number of observed sequences in that run)
real_p_to_p <- data.frame(matrix(ncol=11, nrow=5e5)) # individual speeds between each step (much more than above)
obs_m <- data.frame(matrix(ncol=11, nrow=5e5)) # M's way of working out observed speeds (arithmetic mean of speeds within each sequence)
obs_p_to_p <- data.frame(matrix(ncol=11, nrow=5e5)) # observed speeds are just the speeds between consecutive steps irrespective of path (much more than above too)

# initialise vectors to store means & medians of realised and observed speeds
wMRS <- c() # mean of my original way of working out realised speeds
aMRS <- c() # arithmetic mean of point-to-point realised speeds
gMRS <- c() # geometric mean of point-to-point realised speeds
med_wMRS <- c() # median realised speed using my original way of working out realised speeds
med_pMRS <- c() # median realised speed using point-to-point realised speeds

mMOS <- c() # arithmetic mean of M's way of working out observed speeds
gmMOS <- c() # geometric mean of M's way of working out observed speeds
aMOS <- c() # arithmetic mean of point-to-point observed speeds
gMOS <- c() # geometric mean of point-to-point observed speeds
med_mMOS <- c() # median of M's way of working out observed speeds
med_pMOS <- c() # median of point-to-point observed speeds

hmean_m <- c() # calculated using M's way of working out observed speeds
hmean_p <- c() # calculated using point-to-point observed speeds
lnorm_m <- c() # same as for hmean
lnorm_p <- c()
gamma_m <- c() # ditto
gamma_p <- c()
weibull_m <- c() # ditto
weibull_p <- c()

# (haven't included singles & zeros here bc would make it all too complicated - first make a decision on what's best)

# loop through each speed parameter and fill these dataframes and vectors

speed_parameters <- c(0.02, 0.06, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90) # sp1.00 is being problematic with sigma... - ask Francis/Marcus about this
speed_parameters <- round(speed_parameters, digits = 3) # to avoid floating point issues

for (i in 1:length(speed_parameters)){
  j <- speed_parameters[i]
  
  load(paste0("../results/seq_dats/sp", j, "iter1.RData"))
  load(paste0("../results/paths_copy_for_analysis/sp", j, "/iter1.RData"))
  
  ## realised speeds ###################################################################################################################################
  
  r_og <- seq_dats$realised # my initial wrong way of working out realised speeds (using selected chunks of the path of length equal to average obs sequence length)
  r_p_to_p <- path$speed # point-to-point realised speeds
  
  # cap both at 10m/s (36km/h) to get rid of unrealistically high ones
  r_og <- r_og[r_og<10]
  r_p_to_p <- r_p_to_p[r_p_to_p<10]
  
  # fill dataframes with those speeds
  real_og[,i] <- c(r_og, rep(NA, times = (5e5-length(r_og))))
  real_p_to_p[,i] <- c(r_p_to_p, rep(NA, times = (5e5-length(r_p_to_p))))
  
  wMRS <- c(wMRS, mean(r_og)) # my original way of working out MRS 
  aMRS <- c(aMRS, mean(r_p_to_p)) # arithmetic MRS
  gMRS <- c(gMRS, exp(mean(log(r_p_to_p)))) # geometric MRS
  med_wMRS <- c(med_wMRS, median(r_og))
  med_pMRS <- c(med_pMRS, median(r_p_to_p))
  
  ## mean observed speeds #################################################################################################################################
  
  m_obs <- seq_dats$observed # M's way of working out observed speeds
  m_obs <- m_obs[is.finite(m_obs)]
  
  p_obs <- seq_dats$posdat$distance # point-to-point observed speeds irrespective of sequence
  p_obs <- p_obs[is.finite(p_obs)]
  
  obs_m[,i] <- c(m_obs, rep(NA, times = (5e5-length(m_obs)))) # M's way of working out observed speeds
  obs_p_to_p[,i] <- c(p_obs, rep(NA, times = (5e5-length(p_obs)))) # point-to-point observed speeds
  
  mMOS <- c(mMOS, mean(m_obs)) 
  gmMOS <- c(gmMOS, exp(mean(log(m_obs))))
  aMOS <- c(aMOS, mean(p_obs))
  gMOS <- c(gMOS, exp(mean(log(p_obs))))
  
  med_mMOS <- c(med_mMOS, median(m_obs))
  med_pMOS <- c(med_pMOS, median(p_obs))
  
  # estimated speeds ##
  
  hmean_m <- c(hmean_m, (hmean_calc(m_obs))[1]) # harmonic mean estimate using M's observed speeds
  
  hmean_p <- c(hmean_p, (hmean_calc(p_obs))[1]) # using raw point-to-point speeds
  
  obs_df_m <- data.frame(speed = m_obs)
  obs_df_p <- data.frame(speed = p_obs)
  
  mods_m <- sbm3(speed~1, obs_df_m) # fit all the models
  mods_p <- sbm3(speed~1, obs_df_p)
  
  lnorm_m <- c(lnorm_m, predict.sbm(mods_m[[1]]$lnorm)[1,1])
  lnorm_p <- c(lnorm_p, predict.sbm(mods_p[[1]]$lnorm)[1,1])
  
  gamma_m <- c(gamma_m, predict.sbm(mods_m[[1]]$gamma)[1,1])
  gamma_p <- c(gamma_p, predict.sbm(mods_p[[1]]$gamma)[1,1]) # this is the problematic one for sp = 1.00 for some reason!! -- check this with Francis tomorrow
  
  weibull_m <- c(weibull_m, predict.sbm(mods_m[[1]]$weibull)[1,1])
  weibull_p <- c(weibull_p, predict.sbm(mods_p[[1]]$weibull)[1,1])
  
  rm(list = c("seq_dats", "metadata_sim", "path"))
  
}


## plot - which you'll then loop through each speed parameter

## realised speeds plot - with mean and median realised speeds and estimated speeds
# top one = my original way of working out realised speeds
# bottom one = point-to-point

# problems with super high realised speeds - see notebook for info but best solution rn is just to truncate at 10m/s

real_plots <- c()

for (i in 1:ncol(real_og)){
  r_og <- real_og[,i]
  r_p2p <- real_p_to_p[,i]
  if (i < 7){ # for the lower speeds - truncate at 3m/s for the purposes of visualisation
    r_og <- r_og[r_og<3]
    r_p2p <- r_p2p[r_p2p<3]
  }
  real_plot_df <- data.frame(real = c(real_og[,i], real_p_to_p[,i]),
                             type = c(rep("path sections", times = length(real_og[,i])), rep("all steps", times = length(real_p_to_p[,i]))))
  real_plot <- ggplot(real_plot_df, aes(x = real))+
    geom_density()+
    facet_grid(vars(type))+
    geom_vline(xintercept = aMRS[i], colour = "blue", linetype = "dashed")
  # facet_wrap(type ~.)
  real_plots <- c(real_plots, real_plot)
}

## to finish if decide it would be useful -- for now try to do the multi-speed plots though

# write this down in notebook then can close this script for now

  
  
  
  


