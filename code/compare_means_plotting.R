## COMPARING DIFF MEANS & MEDIANS PLOTTING ##

setwd("~/Documents/Project/CamtrapSimulation/code")

rm(list = ls())
source("CamtrapSimulation.R", echo=TRUE)

require(ggplot2)
require(ggpubr)


# one plot per simulation speed parameter (just one run for each - so just iter1)

# initialise dataframes to store realised and observed speeds
real_og <- data.frame(matrix(ncol=12, nrow=5e5)) # my original way of working out realised speeds (select chunks of the path of length same as mean observed sequence length - select the same number as number of observed sequences in that run)
real_p_to_p <- data.frame(matrix(ncol=12, nrow=5e5)) # individual speeds between each step (much more than above)
obs_m <- data.frame(matrix(ncol=12, nrow=5e5)) # M's way of working out observed speeds (arithmetic mean of speeds within each sequence)
obs_p_to_p <- data.frame(matrix(ncol=12, nrow=5e5)) # observed speeds are just the speeds between consecutive steps irrespective of path (much more than above too)

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

speed_parameters <- c(0.02, 0.06, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)
speed_parameters <- round(speed_parameters, digits = 3) # to avoid floating point issues

for (i in 1:length(speed_parameters)){
  j <- speed_parameters[i]
  
  load(paste0("../results/seq_dats/sp", j, "iter1.RData"))
  load(paste0("../results/paths_copy_for_analysis/sp", j, "/iter1.RData"))
  
  ## realised speeds ###################################################################################################################################
  
  real_og[,i] <- c(seq_dats$realised, rep(NA, times = (5e5-length(seq_dats$realised)))) # my initial wrong way of working out realised speeds (using selected chunks of the path of length equal to average obs sequence length)
  real_p_to_p[,i] <- path$speed # point-to-point realised speeds
  
  wMRS <- c(wMRS, mean(seq_dats$realised)) # my original way of working out MRS 
  aMRS <- c(aMRS, mean(path$speed)) # arithmetic MRS
  gMRS <- c(gMRS, exp(mean(log(path$speed)))) # geometric MRS
  med_wMRS <- c(med_wMRS, median(seq_dats$realised))
  med_pMRS <- c(med_pMRS, median(path$speed))
  
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
  gamma_p <- c(gamma_p, predict.sbm(mods_p[[1]]$gamma)[1,1])
  
  weibull_m <- c(weibull_m, predict.sbm(mods_m[[1]]$weibull)[1,1])
  weibull_p <- c(weibull_p, predict.sbm(mods_p[[1]]$weibull)[1,1])
  
}







