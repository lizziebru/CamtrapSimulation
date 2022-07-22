#### SIMULATING DIFFERENT ANIMALS' MOVEMENT PATHS 

require(dplyr)
require(ggplot2)
require(ggpubr)
require(fitdistrplus)
require(stringr)

source("~/Documents/Project/CamtrapSimulation/code/CamtrapSimulation.R", echo=TRUE)


regentspark_mov_data <- read.csv("../data/regentspark_mov_data.csv")
india_mov_data <- read.csv("../data/india_mov_data.csv")
panama_data <- read.csv("../data/panama_data.csv")

data_all_cats <- read.csv("../data/data_all_cats.csv")



# parameters for bimodal speed modelling: using Pablo's data --------------

## add columns for species and body mass and format columns correctly

# read in Pablo's data from innovations paper
# pablo_spds_raw <- read.csv2("../data/Pablo_spds_raw.txt")


## from info about Pablo's dataset:
# GE (genet), BA (badger), FD (fallow deer), IG (Iberian goat), MA (stone/pine marten), 
# MO (mouflon), RF (red fox), RO (roe deer), RD (red deer), RS (red squirrel) and WB (wild boar).

## from Pantheria: (in kg)
# genet: 1.75617
# badger: 11.88403
# fallow deer: 57.22461
# iberian goat: 60.89877
# stone/pine marten: 1.675 (stone) & 1.29999 (pine) --> mean = 1.487495
# mouflon: 39.09789
# red fox: 4.82036
# roe deer: 22.50201
# red deer: 240.86713
# red squirrel: 0.333
# wild boar: 84.47154


# RF_group <- c("RF1", "RF2", "RF3", "RF4", "RF5")
# RD_group <- c("RD1", "RD2", "RD3", "RD4", "RD6", "RD7")
# RO_group <- c("RO1", "RO2", "RO3", "RO4", "RO5")
# WB_group <- c("WB1", "WB2", "WB3", "WB4", "WB5", "WB6", "WB7")
# BA_group <- c("BA")
# FD_group <- c("FD1", "FD2")
# GE_group <- c("GE")
# IG_group <- c("IG")
# MA_group <- c("MA")
# MO_group <- c("MO1", "MO2")
# RS_group <- c("RS")
# 
# # also make the columns the right types
# pablo_spds_raw$Code <- as.character(pablo_spds_raw$Code)
# pablo_spds_raw$Speed.m.s <- as.numeric(as.character(pablo_spds_raw$Speed.m.s))
# 
# species_newcol <- c()
# Mb_newcol <- c()
# 
# for (i in seq_len(nrow(pablo_spds_raw))){
#   p <- pablo_spds_raw[i,]
#   if (p$Code %in% RF_group){
#    species_newcol <- c(species_newcol, "red_fox")
#    Mb_newcol <- c(Mb_newcol, 4.82036)
#   }
#   if (p$Code %in% RD_group){
#     species_newcol <- c(species_newcol, "red_deer")
#     Mb_newcol <- c(Mb_newcol, 240.86713)
#   }
#   if (p$Code %in% RO_group){
#     species_newcol <- c(species_newcol, "roe_deer")
#     Mb_newcol <- c(Mb_newcol, 22.50201)
#   }
#   if (p$Code %in% WB_group){
#     species_newcol <- c(species_newcol, "wild_boar")
#     Mb_newcol <- c(Mb_newcol, 84.47154)
#   }
#   if (p$Code %in% BA_group){
#     species_newcol <- c(species_newcol, "badger")
#     Mb_newcol <- c(Mb_newcol, 11.88403)
#   }
#   if (p$Code %in% FD_group){
#     species_newcol <- c(species_newcol, "fallow_deer")
#     Mb_newcol <- c(Mb_newcol, 57.22461)
#   }
#   if (p$Code %in% GE_group){
#     species_newcol <- c(species_newcol, "genet")
#     Mb_newcol <- c(Mb_newcol, 1.75617)
#   }
#   if (p$Code %in% IG_group){
#     species_newcol <- c(species_newcol, "iberian_goat")
#     Mb_newcol <- c(Mb_newcol, 60.89877)
#   }
#   if (p$Code %in% MA_group){
#     species_newcol <- c(species_newcol, "stone/pine_marten")
#     Mb_newcol <- c(Mb_newcol, 1.487495)
#   }
#   if (p$Code %in% MO_group){
#     species_newcol <- c(species_newcol, "mouflon")
#     Mb_newcol <- c(Mb_newcol, 39.09789)
#   }
#   if (p$Code %in% RS_group){
#     species_newcol <- c(species_newcol, "red_squirrel")
#     Mb_newcol <- c(Mb_newcol, 0.333)
#   }
# }
# 
# pablo_spds_raw["species"] <- as.character(species_newcol)
# pablo_spds_raw["body_mass"] <- as.character(Mb_newcol)


# save as a new df and read in from here from now on:

# write.csv(pablo_spds_raw, file = "../data/pablo_spds_raw.csv")



## FROM NOW ON: GO FROM HERE:

pablo_spds_raw <- read.csv("../data/pablo_spds_raw.csv")

# 11 species altogether


## get parameters for two separate lognormal speed distributions: foraging & moving

# standard deviation: have a look at whether sd varies between movement behaviours 

# mean: estimate by fitting lognormal distributions to his data since it's biased CT data (like you did to get mean speed for unimodal lognormal distribution using Panama & RP data)

# vmax: use vmax - body mass relationship from Garland 1983 - cause that's the speed limit for that species

# means:
pablo_lnorm_est_mov <- c()
pablo_lnorm_est_feed <- c()

# sd:
pablo_lnorm_sd_mov <- c()
pablo_lnorm_sd_feed <- c()
pablo_lnorm_sd_log_mov <- c() # for speeds on a log scale
pablo_lnorm_sd_log_feed <- c() # ditto

# subset by feeding behaviour
pablo_mov <- pablo_spds_raw[pablo_spds_raw$Behaviour=="Moving",]
pablo_feed <- pablo_spds_raw[pablo_spds_raw$Behaviour=="Feeding",]

for (i in unique(pablo_spds_raw$species)){ # loop through each species
  
  ## moving speed estimate:
  v_mov <- na.omit(pablo_mov[pablo_mov$species==i,]$Speed.m.s)
  if (length(v_mov) <= 1 || i == "iberian_goat"){ # if there are no moving speeds for that animal or just one, can't fit the model so assign the estimate as NA -- and also if it's an iberian goat - optimisation algorithm can't do it so just skip it
    pablo_lnorm_est_mov <- c(pablo_lnorm_est_mov, NA)
    pablo_lnorm_sd_mov <- c(pablo_lnorm_sd_mov, NA)
    pablo_lnorm_sd_log_mov <- c(pablo_lnorm_sd_log_mov, NA)
  }
  else{ # otherwise, fit the models
    v_df_mov <- data.frame(speed = v_mov)
    mods_mov <- sbm3(speed~1, v_df_mov) # fit all the models
    pablo_lnorm_est_mov <- c(pablo_lnorm_est_mov, predict.sbm(mods_mov[[1]]$lnorm)[1,1])
    pablo_lnorm_sd_mov <- c(pablo_lnorm_sd_mov, predict.sbm(mods_mov[[1]]$lnorm)[1,2])
    pablo_lnorm_sd_log_mov <- c(pablo_lnorm_sd_log_mov, mean(sd(log(v_mov))))
  }
  
  ## feeding speed estimate:
  v_feed <- na.omit(pablo_feed[pablo_feed$species==i,]$Speed.m.s)
  if (length(v_feed) <= 1 || i == "fallow_deer"){ # if there are no feeding speeds for that animal or just one, can't fit the model so assign the estimate as NA - also if it's a fallow deer - optim also doesn't work with this so just skip this
    pablo_lnorm_est_feed <- c(pablo_lnorm_est_feed, NA)
    pablo_lnorm_sd_feed <- c(pablo_lnorm_sd_feed, NA)
    pablo_lnorm_sd_log_feed <- c(pablo_lnorm_sd_log_feed, NA)
  }
  else{ # otherwise, fit the models
    v_df_feed <- data.frame(speed = v_feed)
    mods_feed <- sbm3(speed~1, v_df_feed) # fit all the models
    pablo_lnorm_est_feed <- c(pablo_lnorm_est_feed, predict.sbm(mods_feed[[1]]$lnorm)[1,1])
    pablo_lnorm_sd_feed <- c(pablo_lnorm_sd_feed, predict.sbm(mods_feed[[1]]$lnorm)[1,2])
    pablo_lnorm_sd_log_feed <- c(pablo_lnorm_sd_log_feed, mean(sd(log(v_feed))))
  }
}


# make new dataframe:
pablo_main <- data.frame(species = rep(unique(pablo_spds_raw$species), times = 2),
                         body_mass = rep(unique(pablo_spds_raw$body_mass), times = 2),
                         behav = c(rep("moving", times = length(pablo_lnorm_est_mov)), rep("feeding", times = length(pablo_lnorm_est_feed))),
                         v_mean = c(pablo_lnorm_est_mov, pablo_lnorm_est_feed),
                         v_sd_log = c(pablo_lnorm_sd_log_mov, pablo_lnorm_sd_log_feed))


# check for whether speed sd varies with body mass:
ggplot(pablo_main, aes(x = body_mass, y = v_sd_log, colour = behav))+
  geom_point()

# no clear trend with body mass - esp if only look up until 50kg

# but sd is higher for moving than feeding behaviours - so use a fixed sd for each behaviour
pablo_sd_log_mov <- mean(na.omit(pablo_main[pablo_main$behav=="moving",]$v_sd_log)) 
pablo_sd_log_feed <- mean(na.omit(pablo_main[pablo_main$behav=="feeding",]$v_sd_log)) 


# get relationship between body mass (kg) and mean_v (m/s) for each behaviour:

# need this on a log scale:
v_bodymass_relat_pablo <- ggplot(pablo_main, aes(x = log10(body_mass), y = log10(v_mean), colour = behav))+
  geom_point()
v_bodymass_relat_pablo

# (this plot produces the same plot -- just for my sake of understanding this):
v_bodymass_relat2_pablo <- ggplot(pablo_main, aes(x = body_mass, y = v_mean, colour = behav))+
  geom_point()+
  scale_y_log10()+
  scale_x_log10()
v_bodymass_relat2_pablo

# fit regression model:
v_bodymass_lm_fit_pablo_mov <- lm(log(v_mean) ~ log(body_mass), data = pablo_main[pablo_main$behav=="moving",])
v_bodymass_lm_fit10_pablo_mov <- lm(log10(v_mean) ~ log10(body_mass), data = pablo_main[pablo_main$behav=="moving",])
summary(v_bodymass_lm_fit_pablo_mov)

cfs1 <- coef(v_bodymass_lm_fit_pablo_mov)
cfs2 <- coef(v_bodymass_lm_fit10_pablo_mov)

exp(cfs1[1])
10^cfs2[1]
# --> both of these give you the same bc the back-transform corresponds to which way you logged it originally


v_bodymass_lm_fit_pablo_feed <- lm(log(v_mean) ~ log(body_mass), data = pablo_main[pablo_main$behav=="feeding",])
summary(v_bodymass_lm_fit_pablo_feed)

cfs_feed <- coef(v_bodymass_lm_fit_pablo_feed)

exp(cfs_feed[1])

# coefficients:
# moving: 
# intercept = 0.396163, slope = 0.04714894
# feeding: 
# intercept = 0.03414077, slope = 0.1410986

# power law eqns to use in pathgen:
# c = 10^intercept
# b = slope

# moving:
# v_av = 2.489792*(Mb^0.04714894)
# feeding:
# v_av = 1.081785*(Mb^0.1410986)

##----------------------------------------------------------------##

## time spent feeding vs moving

# time spent feeding: draw from a normal distribution with mean dependent on body mass

# use scaling relationship between handling time and body mass from Pawar et al. 2012

## mean:
# th = 8912.509*(Mb^-1.02) -- from supp fig 2e Pawar et al. 2012
# th measured in s/kg though - where the kg is for resource mass - don't have this though...
# to get into hours: th = (8912.509*(Mb^-1.02))/3600

## sd:
# exponent is -1.02 plus or minus 0.08, so could just use 0.08 as the standard deviation?


# test it out:
# Mb = 10 kg
# th*Mb = 8511.38 s
# sd = 0.08

plot(density(rnorm(1000, mean = 8511.38, sd = 0.08)))
# sd feels a bit small though - maybe should multiply it by Mb^2? Cause its units are technically s/kg^2..?

# if use sd = 8 (i.e. multiplied by Mb^2), increases the range a bit but still not by much




## things to figure out:

# 1. can I equate th in s/kg to just seconds if assume 1kg of resource mass -- i.e. just define my feeding time as handling time for 1kg resource mass? - then could just have that in the discussion as a limitation of the model is that it I haven't modelled how resource mass varies with consumer mass?
# 2. standard deviation: seems too small to use 0.08...?
# 3. what's the probability of switching from moving into feeding? 10%?

# 1:
# using 1kg of resource mass means that time decreases way too much for larger body masses:
# 1kg: 2.48 hours
# 10kg: 14.2 min
# 20kg: 7 min
# 30kg: 4.63 min
# 40kg: 3.45 min
# 50kg: 2.75 min

# would be good to find a scaling trend of resource mass with consumer mass...

# there is one in Pawar et al. 2012 too! - supp fig 2g
# Mr = 10^(-2.76)*Mc^(0.73)
# where Mr = resource mass, Mc = consumer mass

# so th in seconds would be the th equation above multiplied by the resource mass for that consumer mass:
# th = 10^(3.95)*Mb^(-1.02) * 10^(-2.76)*Mb^(0.73)
# th = 10^(1.19)*Mb^(-0.29)
# --> is it ok to do this though?

# does this generate more legit times?
# 1kg: 15.48817 s
# 10kg: 7.943282 s
# 20kg: 6.496827 s
# 30kg: 5.776102 s
# 40kg: 5.313768 s
# 50kg: 4.980795 s
# --> seem way to small now...
# bc resource masses are pretty small for consumer masses up to 50kg
# but why is the time decreasing? doesn't look right

# maybe it's a units issue?
# no, everything's in kg

# need to discuss q1 with Samraat
# and q2 (sd issue) too

# and also q3: discuss with Samraat or Chris: any work which could inform probability of switching from moving to feeding?
# (or proportion of time spent moving vs feeding?)



##-----------------------------------------------##

# turnangle for feeding vs moving:

# in my unimodal simulation:
# pTurn (prob of turning at each step) = 0.5
# kTurn: mean vonMises concentration parameter (kappa) for turn angle (higher = more concentrated) = 2

# what I probably need to change for moving vs feeding: pTurn
# could just keep kTurn the same to keep things more constant? - discuss with M though

# Pablo doesn't use pTurn - he just varies kTurn

# pTurn in pathgen: used here:
tTurn <- rbinom(n_capped,1,pTurn) # generates set of n (= no of steps) numbers either 1 and 0 where higher probability of turning at each step = more likely to have 1
# try out various values of pTurn:
# could say that feeding behaviour = more likely to turn = pTurn==0.75??
# and moving behaviour = pTurn==0.25??

# does kappa need changing?


## ------------------------------------------------##

# test all this out


## probability of switching between the two behaviours:

## while moving: fixed probability of switching to feeding behaviour of 10%
# simplifies things - means don't go into modelling the environment and needs -- just average out to roughly 10%
# but have a look at the kind of trajectories this generates and if they look right
# OR: could model search rate with body size also using Pawar et al. 2012
# BUT: movement isn't JUST about searching for food: also for mates, territory, fleeing disturbance/danger etc
# so summarise this as 10% for simplicity for now


## while feeding: draw length of feeding time from a distribution with parameters defined by body mass:
# normal distribution with mean & sd

# relationship between mean feeding time and body mass:
# from Pawar et al. 2012 (Nature):
# consumption rates scale sublinearly with consumer body mass (exponent of approximately 0.85) for 2D interactions (terrestrial)
# I want the scaling relationship for th (handling time)


# relationship between sd of feeding time and body mass:
# is there one?
# maybe use a fixed sd?



## tortuosity parameters for the two behaviours:




## summary of all parameters:

# moving:
# v_av = 2.489792*(Mb^0.04714894)
# v_sd = 0.0465557
# vmax = (8.356367*(Mb^0.25892))/(M^(0.06237*log10(Mb))) (from Garland 1983)

# feeding:
# v_av = 1.081785*(Mb^0.1410986)
# v_sd = 0.01015985
# vmax = (8.356367*(Mb^0.25892))/(M^(0.06237*log10(Mb))) (from Garland 1983)



# parameters for unimodal speed modelling: linking body mass to mean speed, sd, & max speed  ---------------------

### logspeedSD - use the distribution of speed SDs in the data - but exclude bears & takins bc anomalous
rp_panama_only <- read.csv("../results/rp_panama_only.csv") # everything apart from bears & takins
plot(density(rp_panama_only$sd_log_v))
plot(density(rp_panama_only$sd_v))

sd_log_density <- ggplot(rp_panama_only, aes(x = sd_log_v))+
  geom_density()+
  geom_vline(xintercept = mean(rp_panama_only$sd_log_v), colour = "red")
sd_log_density

# use the mean of this as the fixed speed SD - bc there's no literature on the variation of speedSD with body size
# and empirical data suggest it remains relatively constant across body masses too
fixed_logspeedSD <- mean(rp_panama_only$sd_log_v) ## == 0.8546151


## maybe there is literature on this though - how speed SD scales with body mass
# or how it scales with mean speed?
ggplot(rp_panama_only, aes(x = mean_v, y= sd_v))+
  geom_point()






### max speed (=truncation parameter) 

# use the relationship between body mass & max speed from data

# Garland 1983: relationship between body mass (kg) & max running speed (km/h):

# use the polynomial relationship so that can relate body mass to it well

# polynomial relationship between body mass and max speed:

# vmax = max speed in km/h - just need to figure out units though
# Mb = body mass in kg

# log10(vmax) = 1.47832 + 0.25892*log10(Mb) - 0.06237*(log10(Mb))^2 -- original eqn from paper

# get it in non-log form then convert units (see notebook for full maths)
# vmax = (30.08292*(Mb^0.25892))/(M^(0.06237*log10(Mb)))
# in m/s: (!!) (need to multiply by 5/18)
# vmax = (8.356367*(Mb^0.25892))/(M^(0.06237*log10(Mb))) -- eqn to use in pathgen 




# Rowcliffe et al. 2016: mean travel speed related to body mass - using Panama data

# mean scaling exponent (faunivores & herbivores) = 0.17
# need the constant though
# --> need to reproduce their work to work out constant so that can calculate mean speed from body mass

# do the same as Rowcliffe et al. but also use regent's park data

# use lnorm

rp_panama_only <- read.csv("../results/rp_panama_only.csv") # everything apart from bears & takins

# exclude mouse bc only one datapoint
rp_spp <- str_remove(rp_panama_only$species, "mouse") 
rp_panama_only2 <- rp_panama_only[rp_panama_only$species %in% rp_spp,]

lnorm_est <- c()

for (i in rp_panama_only2$species){
  v <- data_all_cats_rp[data_all_cats_rp$species==i,]$speed
  v_df <- data.frame(speed = v)
  mods <- sbm3(speed~1, v_df) # fit all the models
  lnorm_est <- c(lnorm_est, predict.sbm(mods[[1]]$lnorm)[1,1])
}

# make dataframe:
rp_panama_only2["lnorm_est"] <- lnorm_est

# write.csv(rp_panama_only2, file = "../results/rp_panama_only2.csv")
# --> if need to go back to this can just read in the saved csv now
rp_panama_only2 <- read.csv("../results/rp_panama_only2.csv")

# now need the relationship between travel speed (m/s) & body mass (kg)
rp_panama_only2["body_mass_kg"] <- (rp_panama_only2$body_mass)/1000

# need this on a log scale:
v_bodymass_relat <- ggplot(rp_panama_only2, aes(x = log10(body_mass_kg), y = log10(lnorm_est)))+
  geom_point()
v_bodymass_relat

v_bodymass_relat2 <- ggplot(rp_panama_only2, aes(x = body_mass_kg, y = lnorm_est))+
  geom_point()+
  scale_y_log10()+
  scale_x_log10()
v_bodymass_relat2

# fit regression model:
v_bodymass_lm_fit <- lm(log(lnorm_est) ~ log(body_mass_kg), data = rp_panama_only2)
v_bodymass_lm_fit10 <- lm(log10(lnorm_est) ~ log10(body_mass_kg), data = rp_panama_only2)
summary(v_bodymass_lm_fit)

cfs1 <- coef(v_bodymass_lm_fit)
cfs2 <- coef(v_bodymass_lm_fit10)
exp(cfs[1])*1^cfs[2]

exp(cfs1[1])
10^cfs2[1]
# --> both of these give you the same bc the back-transform corresponds to which way you logged it originally


# plot this regression on the log scale plot to check it's ok:
rp_panama_only2["lm_predicted"] <- predict(v_bodymass_lm_fit)

v_bodymass_relat3 <- ggplot(rp_panama_only2, aes(x = body_mass_kg, y = lnorm_est))+
  geom_point()
#  scale_y_log10()+
#  scale_x_log10()+
  geom_line(aes(y=lm_predicted), colour="red")+
  geom_smooth(method="lm", colour = "blue", se=F)+
  stat_regline_equation()
v_bodymass_relat3


plot(lnorm_est ~ body_mass_kg, data=rp_panama_only2, log=c('x', 'y'))
abline(v_bodymass_lm_fit, col='red')

# intercept: -0.8673281 
# slope: 0.197178

# c = 10^intercept = 0.1357288
# b = slope = 0.197178

# therefore, power law eqn to use in pathgen:
# v_av = 0.1357288*(Mb^0.197178)




# investigating body mass - speed - SD relationship more closely -----------

# add column with body mass to data_all_cats dataframe

data_all_cats["Body_mass_g"] <- rep(NA, times = nrow(data_all_cats))

for (i in 1:nrow(data_all_cats)){
  if (data_all_cats[i,]$species=="Fox"){
    data_all_cats[i,]$Body_mass_g <- 4820.36
  }
  if (data_all_cats[i,]$species=="Hedgehog"){
    data_all_cats[i,]$Body_mass_g <- 777.95
  }
  if (data_all_cats[i,]$species=="takin"){
    data_all_cats[i,]$Body_mass_g <- 294515.33
  }
  if (data_all_cats[i,]$species=="takin young"){
    data_all_cats[i,]$Body_mass_g <- 100000
  }
  if (data_all_cats[i,]$species=="himalayan black bear"){
    data_all_cats[i,]$Body_mass_g <- 99714.19
  }
  if (data_all_cats[i,]$species=="agouti"){
    data_all_cats[i,]$Body_mass_g <- 2309.12
  }
  if (data_all_cats[i,]$species=="coati"){
    data_all_cats[i,]$Body_mass_g <- 3775.5
  }
  if (data_all_cats[i,]$species=="ocelot"){
    data_all_cats[i,]$Body_mass_g <- 11880
  }
  if (data_all_cats[i,]$species=="squirrel"){
    data_all_cats[i,]$Body_mass_g <- 545.4
  }
  if (data_all_cats[i,]$species=="paca"){
    data_all_cats[i,]$Body_mass_g <- 8172.55
  }
  if (data_all_cats[i,]$species=="rat"){
    data_all_cats[i,]$Body_mass_g <- 282.17
  }
  if (data_all_cats[i,]$species=="tamandua"){
    data_all_cats[i,]$Body_mass_g <- 4800
  }
  if (data_all_cats[i,]$species=="peccary"){
    data_all_cats[i,]$Body_mass_g <- 21133.69
  }
  if (data_all_cats[i,]$species=="brocket"){
    data_all_cats[i,]$Body_mass_g <- 20546.86
  }
  if (data_all_cats[i,]$species=="mouse"){
    data_all_cats[i,]$Body_mass_g <- 19.3
  }
  if (data_all_cats[i,]$species=="armadillo"){
    data_all_cats[i,]$Body_mass_g <- 3949.01
  }
  if (data_all_cats[i,]$species=="opossum"){
    data_all_cats[i,]$Body_mass_g <- 1134.75
  }
}

# write.csv(data_all_cats, file = "../data/data_all_cats.csv")


# need:
# species
# body mass
# mean speed
# mean log speed
# speed sd
# sd of log speeds
# max speed
# max log speed

species <- c()
body_mass <- c()
mean_v <- c()
mean_log_v <- c()
sd_v <- c()
sd_log_v <- c()
max_v <- c()
max_log_v <- c()

for (i in unique(data_all_cats$species)){
  species <- c(species, i)
  body_mass <- c(body_mass, data_all_cats[data_all_cats$species==i,]$Body_mass_g[1])
  
  spds <- data_all_cats[data_all_cats$species==i,]$speed
  spds <- spds[spds>0]
  mean_v <- c(mean_v, mean(spds))
  sd_v <- c(sd_v, sd(spds))

  spds_log <- log(spds) # log the speeds
  mean_log_v <- c(mean_log_v, mean(spds_log))
  sd_log_v <- c(sd_log_v, sd(spds_log))
  
  max_v <- c(max_v, max(spds))
  max_log_v <- c(max_log_v, max(spds_log))

}

sd_explore_df <- data.frame(species=species, 
                            body_mass=body_mass,
                            mean_v=mean_v,
                            mean_log_v=mean_log_v,
                            sd_v=sd_v,
                            sd_log_v=sd_log_v,
                            max_v=max_v,
                            max_log_v=max_log_v)

# write.csv(sd_explore_df, file = "../results/sd_explore_df.csv")

ggplot(sd_explore_df, aes(x = mean_v, y = max_v))+
  geom_point()


# SD of logged speeds agains mean log speed
sd_v_plot <- ggplot(sd_explore_df, aes(x = mean_log_v, y = sd_log))+
  geom_point()+
  # geom_smooth()+
  theme_minimal()+
  labs(x = "mean log speed (m/s)",
       y = "SD of logged speeds (m/s)")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=15))
sd_v_plot

# body mass plot
body_mass_df <- data.frame(log_body_mass=rep(log(body_mass), times = 6),
                           measure=c(mean_v, mean_log_v, sd_v, sd_log_v, max_v, max_log_v),
                           logged=c(rep("raw", times = length(mean_v)), rep("logged", times = length(mean_log_v)), rep("raw", times = length(mean_v)), rep("logged", times = length(mean_log_v)), rep("raw", times = length(mean_v)), rep("logged", times = length(mean_log_v))),
                           type=c(rep("mean speed", times = 2*length(mean_v)), rep("speed SD", times = 2*length(mean_v)), rep("max speed", times = 2*length(mean_v))))

max_raw_plot <- ggplot(body_mass_df[body_mass_df$logged=="raw" & body_mass_df$log_body_mass<10,])+

ggplot()


body_mass_plot <- ggplot(body_mass_df[body_mass_df$logged=="raw" & body_mass_df$log_body_mass<10,], aes(x = log_body_mass, y = measure))+
  geom_point()+
  # geom_smooth()+
  facet_grid(type~.)+
  theme_bw()+
  labs(x = "body mass (g)",
       y = "",
       title = "raw")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=15),
        strip.text = element_text(size = 14))
body_mass_plot  

body_mass_plot_log <- ggplot(body_mass_df[body_mass_df$logged=="logged" & body_mass_df$log_body_mass<10,], aes(x = log_body_mass, y = measure))+
  geom_point()+
  # geom_smooth()+
  facet_grid(type~.)+
  theme_bw()+
  labs(x = "body mass (g)",
       y = "",
       title = "raw")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=15),
        strip.text = element_text(size = 14))
body_mass_plot  


## --> to better visualise (advice from M):
# no geom_smooth - just the points
# avoid using facets when comparing logged & non logged things or just generally anything that isn't in the same range
# -- bc with facets it means the axes are all the same so it can be harder to discern relationships


## --> conclusion of all this: no clear trends suggesting we need to vary speed SD, so keep it to 1 for the sake of the simulation

# but need to find a relationship between max and mean speed so that can add a truncation parameter to pathgen


# herbivores vs carnivores ------------------------------------------------

# add column for herbivores vs carnivores to data_all_cats:

small_herbivores <- c("mouse", "rat", "squirrel", "Hedgehog", "agouti", "coati")

large_herbivores <- c("paca", "brocket", "peccary", "takin")

small_carnivores <- c("armadillo", "tamandua", "opossum", "Fox", "ocelot")

large_carnivores <- c("himalayan black bear")

data_all_cats['CAT'] <- as.character(NA)

for (i in 1:nrow(data_all_cats)){
  d <- data_all_cats[i,]
  if (d$species %in% small_herbivores){
    d$CAT <- "Small herbivore"
  }
  if (d$species %in% large_herbivores){
    d$CAT <- "Large herbivore"
  }
  if (d$species %in% small_carnivores){
    d$CAT <- "Small carnivore"
  }
  if (d$species %in% large_carnivores){
    d$CAT <- "Large carnivore"
  }
}

# need to figure out why this isn't working

# work out parameter estimates for speed mean & SD ------------------------

# thought process:
# take the data
# log the speeds
# fit a normal distribution to those
# extract mean & sd
# use that mean & sd in the simulation
# simulation then generates a normal distribution of speeds from that mean & sd
# simulation then exponentiates it

# make df of parameter estimates (mean, CV, and logcv) from fitting log normal distributions to each species

data_all_cats <- read.csv("../data/data_all_cats.csv")

species <- c()
size <- c()
mean_log <- c()
mean_log2 <- c()
sd_log <- c()
sd_log2 <- c()
CV_log <- c() # = sdlog/meanlog
PGCV <- c() # = pseudo-geometric coefficient of variation = exp(sdlog)/exp(meanlog)
# logCV <- c()
for (i in unique(data_all_cats$species)){
  spds <- data_all_cats[data_all_cats$species==i,]$speed
  spds <- spds[spds>0] # can't fit lnorm otherwise
  species <- c(species, i)
  size <- c(size, as.character(data_all_cats[data_all_cats$species==i,]$size[1]))
  
  # 2 ways of doing this: log the speeds and use mean & sd from that  
  spds_log <- log(spds) # log the speeds
  mean_log <- c(mean_log, mean(spds_log))
  sd_log <- c(sd_log, sd(spds_log))
  
  # OR: fit lnorm and use mean & sd from that
  fit_lnorm <- fitdist(spds, "lnorm")
  mean_log2 <- c(mean_log2, fit_lnorm$estimate[[1]])
  sd_log2 <- c(sd_log2, fit_lnorm$estimate[[2]])
  
  CV_log <- c(CV_log, (sd(spds_log)/mean(spds_log))) #
  PGCV <- c(PGCV, (exp(sd(spds_log))/exp(mean(spds_log))))
}

# mean_log & mean_log2 are the same
# sd_log & sd_log2 are also almost the same
# so doesn't really matter which you use


parameter_est_df <- data.frame(species = species, size = size, mean_log = mean_log, sd_log = sd_log, CV_log = CV_log, PGCV = PGCV)

# write.csv(parameter_est_df, file = "../data/parameter_est_df.csv")


## now need to decide between CV_log & PGCV -- choose whichever one is most consistent within a spp size category

ggplot(parameter_est_df, aes(x = CV_log, colour = size))+
  geom_density()

ggplot(parameter_est_df, aes(x = PGCV, colour = size))+
  geom_density()

# CV_log seems to behave more nicely so use that

# use the average for each size category and just keep that as a constant:

CV_small <- mean(parameter_est_df[parameter_est_df$size=="small",]$CV_log)
CV_large <- mean(parameter_est_df[parameter_est_df$size=="large",]$CV_log)

# CV_small = -0.5337202
# CV_large = -0.6842889
#--> but use the absolute of both

### explore distributions of speeds...

# 1. By dataset --------------------------------------------------------------

rp_speeds <- ggplot(regentspark_mov_data, aes(x = speed, colour = species))+
  geom_density()+
  labs(title = "Regent's Park")

india_speeds <- ggplot(india_mov_data, aes(x = speed, colour = species))+
  geom_density()+
  labs(title = "India")

panama_speeds <- ggplot(panama_data, aes(x = speed, colour = species))+
  geom_density()+
  theme(legend.position = 'bottom')+
  labs(title = "Panama")

ggarrange(rp_speeds, india_speeds, panama_speeds, nrow = 3)
 


# 2. All together ---------------------------------------------------------


data_all <- data.frame(species = c(as.character(regentspark_mov_data$species), as.character(india_mov_data$species), as.character(panama_data$species)),
                       speed = c(regentspark_mov_data$speed, india_mov_data$speed, panama_data$speed))

data_all$species <- as.character(data_all$species)



# # make them all lower case:
# for (i in 1:nrow(data_all[data_all$species=="Fox",])){
#   #data_all[data_all$species=="Fox", "species"][i] <- "fox"
#   replace(data_all[data_all$species=="Fox", "species"], i, "fox")
# }
# data_all[data_all$species=="Fox", "species"]
# 
# replace(data_all$species, data_all$species=="Fox", "fox")
# data_all[data_all$species=="Fox",]$species <- c(rep(as.character("fox"), length(data_all[data_all$species=="Fox",]$species)))
# 
# # --> can't figure this out... to come back to

speeds_all <- ggplot(data_all, aes(x = log(speed), colour = species))+
  geom_density()
speeds_all




# 3. By size & speed ------------------------------------------------------

# e.g. small and slow, small and fast, large and slow, large and fast

data_all_cats <- data.frame(species = c(as.character(regentspark_mov_data$species), as.character(india_mov_data$species), as.character(panama_data$species)),
                       speed = c(regentspark_mov_data$speed, india_mov_data$speed, panama_data$speed),
                       category = c(rep(NA, length(length(data_all$species)))),
                       size = c(rep(NA, length(length(data_all$species)))))
data_all_cats$category <- as.character(data_all_cats$category)
data_all_cats$category <- as.character(data_all_cats$category)

#write.csv(data_all_cats, file = "data/data_all_cats.csv")

SS <- c("Hedgehog")
SF <- c("mouse", "rat", "squirrel", "agouti", "coati", "armadillo")
LS <- c("himalayan black bear", "paca", "peccary", "tamandua", "takin", "takin young", "opossum")
LF <- c("Fox", "brocket", "ocelot")


for (i in 1:nrow(data_all_cats)){
  
  # if it's one of these species, assign SS (small and slow)
  if (data_all_cats[i, "species"] %in% SS){
    data_all_cats[i,]$category <- "SS"
    data_all_cats[i,]$size <- "small"
  }
  
  # if it's one of these, assign SF (small and fast)
  if (data_all_cats[i, "species"] %in% SF){
    data_all_cats[i,]$category <- "SF"
    data_all_cats[i,]$size <- "small"
  }
  
  # if it's one of these, assign LS (large and slow)
  if (data_all_cats[i, "species"] %in% LS){
    data_all_cats[i,]$category <- "LS"
    data_all_cats[i,]$size <- "large"
  }
  
  # if it's one of these, assign LF (large and fast)
  if (data_all_cats[i, "species"] %in% LF){
    data_all_cats[i,]$category <- "LF"
    data_all_cats[i,]$size <- "large"
  }
}

ggplot(data_all_cats, aes(x = speed, colour = species))+
  geom_density()+
  facet_grid(category ~ .)
# difficult to tell if things are in the right category:
# - LF animals do seem similar
# - the takins look a bit different

# to better visualise things: try plotting small on one and large on another:

ggplot(data_all_cats[data_all_cats$category %in% c("SS", "SF"),], aes(x = speed, colour = species))+
  geom_density()+
  facet_grid(category ~ .)
# they're all pretty similar except for hedgehogs which are consistently slower

ggplot(data_all_cats[data_all_cats$category %in% c("LS", "LF"),], aes(x = speed, colour = species))+
  geom_density()+
  facet_grid(category ~ .)

# himalayan black bear and hedgehogs look pretty similar
ggplot(data_all_cats[data_all_cats$species %in% c("himalayan black bear", "Hedgehog"),], aes(x = speed, colour = species))+
  geom_density()
# -- makes sense that they're similar - both pretty slow

# move things around between fast and slow categories based on how they look on the plots:

# opossums: move from LS to LF:
for (i in 1:nrow(data_all_cats[data_all_cats$species=="opossum",])){
  data_all_cats[data_all_cats$species=="opossum",]$category[i] <- "LF"
}
ggplot(data_all_cats, aes(x = speed, colour = species))+
  geom_density()+
  facet_grid(category ~ .)

# looks like shouldn't have anything other than himlayan black bear and takins in LS: --> move the others to LF
for (i in 1:nrow(data_all_cats[data_all_cats$species=="paca",])){
  data_all_cats[data_all_cats$species=="paca",]$category[i] <- "LF"
}
for (i in 1:nrow(data_all_cats[data_all_cats$species=="peccary",])){
  data_all_cats[data_all_cats$species=="peccary",]$category[i] <- "LF"
}
for (i in 1:nrow(data_all_cats[data_all_cats$species=="peccary",])){
  data_all_cats[data_all_cats$species=="peccary",]$category[i] <- "LF"
}
for (i in 1:nrow(data_all_cats[data_all_cats$species=="tamandua",])){
  data_all_cats[data_all_cats$species=="tamandua",]$category[i] <- "LF"
}

ggplot(data_all_cats, aes(x = speed, colour = species))+
  geom_density()+
  facet_grid(category ~ .)

# squirrels actually look like they could do with being SS rather than SF...?
for (i in 1:nrow(data_all_cats[data_all_cats$species=="squirrel",])){
  data_all_cats[data_all_cats$species=="squirrel",]$category[i] <- "SS"
}
ggplot(data_all_cats, aes(x = speed, colour = species))+
  geom_density()+
  facet_grid(category ~ .)




# 4. By size only ---------------------------------------------------------

# plot just separating small vs large:
small_large_speeds <- ggplot(data_all_cats, aes(x = speed, colour = species))+
  geom_density(size = 0.9)+
  xlab("speed (m/s)")+
  facet_grid(. ~ size)+
  theme_bw()+
  theme(axis.title = element_text(size=22),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.position = "bottom")
small_large_speeds

# png(file="plots/small_large_speeds.png",
#     width=1500, height=1000)
# small_large_speeds
# dev.off()


