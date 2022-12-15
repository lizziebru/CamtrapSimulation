#### SIMULATING DIFFERENT ANIMALS' MOVEMENT PATHS 

# only the first section is used in the write-up/manuscript to work out body mass to travel speed/speedSD/max speed relationships
# everything else is exploration - could be useful in future

require(dplyr)
require(ggplot2)
require(ggpubr)
require(fitdistrplus)
require(stringr)

# functions
source("~/Documents/Project/CamtrapSimulation/code/CamtrapSimulation.R", echo=TRUE)

regentspark_mov_data <- read.csv("../data/regentspark/regentspark_mov_data.csv")
india_mov_data <- read.csv("../data/india/india_mov_data.csv")
panama_data <- read.csv("../data/panama/panama_data.csv")

# combined dataset - with categories of small & large
data_all_cats <- read.csv("../data/combined/data_all_cats.csv")


# linking body mass to travel speed, sd, & max speed using Regent's park & Panama data  ---------------------

### logspeedSD

# use the distribution of speed SDs in the data - but exclude bears & takins bc anomalous

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




## TRAVEL SPEED - BODY MASS RELATIONSHIP

# Rowcliffe et al. 2016: mean travel speed is related to body mass - using Panama data

# mean scaling exponent (faunivores & herbivores) = 0.17
# need the constant though
# --> need to reproduce their work to work out constant so that can calculate mean speed from body mass

# do the same as Rowcliffe et al. but also use regent's park data in addition to Panama data

rp_panama_only <- read.csv("../results/rp_panama_only.csv") # everything apart from bears & takins

# exclude mouse bc only one datapoint
rp_spp <- str_remove(rp_panama_only$species, "mouse") 
rp_panama_only2 <- rp_panama_only[rp_panama_only$species %in% rp_spp,]

# estimate travel speed by fitting log-normal

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
# v_av = 0.1357288*(Mb^0.197178) --> EQN 1 IN MANUSCRIPT




# investigating body mass - speed - SD relationship more closely - NOT USED IN MANUSCRIPT -----------

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


# herbivores vs carnivores - NOT USED IN MANUSCRIPT ------------------------------------------------

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

# work out parameter estimates for speed mean & SD - NOT USED IN MANUSCRIPT ------------------------

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





# exploring speeds of small/large herbivores/carnivores - NOT USED IN MANUSCRIPT -------------------

### small herbivores

# mouse
# spiny rat
# squirrel
# hedgehog
# agouti
# coati

sh_df <- read.csv("data/sh_df.csv")



# distribution of speeds
sh_speeds <- ggplot(sh_df, aes(x = speed, colour = species))+
  geom_density()+
  labs(title = "Small herbivores")+
  theme_minimal()


### large herbivores

# paca
# red brocket deer
# collared peccary
# takin

lh_df <- read.csv("data/lh_df.csv")

# distribution of speeds
lh_speeds <- ggplot(lh_df, aes(x = speed, colour = species))+
  geom_density()+
  labs(title = "Large herbivores")+
  theme_minimal()


### small carnivores

# armadillo
# tamandua
# opossum
# fox
# ocelot

sc_df <- read.csv("data/sc_df.csv")

# distribution of speeds
sc_speeds <- ggplot(sc_df, aes(x = speed, colour = species))+
  geom_density()+
  labs(title = "Small carnivores")+
  theme_minimal()


### large carnivores

# Himalayan black bear


lc_df <- read.csv("../data/lc_df.csv")

# distribution of speeds
lc_speeds <- ggplot(lc_df, aes(x = speed, colour = species))+
  geom_density()+
  labs(title = "Large carnivores")+
  theme_minimal()


speeds_all <- ggarrange(sh_speeds, lh_speeds, sc_speeds, lc_speeds)

png(file="4spp_speeds_all.png",
    width=700, height=600)
speeds_all
dev.off()








# tortuosity exploration using RP data - NOT USED IN MANUSCRIPT ------------------------------------

## TORTUOSITY AND MOVEMENT SPEED

# NB: for future plots: put speed on x-axis and tortuosity on y-axis

# Set-up ------------------------------------------------------------------

require(ggplot2)
require(ggpubr)
require(dplyr)
require(minpack.lm)

## load in and format data

posdata <- read.csv('data/posdat.csv')
movdata <- read.csv('data/movdat.csv')


movdata$sequence <- as.numeric(movdata$sequence)
posdata$sequence <- as.numeric(posdata$sequence)





# working out turn angles - with improvements from Marcus -----------------

# better vectorised way to do it:

# define function to work out the length of a side using the cosine rule
coseqn <- function(a, b, theta){
  
  sqrt(a^2+b^2-2*a*b*cos(theta))
  
}

# define function to work out the angle using the cosine rule
turn <- function(r1, r2, r3, th1, th2, th3){
  
  a <- coseqn(r1, r2,  th2-th1)
  
  b <- coseqn(r2, r3,  th3-th2)
  
  c <- coseqn(r1, r3,  th3-th1)
  
  pi - acos((a^2 + b^2 - c^2) / (2*a*b))
  
}

n <- nrow(posdata)

# work out turn angles for each position data point
angles <- turn(posdata$radius[1:(n-2)], posdata$radius[2:(n-1)], posdata$radius[3:n],
               
               posdata$angle[1:(n-2)],  posdata$angle[2:(n-1)],  posdata$angle[3:n])

angles <- c(NA, angles, NA)

i <- head(cumsum(table(posdata$sequence)), -1) # to get all the sequences with >1 position in it

angles[c(i, i+1)] <- NA # any angles measured between positions from separate sequences - set to NA

posdata$angles <- angles


# now need to make main_df with those new turn angles and see what the relationship looks like:

average <- function(i){
  
  # subset per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # work out mean turn angle:
  mean(k$angles, na.rm = T)
  
}

movdata$turnangle <- sapply(movdata$sequence, average)




# looking at speed-tortuosity relationship --------------------------------

ggplot(movdata, aes(x = turnangle, y = speed, colour = species))+
  geom_point()+
  xlab('tortuosity')

# looks a bit less weird than before?

# try logging speed
ggplot(movdata, aes(x = turnangle, y = log(speed), colour = species))+
  geom_point()+
  xlab('tortuosity')


# first without logging, but separating species is helpful bc expect them to be different:

a <- ggplot(movdata, aes(x = speed, colour = species))+
  geom_density()

b <- ggplot(movdata, aes(x = turnangle, colour = species))+
  geom_density()

ggarrange(a, b, nrow = 2)


# then log to help better discern the relationship (means it's less squished in places and can see the data less clumped together):

c <- ggplot(movdata, aes(x = log(speed), colour = species))+
  geom_density()

d <- ggplot(movdata, aes(x = log(turnangle), colour = species))+
  geom_density()

ggarrange(c, d, nrow = 2)


# also visualise the speed - turnangle relationship

# first without logging:
ggplot(movdata[movdata$species=="Fox",], aes(x = turnangle, y = speed))+
  geom_point()

# then also log things to help visualise:
ggplot(movdata[movdata$species=="Fox",], aes(x = log(turnangle), y = log(speed)))+
  geom_point()


# q: are these distributions just what results from CT biases 



# using gross/net distance ------------------------------------

## investing a potential bias: how does tortuosity relate to speed and how does this compare across different species?

# work out tortuosity for each path: define it as gross distance divided by net distance travelled (seems to be how it's usually done in the literature)

# net distance = distance in movdata

net_dist <- movdata[movdata$sequence==33,]$dist

# gross distance:

# - measure the straight-line distances between successive positions in a sequence then add them up
#--> PROBLEM: is it ok to assume that animals are moving in straight lines between positions? - probably?

# use cosine rule:

# distance between point A and B when:
# A is r1 distance away from CT at angle theta1
# B is r2 distance away from CT at angle theta2
# gross distance = sqrt(r1^2 + r2^2 - 2r1r2cos(theta1 - theta2))

# so for example: Fox's first 2 positions at the start of seq33:

r1 <- 2.3791630
r2 <- 2.3187103
theta1 <- 0.001537844
theta2 <- -0.102266613

gross_dist <- sqrt(r1^2 + r2^2 - 2*r1*r2*cos(theta1 - theta2))


# tortuosity = gross dist divided by net dist:
t <- gross_dist / net_dist
t

# loop:


tortuosity <- list()
for (i in unique(posdata$sequence)) {
  # subset per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # if there is one row then skip to the next one (there was only one position in this sequence):
  if (nrow(k)==1){
    next
  }
  
  # get net dist:
  n <- movdata[movdata$sequence==i,]$dist
  
  # work out gross dist
  gross_dists <- c()
  for (j in 1:(nrow(k)-1)){
    d <- sqrt((k[j,]$radius)^2 + (k[(j+1),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+1),]$radius)*cos(k[j,]$angle - k[(j+1),]$angle))
    gross_dists <- c(gross_dists, d)
  }
  
  # sum gross distances
  g <- sum(gross_dists)
  
  # work out tortuosity
  t <- g / n
  
  # fill tortuosity list
  tortuosity[[paste0("sequence", i)]] <- t
}
tortuosity

# problem: gross distance = net distance for every one
# ask Marcus about how they work out distance?

# what I think net distance is is actually probably gross distance

# so need to work out net distance:


tortuosity2 <- c()
for (i in unique(posdata$sequence)) {
  # subset per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # if there is one row then skip to the next one (there was only one position in this sequence):
  if (nrow(k)==1){
    next
  }
  
  # get gross dist:
  g <- movdata[movdata$sequence==i,]$dist
  
  # work out net dist = distance between first and last
  x <- nrow(k)
  n <- sqrt((k[1,]$radius)^2 + (k[x,]$radius)^2 - 2*(k[1,]$radius)*(k[(x),]$radius)*cos(k[1,]$angle - k[(x),]$angle))
  
  # work out tortuosity
  t <- g / n
  
  # fill tortuosity list
  tortuosity2 <- c(tortuosity2,t)
}
tortuosity2


# the higher the value of t, the more tortuous
t_df <- as.data.frame(cbind(movdata$speed, tortuosity2))
colnames(t_df) <- c("speed", "tortuosity")

t_plot <- ggplot(t_df, aes(x = tortuosity, y = speed))+
  geom_point()
t_plot

# there's a weird one with super high tortuosity: find which one it is:

# it's the 83rd one

# this should correspond to the 83rd row in movdata:
View(movdata[83,])
# --> fox at location 19, sequence 1331

# have a look at the position data for this fox:
View(posdata[posdata$sequence==1331,])

# it looks like it goes back on itself - this is probably what's causing such high tortuosity

# would be nice to plot its actual movement path
# could just use the x & y pixel positions in the image?

tortuous_fox <- ggplot(posdata[posdata$sequence==1331,], aes(x = x, y = y))+
  geom_point()+
  geom_text(aes(label = markerid), hjust = 0.5, vjust = -0.5)
tortuous_fox



# either way: looks like that fox has a pretty tortuous path


# test for relationship between speed and tortuosity


# data are not normal

ggdensity(t_df$speed,
          xlab = 'speed')

ggqqplot(t_df$speed)

## - deviates quite a lot from the normal distribution

shapiro.test(t_df$speed)
# - pretty significantly not normally distributed


# could use Spearman's correlation coefficient: doesn't assume normality
# assumptions: data must be at least ordinal and the scores on one variable must be monotonically related to the other variable

cor.test(t_df$tortuosity, t_df$speed, method = c("spearman"))
#--> significant correlation between them

# plot:

ggscatter(t_df, x = "tortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Tortuosity", ylab = "Speed")

# taking logs:
t_df5 <- cbind(t_df, logtortuosity, logspeed)
ggscatter(t_df5, x = "logtortuosity", y = "logspeed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "log(Tortuosity)", ylab = "log(Speed)")


## RE-DO ALL THIS WITHOUT THE OUTLIER FOX

t_df2 <- t_df[-83,]

# could use Spearman's correlation coefficient: doesn't assume normality
# assumptions: data must be at least ordinal and the scores on one variable must be monotonically related to the other variable

cor.test(t_df2$tortuosity, t_df2$speed, method = c("spearman"))

cor.test(t_df3$logtortuosity, t_df3$speed, method = c("spearman"))

cor.test(t_df4$logtortuosity, t_df4$logspeed, method = c("spearman"))
#--> still significant

logtortuosity <- log(t_df2$tortuosity)

t_df3 <- cbind(t_df2, logtortuosity)

logspeed <- log(t_df3$speed)

t_df4 <- cbind(t_df3, logspeed)

# plot:

ggscatter(t_df2, x = "tortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Tortuosity", ylab = "Speed")

ggscatter(t_df3, x = "logtortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "log(Tortuosity)", ylab = "Speed")

ggscatter(t_df4, x = "logtortuosity", y = "logspeed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "log(Tortuosity)", ylab = "log(Speed)")

###--> but: because tortuosities are super clustered at low values, hard to tell whether there is really a correlation
# (i.e. non-similarity of tortuosity values is skewing the relationship)

# TO DO: investigate more with logs and try to better understand this complex relationship






### Tortuosity & speed relationship - using turn angles

## investing a potential bias: how does tortuosity relate to speed and how does this compare across different species?



# work out turn angles ----------------------------------------------------

# need minimum of 3 points: 1, 2, 3

# cos(angle) = (b^2 + c^2 - a^2) / (2*b*c)
# where: 
# b = dist between point 1 and 2
# c = dist between point 2 and 3
# a = dist between point 1 and 3

turnangles <- c()
seq_no <- c()
for (i in unique(posdata$sequence)) {
  
  # subset per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # if there are fewer than 3 rows then skip to the next one (need 3 positions in the sequence to calculate tortuosity):
  if (nrow(k) < 3){
    next
  }
  
  # vector of sequence numbers that make it past this point:
  seq_no <- c(seq_no, unique(k$sequence))
  
  # select groups of 3 and work out tortuosity for each
  
  x <- c(1:(nrow(k)-2))
  
  angles <- c()
  
  for (j in x) {
    a <- sqrt((k[j,]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+2),]$radius)*cos(k[j,]$angle - k[(j+2),]$angle))
    b <- sqrt((k[j,]$radius)^2 + (k[(j+1),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+1),]$radius)*cos(k[j,]$angle - k[(j+1),]$angle))
    c <- sqrt((k[(j+1),]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[(j+1),]$radius)*(k[(j+2),]$radius)*cos(k[(j+1),]$angle - k[(j+2),]$angle))
    
    angle <- pi - acos((b^2 + c^2 - a^2) / (2*b*c)) # so that highly tortuous paths have a high number (straight-line paths turn angle = 0, complete turn = pi (180deg))
    
    angles <- c(angles, angle)
  }
  
  # for each sequence: work out the mean of all the turn angles involved in the sequence
  
  angles_mean <- mean(angles)
  
  turnangles <- c(turnangles, angles_mean)
  
}
seq_no
turnangles





# --> problem: a few NaNs --> probably bc of problems with inverse cos - maybe look into this more though?

# make main df with sequence no, tortuosity, speed, and species


# problem: clearly not all of these sequence numbers appear in movdata - must've had some for which speeds weren't calculated
#--> need to just use the sequences which appear in both

seqs1 <- movdata[movdata$sequence %in% seq_no, ]$sequence

# get tortuosities for just those sequences:
turnangles2 <- c()
for (i in seqs1) {
  
  # subset per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # select groups of 3 and work out tortuosity for each
  
  x <- c(1:(nrow(k)-2))
  
  angles <- c()
  
  for (j in x) {
    a <- sqrt((k[j,]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+2),]$radius)*cos(k[j,]$angle - k[(j+2),]$angle))
    b <- sqrt((k[j,]$radius)^2 + (k[(j+1),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+1),]$radius)*cos(k[j,]$angle - k[(j+1),]$angle))
    c <- sqrt((k[(j+1),]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[(j+1),]$radius)*(k[(j+2),]$radius)*cos(k[(j+1),]$angle - k[(j+2),]$angle))
    
    angle <- acos((b^2 + c^2 - a^2) / 2*b*c)
    
    angles <- c(angles, angle)
  }
  
  # for each sequence: work out the mean of all the turn angles involved in the sequence
  
  angles_mean <- mean(angles)
  
  turnangles2 <- c(turnangles2, angles_mean)
  
}
turnangles2


main_df <- data.frame(sequence = seqs1,
                      tortuosity = turnangles2,
                      speed = movdata[movdata$sequence %in% seq_no, ]$speed,
                      species = as.character(movdata[movdata$sequence %in% seq_no, ]$species)
)

# remove rows containing NaNs:
main_df <- na.omit(main_df)







# investigating relationship between speed & tortuosity - using all the data -------------------


# initial plot with separate colours for species:

ggplot(main_df, aes(x = tortuosity, y = speed, colour = species))+
  geom_point()

# with spearman regression lines:

f_s1 <- ggscatter(main_df[main_df$species=="Fox",], x = "tortuosity", y = "speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                  title = "Foxes (spearman)")

h_s1 <- ggscatter(main_df[main_df$species=="Hedgehog",], x = "tortuosity", y = "speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                  title = "Hedgehogs (spearman)")

# quite a few outliers still

# generally +ve relationship between speed & tortuosity

# although the data are pretty clustered around similar tortuosities

# need to investigate this relationship in more detail..


# try again using Pearson's?

f_p <- ggscatter(main_df[main_df$species=="Fox",], x = "tortuosity", y = "speed", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                 title = "Foxes (pearson)")

h_p <- ggscatter(main_df[main_df$species=="Hedgehog",], x = "tortuosity", y = "speed", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                 title = "Hedgehogs (pearson)")

ggarrange(f_s1, h_s1, f_p, h_p, nrow = 2, ncol = 2)

# --> doesn't look that different
# but seems like spearman's is probably better bc seems less affected by the outliers than pearson's is




## try again taking logs:

# add logged columns to df:

main_df <- data.frame(main_df,
                      log_tortuosity = log(main_df$tortuosity),
                      log_speed = log(main_df$speed))



# logging just tortuosity:

f_s2 <- ggscatter(main_df[main_df$species=="Fox",], x = "log_tortuosity", y = "speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "log Tortuosity (average turn angles)", ylab = "Speed",
                  title = "Foxes (spearman)")

h_s2 <- ggscatter(main_df[main_df$species=="Hedgehog",], x = "log_tortuosity", y = "speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "log Tortuosity (average turn angles)", ylab = "Speed",
                  title = "Hedgehogs (spearman)")


# logging just speed:

f_s3 <- ggscatter(main_df[main_df$species=="Fox",], x = "tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Foxes (spearman)")

h_s3 <- ggscatter(main_df[main_df$species=="Hedgehog",], x = "tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Hedgehogs (spearman)")


# logging both:

f_s4 <- ggscatter(main_df[main_df$species=="Fox",], x = "log_tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "log Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Foxes (spearman)")

h_s4 <- ggscatter(main_df[main_df$species=="Hedgehog",], x = "log_tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "log Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Hedgehogs (spearman)")


ggarrange(f_s1, h_s1, f_s2, h_s2, f_s3, h_s3, f_s4, h_s4,
          nrow = 4,
          ncol = 2)

#--> logging tortuosity doesn't seem to do much and logging speed makes things worse


# need to better discern this complex-looking relationship between 2 continuous variables


# try fitting some models:


## Linear models: regression:

# 1. explore data to determine whether LM is a good choice

# looking at correlations & what the data look like - LM looks pretty good


# 2. fit Linear Regression Model to data

# initial logging didn't seem to do much - but probs stil worth investigating whether a logistic model could be any good

# fit the linear (regression) model:

f1 <- lm(speed ~ tortuosity, data = fox_df)

summary(f1)

ggplot(fox_df, aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", geom = "smooth")+
  ggtitle("Foxes - linear model")

# diagnostic plots:
par(mfrow = c(2,2), mar = c(5, 5, 1.5, 1.5))
plot(f1)
# normal Q-Q plot not great - quite a lot of deviation at the ends - residuals are not v normally distributed
# scale location plot also not great - the variance of the residuals does change as a function of the predictor
# a couple of points also have rly high leverage

h1 <- lm(speed ~ tortuosity, data = hedgehog_df)

summary(h1)

ggplot(hedgehog_df, aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", geom = "smooth")+
  ggtitle("Hedgehogs - linear model")

# diagnostic plots:
par(mfrow = c(2,2), mar = c(5, 5, 1.5, 1.5))
plot(h1)
# issues with residuals vs fitted: the distribution of residuals has pretty uneven variance
# --> is this something to deal with?
# scale location plot also not great - the variance of the residuals does change as a function of the predictor
# a couple of points also have rly high leverage


# --> problem: all the data are pretty clustered at low tortuosity values

# the diagnostic plots basically show that the slope coefficient estimates are pretty strongly affected by certain data points

# suggests that could be a good idea to get rid of some outliers...


# other models:

# LINEAR MODELS: 

# cubic

f2 <- lm(fox_df$speed ~ poly(fox_df$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(f2)

h2 <- lm(hedgehog_df$speed ~ poly(hedgehog_df$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(h2)


# quadratic

f3 <- lm(fox_df$speed ~ poly(fox_df$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(f3)

h3 <- lm(hedgehog_df$speed ~ poly(hedgehog_df$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(h3)


# NON-LINEAR MODELS

# logistic

logistic_model <- function(t, r_max, K, N_0){ # The classic logistic equation
  return(N_0 * K * exp(r_max * t)/(K + N_0 * (exp(r_max * t) - 1)))
}

N_0_start_f <- min(fox_df$speed) # lowest speed
K_start_f <- max(fox_df$speed) # highest speed
r_max_start_f <- 0.7143 # use estimate from OLS fitting (model f1)

f4 <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), fox_df,
            list(r_max=r_max_start_f, N_0 = N_0_start_f, K = K_start_f))


N_0_start_h <- min(hedgehog_df$speed) # lowest speed
K_start_h <- max(hedgehog_df$speed) # highest speed
r_max_start_h <- 5.7726 # use estimate from OLS fitting (model h1)

h4 <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), hedgehog_df,
            list(r_max=r_max_start_h, N_0 = N_0_start_h, K = K_start_h))


# plotting all the models for each species:

# foxes:

timepoints_f <- seq(0, max(main_df[main_df$species=="Fox",]$tortuosity, na.rm=T), 0.1)
f_logvals <- data.frame(timepoints = timepoints_f,
                        log_vals = log(logistic_model(r_max=coef(f4)[1], N_0 = coef(f4)[2], K = coef(f4)[3], t = timepoints_f)))

f_models <- ggplot(main_df[main_df$species=="Fox", ], aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), se = FALSE, aes(colour = "#CC79A7")) + #add aes colours to tell them apart
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3, raw = TRUE), se = FALSE, aes(colour = "#D55E00")) +
  geom_smooth(method = "loess", data = f_logvals, formula = y ~ x, aes(timepoints, log_vals, colour = "#0072B2")) +
  scale_color_manual(name = NULL, values = c("#CC79A7", "#D55E00", "#0072B2"), labels = c("Quadratic", "Cubic", "Logistic"))+
  guides(col = guide_legend("Model"))+
  ggtitle("Regent's park - foxes")+
  theme_bw()+
  xlim(min(main_df[main_df$species=="Fox",]$tortuosity, na.rm = T), 3)


# hedgehogs:

timepoints_h <- seq(0, max(main_df[main_df$species=="Hedgehog",]$tortuosity, na.rm=T), 0.1)
h_logvals <- data.frame(timepoints = timepoints_h,
                        log_vals = log(logistic_model(r_max=coef(h4)[1], N_0 = coef(h4)[2], K = coef(h4)[3], t = timepoints_h)))

h_models <- ggplot(main_df[main_df$species=="Hedgehog", ], aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), se = FALSE, aes(colour = "#CC79A7")) + #add aes colours to tell them apart
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3, raw = TRUE), se = FALSE, aes(colour = "#D55E00")) +
  geom_smooth(method = "loess", data = h_logvals, formula = y ~ x, aes(timepoints, log_vals, colour = "#0072B2")) +
  scale_color_manual(name = NULL, values = c("#CC79A7", "#D55E00", "#0072B2"), labels = c("Quadratic", "Cubic", "Logistic"))+
  guides(col = guide_legend("Model"))+
  ggtitle("Regent's park - hedgehogs")+
  theme_bw()+
  xlim(min(main_df[main_df$species=="Hedgehog",]$tortuosity, na.rm = T), 1.9)

ggarrange(f_models, h_models, nrow = 2)

# it all looks pretty skewed by outliers...

# logistic looks tentatively the best though













# investigating relationship between speed & tortuosity - without outliers --------


# make new df without 2 fox outliers and 3 hedgehog ones:

# identify which ones they are:
max(main_df[main_df$species=="Fox",]$tortuosity, na.rm = T)

# separate out the fox tortuosities and identify the 2 largest values:

fox_t <- main_df[main_df$species=="Fox",]$tortuosity
remove_f <- tail(unique(fox_t[order(fox_t)]), 2)

hedg_t <- main_df[main_df$species=="Hedgehog",]$tortuosity
remove_h <- tail(unique(hedg_t[order(hedg_t)]), 3)

remove <- c(remove_f, remove_h)

main_df2 <- main_df[!(main_df$tortuosity==remove[1] | main_df$tortuosity==remove[2] | main_df$tortuosity==remove[3] | main_df$tortuosity==remove[4] | main_df$tortuosity==remove[5]),]


# now re-doing everything:

# initial plot with separate colours for species:

ggplot(main_df2, aes(x = tortuosity, y = speed, colour = species))+
  geom_point()

# with spearman regression lines:

f_s1_2 <- ggscatter(main_df2[main_df2$species=="Fox",], x = "tortuosity", y = "speed", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                    title = "Foxes (spearman)")

h_s1_2 <- ggscatter(main_df2[main_df2$species=="Hedgehog",], x = "tortuosity", y = "speed", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                    title = "Hedgehogs (spearman)")


# try again using Pearson's?

f_p_2 <- ggscatter(main_df2[main_df2$species=="Fox",], x = "tortuosity", y = "speed", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                   title = "Foxes (pearson)")

h_p_2 <- ggscatter(main_df2[main_df2$species=="Hedgehog",], x = "tortuosity", y = "speed", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                   title = "Hedgehogs (pearson)")

ggarrange(f_s1_2, h_s1_2, f_p_2, h_p_2, nrow = 2, ncol = 2)

# very similar: probably use spearman's though bc data are not normal




## taking logs:

# logging just tortuosity:

f_s2_2 <- ggscatter(main_df2[main_df2$species=="Fox",], x = "log_tortuosity", y = "speed", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "log Tortuosity (average turn angles)", ylab = "Speed",
                    title = "Foxes (spearman)")

h_s2_2 <- ggscatter(main_df2[main_df2$species=="Hedgehog",], x = "log_tortuosity", y = "speed", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "log Tortuosity (average turn angles)", ylab = "Speed",
                    title = "Hedgehogs (spearman)")


# logging just speed:

f_s3_2 <- ggscatter(main_df2[main_df2$species=="Fox",], x = "tortuosity", y = "log_speed", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "Tortuosity (average turn angles)", ylab = "log Speed",
                    title = "Foxes (spearman)")

h_s3_2 <- ggscatter(main_df2[main_df2$species=="Hedgehog",], x = "tortuosity", y = "log_speed", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "Tortuosity (average turn angles)", ylab = "log Speed",
                    title = "Hedgehogs (spearman)")


# logging both:

f_s4_2 <- ggscatter(main_df2[main_df2$species=="Fox",], x = "log_tortuosity", y = "log_speed", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "log Tortuosity (average turn angles)", ylab = "log Speed",
                    title = "Foxes (spearman)")

h_s4_2 <- ggscatter(main_df2[main_df2$species=="Hedgehog",], x = "log_tortuosity", y = "log_speed", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "log Tortuosity (average turn angles)", ylab = "log Speed",
                    title = "Hedgehogs (spearman)")


ggarrange(f_s1_2, h_s1_2, f_s2_2, h_s2_2, f_s3_2, h_s3_2, f_s4_2, h_s4_2,
          nrow = 4,
          ncol = 2)

#--> logging tortuosity doesn't seem to do much but logging speed does do something - looks like an asymptotal relatioship..




### try fitting some models:

# linear regression model looked pretty crap before so don't do it again here


# other models:

# LINEAR MODELS: 

# cubic

f2_2 <- lm(main_df2[main_df2$species=="Fox",]$speed ~ poly(main_df2[main_df2$species=="Fox",]$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(f2_2)

h2_2 <- lm(main_df2[main_df2$species=="Hedgehog",]$speed ~ poly(main_df2[main_df2$species=="Hedgehog",]$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(h2_2)


# quadratic

f3_2 <- lm(main_df2[main_df2$species=="Fox",]$speed ~ poly(main_df2[main_df2$species=="Fox",]$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(f3_2)

h3_2 <- lm(main_df2[main_df2$species=="Hedgehog",]$speed ~ poly(main_df2[main_df2$species=="Hedgehog",]$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(h3_2)


# NON-LINEAR MODELS

# logistic

logistic_model <- function(t, r_max, K, N_0){ # The classic logistic equation
  return(N_0 * K * exp(r_max * t)/(K + N_0 * (exp(r_max * t) - 1)))
}

N_0_start_f_2 <- min(main_df2[main_df2$species=="Fox",]$speed) # lowest speed
K_start_f_2 <- max(main_df2[main_df2$species=="Fox",]$speed) # highest speed
r_max_start_f_2 <- 2.4577  # use estimate from OLS fitting:
f1_2 <- lm(speed ~ tortuosity, data = main_df2[main_df2$species=="Fox",])

f4_2 <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), main_df2[main_df2$species=="Fox",],
              list(r_max=r_max_start_f_2, N_0 = N_0_start_f_2, K = K_start_f_2))


N_0_start_h_2 <- min(main_df2[main_df2$species=="Hedgehog",]$speed) # lowest speed
K_start_h_2 <- max(main_df2[main_df2$species=="Hedgehog",]$speed) # highest speed
r_max_start_h_2 <- 14.139 # use estimate from OLS fitting (model h1_2):
h1_2 <- lm(speed ~ tortuosity, data = main_df2[main_df2$species=="Hedgehog",])

h4 <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), main_df2[main_df2$species=="Hedgehog",],
            list(r_max=r_max_start_h_2, N_0 = N_0_start_h_2, K = K_start_h_2))
# starting values aren't good enough for a model to converge - need to find better ones


# plotting all the models for each species:

# foxes:

timepoints_f_2 <- seq(0, max(main_df2[main_df2$species=="Fox",]$tortuosity, na.rm=T), 0.1)
f_logvals_2 <- data.frame(timepoints = timepoints_f,
                          log_vals = log(logistic_model(r_max=coef(f4_2)[1], N_0 = coef(f4_2)[2], K = coef(f4_2)[3], t = timepoints_f)))

f_models_2 <- ggplot(main_df2[main_df2$species=="Fox", ], aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), se = FALSE, aes(colour = "#CC79A7")) + #add aes colours to tell them apart
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3, raw = TRUE), se = FALSE, aes(colour = "#D55E00")) +
  geom_smooth(method = "loess", data = f_logvals_2, formula = y ~ x, aes(timepoints, log_vals, colour = "#0072B2")) +
  scale_color_manual(name = NULL, values = c("#CC79A7", "#D55E00", "#0072B2"), labels = c("Quadratic", "Cubic", "Logistic"))+
  guides(col = guide_legend("Model"))+
  ggtitle("Regent's park - foxes")+
  theme_bw()+
  xlim(min(main_df2[main_df2$species=="Fox",]$tortuosity, na.rm = T), 3)
f_models

# logistic looks pretty promising!






# TO DO

# for foxes:
# - scrap quadratic
# - keep cubic & logistic
# - find another model that might better describe the asymptote


# hedgehogs:
# --> haven't done this yet cause haven't been able to fit the logistic model 
# - if this stuff is useful then invest more time in it and try to fit logistic & other models



# also maybe try more transformations?








# considering the no. of images per sequence ----------------------------------

### look at the distribution of no. of images per sequence

# add column about this to main_df2:

# for every sequence no. in main_df2: count the number of rows with that sequence value in posdata:

images <- c()
for (i in unique(main_df2$sequence)) {
  
  # subset posdata per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # count the number of rows and fill images with it
  r <- nrow(k)
  
  images <- c(images, r)
  
}

main_df2 <- cbind(main_df2, images)

ggdensity(main_df2$images,
          xlab = "Number of images per sequence")

# separate out by species:

f_imag <- ggdensity(main_df2[main_df2$species=="Fox",]$images,
                    xlab = "Number of images per sequence",
                    title = "Foxes")

h_imag <- ggdensity(main_df2[main_df2$species=="Hedgehog",]$images,
                    xlab = "Number of images per sequence",
                    title = "Hedgehog")

ggarrange(f_imag, h_imag, nrow = 2)
# there are more foxes moving faster (i.e. that have small no. of images per sequence) than hedgehogs
# both peak at around 10 images per sequence




# Could think about no. of points - do analysis which includes the no. of points in the sequence to address those with low speeds & low tortuosity
# --> not sure about what Chris means by this exactly - discuss











# extra bits of code not currently using ----------------------------------

# separate out into species:

posdata_fox <- posdata[posdata$species=="Fox",]

posdata_hedgehog <- posdata[posdata$species=="Hedgehog",]


# re-do tortuosity stuff for each species:

turnangles_f <- c()
seq_no_f <- c()
for (i in unique(posdata_fox$sequence)) {
  
  # subset per sequence number:
  k <- posdata_fox[posdata_fox$sequence==i,] 
  
  # if there are fewer than 3 rows then skip to the next one (need 3 positions in the sequence to calculate tortuosity):
  if (nrow(k) < 3){
    next
  }
  
  # vector of sequence numbers that make it past this point:
  seq_no_f <- c(seq_no_f, unique(k$sequence))
  
  # select groups of 3 and work out mean tortuosity for each
  
  x <- c(1:(nrow(k)-2))
  
  angles <- c()
  
  for (j in x) {
    a <- sqrt((k[j,]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+2),]$radius)*cos(k[j,]$angle - k[(j+2),]$angle))
    b <- sqrt((k[j,]$radius)^2 + (k[(j+1),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+1),]$radius)*cos(k[j,]$angle - k[(j+1),]$angle))
    c <- sqrt((k[(j+1),]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[(j+1),]$radius)*(k[(j+2),]$radius)*cos(k[(j+1),]$angle - k[(j+2),]$angle))
    
    angle <- acos((b^2 + c^2 - a^2) / 2*b*c)
    
    angles <- c(angles, angle)
  }
  
  angles_mean <- mean(angles)
  
  turnangles_f <- c(turnangles_f, angles_mean)
  
}

tortuosity_fox <- cbind(seq_no_f, turnangles_f)

##  looking at relationship between speed & tortuosity using turnangles as a measure of tortuosity:

fox_df <- as.data.frame(cbind(movdata[movdata$sequence %in% seq_no_f, ]$speed, tortuosity_fox))

colnames(fox_df) <- c("speed", "sequence", "tortuosity")

f_plot <- ggscatter(fox_df, 
                    x = "tortuosity", 
                    y = "speed", 
                    add = "reg.line", 
                    conf.int = TRUE, 
                    cor.coef = TRUE, 
                    cor.method = "spearman",
                    xlab = "Tortuosity (average turn angles)", 
                    ylab = "Speed",
                    title = "Regent's park: foxes")


turnangles_h <- c()
seq_no_h <- c()
for (i in unique(posdata_hedgehog$sequence)) {
  
  # subset per sequence number:
  k <- posdata_hedgehog[posdata_hedgehog$sequence==i,] 
  
  # if there are fewer than 3 rows then skip to the next one (need 3 positions in the sequence to calculate tortuosity):
  if (nrow(k) < 3){
    next
  }
  
  # vector of sequence numbers that make it past this point:
  seq_no_h <- c(seq_no_h, unique(k$sequence))
  
  # select groups of 3 and work out mean tortuosity for each
  
  x <- c(1:(nrow(k)-2))
  
  angles <- c()
  
  for (j in x) {
    a <- sqrt((k[j,]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+2),]$radius)*cos(k[j,]$angle - k[(j+2),]$angle))
    b <- sqrt((k[j,]$radius)^2 + (k[(j+1),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+1),]$radius)*cos(k[j,]$angle - k[(j+1),]$angle))
    c <- sqrt((k[(j+1),]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[(j+1),]$radius)*(k[(j+2),]$radius)*cos(k[(j+1),]$angle - k[(j+2),]$angle))
    
    angle <- acos((b^2 + c^2 - a^2) / 2*b*c)
    
    angles <- c(angles, angle)
  }
  
  angles_mean <- mean(angles)
  
  turnangles_h <- c(turnangles_h, angles_mean)
  
}

tortuosity_hedgehog <- cbind(seq_no_h, turnangles_h)

##  looking at relationship between speed & tortuosity using turnangles as a measure of tortuosity:

hedgehog_df <- as.data.frame(cbind(movdata[movdata$sequence %in% seq_no_h, ]$speed, tortuosity_hedgehog))

colnames(hedgehog_df) <- c("speed", "sequence", "tortuosity")

h_plot <- ggscatter(hedgehog_df, 
                    x = "tortuosity", 
                    y = "speed", 
                    add = "reg.line", 
                    conf.int = TRUE, 
                    cor.coef = TRUE, 
                    cor.method = "spearman",
                    xlab = "Tortuosity (average turn angles)", 
                    ylab = "Speed",
                    title = "Regent's park: hedgehogs")

# make multi-panel plot for the different species:

ggarrange(f_plot, h_plot, nrow = 2)


# try again using Pearson's?

f_plot5 <- ggscatter(fox_df, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "pearson",
                     xlab = "Tortuosity (average turn angles)", 
                     ylab = "Speed",
                     title = "Regent's park: foxes (Pearson's)")

h_plot5 <- ggscatter(hedgehog_df, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "pearson",
                     xlab = "Tortuosity (average turn angles)", 
                     ylab = "Speed",
                     title = "Regent's park: hedgehogs (Pearson's)")


ggarrange(f_plot5, h_plot5, nrow = 2)

# --> doesn't look that different
# but seems like spearman's is probably better bc seems less affected by the outliers than pearson's is





## try again taking logs:

# logging tortuosity:

fox_df2 <- fox_df

fox_df2$tortuosity <- log(fox_df2$tortuosity)

f_plot2 <- ggscatter(fox_df2, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "log(tortuosity) (average turn angles)", 
                     ylab = "Speed",
                     title = "Regent's park: foxes")



hedgehog_df2 <- hedgehog_df

hedgehog_df2$tortuosity <- log(hedgehog_df2$tortuosity)

h_plot2 <- ggscatter(hedgehog_df2, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "log(tortuosity) (average turn angles)", 
                     ylab = "Speed",
                     title = "Regent's park: hedgehogs")

ggarrange(f_plot2, h_plot2, nrow = 2)


# logging speed:

fox_df3 <- fox_df

fox_df3$speed <- log(fox_df3$speed)

f_plot3 <- ggscatter(fox_df3, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "Tortuosity (average turn angles)", 
                     ylab = "log(speed)",
                     title = "Regent's park: foxes")



hedgehog_df3 <- hedgehog_df

hedgehog_df3$speed <- log(hedgehog_df3$speed)

h_plot3 <- ggscatter(hedgehog_df3, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "Tortuosity (average turn angles)", 
                     ylab = "log(speed)",
                     title = "Regent's park: hedgehogs")

ggarrange(f_plot3, h_plot3, nrow = 2)




# logging both:

fox_df4 <- fox_df

fox_df4$speed <- log(fox_df4$speed)

fox_df4$tortuosity <- log(fox_df4$tortuosity)

f_plot4 <- ggscatter(fox_df4, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "log(tortuosity) (average turn angles)", 
                     ylab = "log(speed)",
                     title = "Regent's park: foxes")



hedgehog_df4 <- hedgehog_df

hedgehog_df4$speed <- log(hedgehog_df4$speed)

hedgehog_df4$tortuosity <- log(hedgehog_df4$tortuosity)

h_plot4 <- ggscatter(hedgehog_df4, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "log(tortuosity) (average turn angles)", 
                     ylab = "log(speed)",
                     title = "Regent's park: hedgehogs")

ggarrange(f_plot4, h_plot4, nrow = 2)


# need to better discern this complex-looking relationship between 2 continuous variables


## Linear models: regression:

# 1. explore data to determine whether LM is a good choice

# looking at correlations & what the data look like - LM looks pretty good


# 2. fit Linear Regression Model to data

# initial logging didn't seem to do much - but probs stil worth investigating whether a log linear model could be any good

# fit the linear (regression) model:

f1 <- lm(speed ~ tortuosity, data = fox_df)
summary(f1)
ggplot(fox_df, aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", geom = "smooth")+
  ggtitle("Foxes - linear model")
par(mfrow = c(2,2), mar = c(5, 5, 1.5, 1.5))
plot(f1)
# normal Q-Q plot not great - quite a lot of deviation at the ends - residuals are not v normally distributed
# scale location plot also not great - the variance of the residuals does change as a function of the predictor
# a couple of points also have rly high leverage

h1 <- lm(speed ~ tortuosity, data = hedgehog_df)
summary(h1)
ggplot(hedgehog_df, aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", geom = "smooth")+
  ggtitle("Hedgehogs - linear model")
par(mfrow = c(2,2), mar = c(5, 5, 1.5, 1.5))
plot(h1)
# issues with residuals vs fitted: the distribution of residuals has pretty uneven variance
# --> is this something to deal with?
# scale location plot also not great - the variance of the residuals does change as a function of the predictor
# a couple of points also have rly high leverage


# --> problem: all the data are pretty clustered at low tortuosity values

# the diagnostic plots basically show that the slope coefficient estimates are pretty strongly affected by certain data points

# other models could do:

# LINEAR MODELS 

# cubic

f2 <- lm(fox_df$speed ~ poly(fox_df$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(f2)

h2 <- lm(hedgehog_df$speed ~ poly(hedgehog_df$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(h2)


# quadratic

f3 <- lm(fox_df$speed ~ poly(fox_df$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(f3)

h3 <- lm(hedgehog_df$speed ~ poly(hedgehog_df$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(h3)


# NON-LINEAR MODELS

# logistic

logistic_model <- function(t, r_max, K, N_0){ # The classic logistic equation
  return(N_0 * K * exp(r_max * t)/(K + N_0 * (exp(r_max * t) - 1)))
}

N_0_start_f <- min(fox_df$speed) # lowest speed
K_start_f <- max(fox_df$speed) # highest speed
r_max_start_f <- 0.7143 # use estimate from OLS fitting (model f1)

f_log <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), fox_df,
               list(r_max=r_max_start_f, N_0 = N_0_start_f, K = K_start_f))


N_0_start_h <- min(hedgehog_df$speed) # lowest speed
K_start_h <- max(hedgehog_df$speed) # highest speed
r_max_start_h <- 5.7726 # use estimate from OLS fitting (model h1)

h_log <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), hedgehog_df,
               list(r_max=r_max_start_h, N_0 = N_0_start_h, K = K_start_h))


# plotting all the models for each species:

# foxes:

timepoints <- seq(0, max(fox_df$tortuosity, na.rm=T), 0.1)
f_logvals <- data.frame(timepoints = timepoints,
                        log_vals = log(logistic_model(r_max=coef(f_log)[1], N_0 = coef(f_log)[2], K = coef(f_log)[3], t = timepoints)))

f_models <- ggplot(fox_df, aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), se = FALSE, aes(colour = "#CC79A7")) + #add aes colours to tell them apart
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3, raw = TRUE), se = FALSE, aes(colour = "#D55E00")) +
  geom_smooth(method = "loess", data = f_logvals, formula = y ~ x, aes(timepoints, log_vals, colour = "#0072B2")) +
  scale_color_manual(name = NULL, values = c("#CC79A7", "#D55E00", "#0072B2"), labels = c("Quadratic", "Cubic", "Logistic"))+
  guides(col = guide_legend("Model"))+
  ggtitle("Regent's park - foxes")+
  theme_bw()+
  xlim(min(fox_df$tortuosity, na.rm = T), 3)
f_models

# it all still looks pretty skewed by outliers...

# would potentially be good to get rid of some: could get rid of the 2 with super high tortuosity potentially

# make new fox df without those 2 outliers:



# 3. determine whether the model fits adequately the data



## -- end of tortuosity exploration using RP data









# MODELLING BIMODAL SPEEDS USING PABLO'S DATA - not used in the end-----------------------------


# new way to work out bimodal speed distributions -------------------------

# unimodal speeds:
n=500
speedCor=0.9
Mb=50
vmax <- (8.356367*(Mb^0.25892))/(Mb^(0.06237*log10(Mb))) # using body mass relationship from Garland 1983

# unimodal speeds:
pTurn = 0.5
logspeedSD <- 0.8546151 # set fixed logspeedSD (calculated using regent's park & panama data)
logspeed <- log(0.1357288*(Mb^0.197178)) # set logspeed - using body mass relationship derived from regent's park & panama data (fitting lnorm)
spds <- exp(rautonorm(n, logspeed, logspeedSD, speedCor)) 
spds <- spds[spds<vmax]

# bimodal speeds: using Pablo's data:

logspeedSD_mov <- 0.8656343 
logspeed_mov <- log(2.489792*(Mb^0.04714894)) # this is the problematic one --> moving speeds are pretty fast -- too fast to compare usefully with our unimodal distribution
logspeedSD_feed <- 1.04981 
logspeed_feed <- log(1.081785*(Mb^0.1410986))


## so ditch using Pablo's data and instead generate a bimodal distribution artificially
# unimodal distribution:
plot(density(log(spds)))
# mean of slow speeds: 30% decrease:
logspeed_slow <- log(0.1357288*(Mb^0.197178)*0.7)
spds_slow <- exp(rautonorm(n, logspeed_slow, logspeedSD, speedCor)) 
spds_slow <- spds_slow[spds_slow<vmax]

# mean of fast speeds: 30% increase:
logspeed_fast <- log(0.1357288*(Mb^0.197178)*1.3)
spds_fast <- exp(rautonorm(n, logspeed_fast, logspeedSD, speedCor)) 
spds_fast <- spds_fast[spds_fast<vmax]


density_df <- data.frame(spds = c(log(spds), log(spds_slow), log(spds_fast)),
                         type = c(rep("uni", times = length(spds)), rep("slow", times=length(spds_slow)), rep("fast", times=length(spds_fast))))

density_plot <- ggplot(density_df, aes(x=spds, colour=type))+
  geom_density()
density_plot

# actually probs don't need to change SD --> looks fine


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

pablo_spds_raw <- read.csv("../data/spain/palencia_spds_raw.csv")

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













