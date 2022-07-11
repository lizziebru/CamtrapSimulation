#### SIMULATING DIFFERENT ANIMALS' MOVEMENT PATHS 

require(dplyr)
require(ggplot2)
require(ggpubr)
require(fitdistrplus)
require(stringr)

regentspark_mov_data <- read.csv("../data/regentspark_mov_data.csv")
india_mov_data <- read.csv("../data/india_mov_data.csv")
panama_data <- read.csv("../data/panama_data.csv")

data_all_cats <- read.csv("../data/data_all_cats.csv")



# deciding on speedSD value and truncation parameter ---------------------

### logspeedSD - use the distribution of speed SDs in the data - but exclude bears & takins bc anomalous
rp_panama_only <- read.csv("../results/rp_panama_only.csv") # everything apart from bears & takins
plot(density(rp_panama_only$sd_log_v))

sd_log_density <- ggplot(rp_panama_only, aes(x = sd_log_v))+
  geom_density()+
  geom_vline(xintercept = mean(rp_panama_only$sd_log_v), colour = "red")
sd_log_density

# use the mean of this as the fixed speed SD - bc there's no literature on the variation of speedSD with body size
# and empirical data suggest it remains relatively constant across body masses too
fixed_logspeedSD <- mean(rp_panama_only$sd_log_v) ## == 0.8546151





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


