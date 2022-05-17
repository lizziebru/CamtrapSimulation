#### SIMULATING DIFFERENT ANIMALS' MOVEMENT PATHS 

require(dplyr)
require(ggplot2)
require(ggpubr)

regentspark_mov_data <- read.csv("../data/regentspark_mov_data.csv")
india_mov_data <- read.csv("../data/india_mov_data.csv")
panama_data <- read.csv("../data/panama_data.csv")


# work out parameter estimates for speed mean & SD ------------------------

# make df of parameter estimates (mean, CV, and logcv) from fitting log normal distributions to each species

data_all <- data.frame(species = c(as.character(regentspark_mov_data$species), as.character(india_mov_data$species), as.character(panama_data$species)),
                       speed = c(regentspark_mov_data$speed, india_mov_data$speed, panama_data$speed))

data_all$species <- as.character(data_all$species)

species <- c()
mean <- c()
CV <- c()
logCV <- c()
for (i in unique(data_all$species)){
  spds <- data_all[data_all$species==i,]$speed
  species <- c(species, i)
  mean <- c(mean, mean(spds)) 
  CV <- c(CV, (sd(spds)/mean(spds))) # CV = sd/mean
  logCV <- c(logCV, log(sd(spds)/mean(spds)))
}

parameter_est_df <- data.frame(species = species, mean = mean, CV = CV, logCV = logCV)

# write.csv(parameter_est_df, file = "../data/parameter_est_df.csv")

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

speeds_all <- ggplot(data_all, aes(x = speed, colour = species))+
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


