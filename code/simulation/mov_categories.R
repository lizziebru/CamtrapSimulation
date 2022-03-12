#### SIMULATING DIFFERENT ANIMALS' MOVEMENT PATHS 

require(dplyr)


# by speed ----------------------------------------------------------------

# distributions of speeds:

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
 


# combine the data so that can plot things together:

data_all <- data.frame(species = c(as.character(regentspark_mov_data$species), as.character(india_mov_data$species), as.character(panama_data$species)),
                       speed = c(regentspark_mov_data$speed, india_mov_data$speed, panama_data$speed))

data_all$species <- as.character(data_all$species)

# make them all lower case:
for (i in 1:nrow(data_all[data_all$species=="Fox",])){
  #data_all[data_all$species=="Fox", "species"][i] <- "fox"
  replace(data_all[data_all$species=="Fox", "species"], i, "fox")
}
data_all[data_all$species=="Fox", "species"]

replace(data_all$species, data_all$species=="Fox", "fox")
data_all[data_all$species=="Fox",]$species <- c(rep(as.character("fox"), length(data_all[data_all$species=="Fox",]$species)))

# --> can't figure this out... to come back to



speeds_all <- ggplot(data_all, aes(x = speed, colour = species))+
  geom_density()
speeds_all

# try separating them out and looking for patterns:

# e.g. small and slow, small and fast, large and slow, large and fast

data_all_cats <- data.frame(species = c(as.character(regentspark_mov_data$species), as.character(india_mov_data$species), as.character(panama_data$species)),
                       speed = c(regentspark_mov_data$speed, india_mov_data$speed, panama_data$speed),
                       category = c(rep(NA, length(length(data_all$species)))))
data_all_cats$category <- as.character(data_all_cats$category)


SS <- c("Hedgehog", "opossum")
SF <- c("mouse", "rat", "squirrel", "agouti", "coati")
LS <- c("himalayan black bear", "paca", "peccary", "tamandua", "takin", "takin young")
LF <- c("Fox", "armadillo", "brocket", "ocelot")

for (i in 1:nrow(data_all_cats)){
  
  # if it's one of these species, assign SS (small and slow)
  if (data_all_cats[i, "species"] %in% SS){
    data_all_cats[i,]$category <- "SS"
  }
  
  # if it's one of these, assign SF (small and fast)
  if (data_all_cats[i, "species"] %in% SF){
    data_all_cats[i,]$category <- "SF"
  }
  
  # if it's one of these, assign LS (large and slow)
  if (data_all_cats[i, "species"] %in% LS){
    data_all_cats[i,]$category <- "LS"
  }
  
  # if it's one of these, assign LF (large and fast)
  if (data_all_cats[i, "species"] %in% LF){
    data_all_cats[i,]$category <- "LF"
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

# opossums: move from SS to SF:
for (i in 1:nrow(data_all_cats[data_all_cats$species=="opossum",])){
  data_all_cats[data_all_cats$species=="opossum",]$category[i] <- "SF"
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

# by variation in speed ---------------------------------------------------




