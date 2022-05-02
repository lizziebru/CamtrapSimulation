### all data exploration to inform simulating 4 types of species ###

regentspark_mov_data <- read.csv("data/regentspark_mov_data.csv")
india_mov_data <- read.csv("data/india_mov_data.csv")
panama_data <- read.csv("data/panama_data.csv")



# small herbivores --------------------------------------------------------

# mouse
# spiny rat
# squirrel
# hedgehog
# agouti
# coati

# small_herb_spp <- c("mouse", "rat", "squirrel", "Hedgehog", "agouti", "coati")
# 
# sh_spp <- as.character(c())
# sh_speeds <- c()
# 
# for (i in 1:nrow(panama_data)){
#   r <- panama_data[i,]
#   if (r$species %in% small_herb_spp){
#     sh_spp <- c(sh_spp, as.character(r$species))
#     sh_speeds <- c(sh_speeds, r$speed)
#   }
# }
# 
# for (i in 1:nrow(regentspark_mov_data)){
#   r <- regentspark_mov_data[i,]
#   if (r$species %in% small_herb_spp){
#     sh_spp <- c(sh_spp, as.character(r$species))
#     sh_speeds <- c(sh_speeds, r$speed)
#   }
# }
# 
# for (i in 1:nrow(india_mov_data)){
#   r <- india_mov_data[i,]
#   if (r$species %in% small_herb_spp){
#     sh_spp <- c(sh_spp, as.character(r$species))
#     sh_speeds <- c(sh_speeds, r$speed)
#   }
# }
# 
# sh_df <- data.frame(species = sh_spp,
#                     speed = sh_speeds)
# 
# write.csv(sh_df, file = "data/sh_df.csv")

sh_df <- read.csv("data/sh_df.csv")



# distribution of speeds
sh_speeds <- ggplot(sh_df, aes(x = speed, colour = species))+
  geom_density()+
  labs(title = "Small herbivores")+
  theme_minimal()




# large herbivores ---------------------------------------------------------

# paca
# red brocket deer
# collared peccary
# takin

# large_herb_spp <- c("paca", "brocket", "peccary", "takin", "takin young")
# 
# lh_spp <- as.character(c())
# lh_speeds <- c()
# 
# for (i in 1:nrow(panama_data)){
#   r <- panama_data[i,]
#   if (r$species %in% large_herb_spp){
#     lh_spp <- c(lh_spp, as.character(r$species))
#     lh_speeds <- c(lh_speeds, r$speed)
#   }
# }
# 
# for (i in 1:nrow(regentspark_mov_data)){
#   r <- regentspark_mov_data[i,]
#   if (r$species %in% large_herb_spp){
#     lh_spp <- c(lh_spp, as.character(r$species))
#     lh_speeds <- c(lh_speeds, r$speed)
#   }
# }
# 
# for (i in 1:nrow(india_mov_data)){
#   r <- india_mov_data[i,]
#   if (r$species %in% large_herb_spp){
#     lh_spp <- c(lh_spp, as.character(r$species))
#     lh_speeds <- c(lh_speeds, r$speed)
#   }
# }
# 
# lh_df <- data.frame(species = lh_spp,
#                     speed = lh_speeds)
# 
# write.csv(lh_df, file = "data/lh_df.csv")

lh_df <- read.csv("data/lh_df.csv")

# distribution of speeds
lh_speeds <- ggplot(lh_df, aes(x = speed, colour = species))+
  geom_density()+
  labs(title = "Large herbivores")+
  theme_minimal()

# small carnivores --------------------------------------------------------

# armadillo
# tamandua
# opossum
# fox
# ocelot

# small_carn_spp <- c("armadillo", "tamandua", "opossum", "Fox", "ocelot")
# 
# sc_spp <- as.character(c())
# sc_speeds <- c()
# 
# for (i in 1:nrow(panama_data)){
#   r <- panama_data[i,]
#   if (r$species %in% small_carn_spp){
#     sc_spp <- c(sc_spp, as.character(r$species))
#     sc_speeds <- c(sc_speeds, r$speed)
#   }
# }
# 
# for (i in 1:nrow(regentspark_mov_data)){
#   r <- regentspark_mov_data[i,]
#   if (r$species %in% small_carn_spp){
#     sc_spp <- c(sc_spp, as.character(r$species))
#     sc_speeds <- c(sc_speeds, r$speed)
#   }
# }
# 
# for (i in 1:nrow(india_mov_data)){
#   r <- india_mov_data[i,]
#   if (r$species %in% small_carn_spp){
#     sc_spp <- c(sc_spp, as.character(r$species))
#     sc_speeds <- c(sc_speeds, r$speed)
#   }
# }
# 
# sc_df <- data.frame(species = sc_spp,
#                     speed = sc_speeds)
# 
# write.csv(sc_df, file = "data/sc_df.csv")

sc_df <- read.csv("data/sc_df.csv")

# distribution of speeds
sc_speeds <- ggplot(sc_df, aes(x = speed, colour = species))+
  geom_density()+
  labs(title = "Small carnivores")+
  theme_minimal()




# large carnivores --------------------------------------------------------

# Himalayan black bear

# large_carn_spp <- c("himalayan black bear")
# 
# lc_spp <- as.character(c())
# lc_speeds <- c()
# 
# 
# for (i in 1:nrow(india_mov_data)){
#   r <- india_mov_data[i,]
#   if (r$species %in% large_carn_spp){
#     lc_spp <- c(lc_spp, as.character(r$species))
#     lc_speeds <- c(lc_speeds, r$speed)
#   }
# }
# 
# lc_df <- data.frame(species = lc_spp,
#                     speed = lc_speeds)
# 
# write.csv(lc_df, file = "data/lc_df.csv")

sc_df <- read.csv("data/sc_df.csv")

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

