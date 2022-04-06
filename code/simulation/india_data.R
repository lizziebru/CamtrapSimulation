### sort out India data ###

# require(devtools)
require(jpeg)
require(tidyverse)

# general exploration -----------------------------------------------------

india_mov_data <- read.csv('data/india_mov_data.csv')

# exploring the data a bit:
unique(india_mov_data$species)
max(india_mov_data$speed)
min(india_mov_data$speed)

# distribution of number of points in sequences
ggplot(india_mov_data, aes(x = number_of_points))+
  geom_density()
max(india_mov_data$number_of_points)
min(india_mov_data$number_of_points)
mean(india_mov_data$number_of_points)


# initial formatting & removing anomalously high speeds ----------------------------------------

# # initial formatting
# india_pos_data <- read.csv('data/india_pos_data.csv') # read in position data (one row = one position)
# devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/CTtracking/master/CTtracking.r")
# india_mov_data <- seq.summary(india_pos_data) # format it with one row per sequence
# write.csv(india_mov_data, 'data/india_mov_data.csv', row.names = FALSE)

## removing weidly high speeds
# # units: surely they're not in m/s..?
# 
# ggplot(india_mov_data, aes(x = speed, fill = species))+
#   geom_density()
# 
# ggplot(india_mov_data, aes(x = speed))+
#   geom_density()
# # - looks weird
# 
# # try removing the max speed:
# 
# india2 <- india_mov_data[india_mov_data$speed<max(india_mov_data$speed),]
# 
# ggplot(india2, aes(x = speed))+
#   geom_density()
# 
# max(india2$speed)
# # second highest speed is also pretty unrealistically high if it's in m/s
# 
# # remove the second highest:
# india3 <- india2[india2$speed < max(india2$speed),]
# 
# ggplot(india3, aes(x = speed, colour = species))+
#   geom_density()
# 
# max(india3$speed)
# # - looks more normal - but still around 20mph which is pretty fast
# 
# # removing this one too:
# india4 <- india3[india3$speed < max(india3$speed),]
# 
# ggplot(india4, aes(x = speed, colour = species))+
#   geom_density()
# 
# 
# ## units are in m/s but there may well be some outlying speeds stemming from some problems with the distance prediction process
# # ideally these would be checked and redigitised
# # but for my purposes M suggests I just remove any sequences where the radial distance goes beyond about 12 m -- should then be left with only realistic speeds
# 
# # remove sequences with radial distance > 12:
# 
# ggplot(india_mov_data, aes(x = radius))+
#   geom_density()
# # there are definitely some weirdly large radial distances
# 
# india_mov_data <- india_mov_data[india_mov_data$radius < 12,]
# 
# 
# # now things should look better:
# 
# ggplot(india_mov_data, aes(x = speed, fill = species))+
#   geom_density()
# 
# # there's still one rogue 20m/s
# 
# # get rid of it:
# 
# max(india_mov_data$speed)
# 
# india_mov_data <- india_mov_data[india_mov_data$speed < max(india_mov_data$speed),]
# 
# ggplot(india_mov_data, aes(x = speed, fill = species))+
#   geom_density()
# # - looks ok now
# 
# ## overwrite the india_mov_data.csv for future use:
# write.csv(india_mov_data, 'data/india_mov_data.csv', row.names = FALSE)

