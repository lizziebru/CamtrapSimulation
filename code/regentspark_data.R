### REGENTS' PARK DATA ###

require(ggplot2)

regentspark_pos_data <- read.csv('data/regentspark_pos_data.csv')
regentspark_mov_data <- read.csv('data/regentspark_mov_data.csv')

ggplot(regentspark_mov_data, aes(x = speed, colour = species))+
  geom_density()


ggplot(regentspark_mov_data, aes(x = n))+ # lengths of observed speed sequences
  geom_density()

max(regentspark_mov_data$n)
mean(regentspark_mov_data$n)
min(regentspark_mov_data$n)
