### REGENTS' PARK DATA ###

regentspark_pos_data <- read.csv('data/regentspark_pos_data.csv')
regentspark_mov_data <- read.csv('data/regentspark_mov_data.csv')

ggplot(regentspark_mov_data, aes(x = speed, colour = species))+
  geom_density()
