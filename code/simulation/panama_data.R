### PANAMA DATA ###

# panama <- read.table("data/rse217-sup-0006-speeddatapublished.txt")
# # add a speed column: 
# # speed = d / (i*t)
# panama$speed <- panama$distance / (panama$intervals * panama$avgduration)
# write.csv(panama, 'data/panama_data.csv')

panama_data <- read.csv('data/panama_data.csv')

## explore the data a bit:

ggplot(panama, aes(x = speed))+
  geom_density()

ggplot(panama, aes(x = speed, colour = species))+
  geom_density()