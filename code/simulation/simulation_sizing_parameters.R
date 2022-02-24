## choosing sizing parameters for the simulation based on Regent's Park & Panama data ##

require(ggplot2)

panama_06 <- read.table("data/rse217-sup-0006-speeddatapublished.txt")

posdata <- read.csv("data/posdat.csv")



# Radii and angles --------------------------------------------------------

## distribution of radii and angles:

ggplot(posdata, aes(x = radius))+
  geom_density()+
  ggtitle("Regent's Park")

# don't have radii and angles in Panama data

# --> looks like radius of 10m would be good (it's the max in the regent's park data)


ggplot(posdata, aes(x = angle))+
  geom_density()

min(posdata$angle)
max(posdata$angle)

# --> looks like angle of around 0.82 x 2 (= 1.64) would be good





