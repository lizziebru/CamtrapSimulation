## working out a probability density function for how likely an animal is to trigger a camera at a given distance from it

## to ask:
# - is this even a good idea?
# - what was the radius of the CT used for the regent's park data?
# - do we have more data we could use to make this more robust
# - discuss thoughts on how to include habitat as a factor (is this something to include in simulations etc?)
# - good idea to do it separately for each species?
# - should we also include the likelihood of the position data being accompanied by enough other positions to measure speed? (e.g. the further away you are probably the more likely to only be caught only once by the CT - so can't measure speed)

ggplot(data = posdata, aes(x = radius, colour = species))+
  geom_density()


# probability density estimation: using the data to estimate the general density of probabilities beyond just the sample of data

# looks pretty normal-distribution-based

# estimate the parameters of the distribution from the data

# mean:
mean_f <- mean(posdata[posdata$species=="Fox",]$radius)
mean_h <- mean(posdata[posdata$species=="Hedgehog",]$radius)

# standard deviation:
sd_f <- sd(posdata[posdata$species=="Fox",]$radius)
sd_h <- sd(posdata[posdata$species=="Hedgehog",]$radius)


# maybe do the 2 species together for now then separate out later if needed
mean <- mean(posdata$radius)
sd <- sd(posdata$radius)

# parametric density estimation: normal distribution with mean and sd worked out here


# check if this estimated density is a good fit:

# plot it with the data and compare:

x <- seq(0,10, length = 1000)
y <- dnorm(x, mean = mean, sd = sd, log = F)

ggplot()+
  geom_density(aes(x = posdata$radius))+
  geom_smooth(aes(x = x, y = y))

#--> it's alright but not amazing

# data are a bit skewed - would be maybe be good to transform it before estimating parameters?

# e.g. logging it:
mean2 <- mean(log(posdata$radius))
sd2 <- sd(log(posdata$radius))
y2 <- dnorm(x, mean = mean2, sd = sd2, log = F)


ylognorm <- dlnorm(x, mean = mean, sd = sd, log = F)

ggplot()+
  geom_density(aes(x = log(posdata$radius)))+
  geom_smooth(aes(x = x, y = y2))

# --> much better
# although does this weird drop below zero for some reason - not sure why




# so now that have this estimated probability density (normal with mean = mean2 and sd = sd2), need to find a way to apply it to a probability of being detected at various radii

# see simulation script for the rest...





