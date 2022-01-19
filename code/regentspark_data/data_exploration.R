######### DATA EXPLORATION ############

require(ggplot2)

# load in data
posdata <- read.csv('data/posdat.csv')
movdata <- read.csv('data/movdat.csv')


# relationship between speed and the number of images in the sequence

p1 <- ggplot(movdata, aes(x = n, y = speed))+
  geom_point()
p1
# --> higher speeds associated with fewer images in the sequence


# relationship between speed and distance

p2 <- ggplot(movdata, aes(x = dist, y = speed))+
  geom_point()
p2
# --> weird pattern
# --> but generally higher speeds associated with greater distances travelled


# relationship between speed and location

p3 <- ggplot(movdata, aes(x = location, y = speed))+
  geom_point()
p3


# how does tortuosity relate to speed?

# work out tortuosity for each path: define it as gross distance divided by net distance travelled (seems to be how it's usually done in the literature)


# net distance = distance in movdata


# gross distance:

# - measure the straight-line distances between successive positions in a sequence then add them up
#--> PROBLEM: is it ok to assume that animals are moving in straight lines between positions?

# use cosine rule:









