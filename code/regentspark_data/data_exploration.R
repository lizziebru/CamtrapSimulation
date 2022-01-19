######### DATA EXPLORATION ############

# TO DO: 
# - fix tortuosity loop



# Set-up ------------------------------------------------------------------



require(ggplot2)

## load in and format data

posdata <- read.csv('data/posdat.csv')
movdata <- read.csv('data/movdat.csv')


movdata$sequence <- as.numeric(movdata$sequence)
posdata$sequence <- as.numeric(posdata$sequence)





# Speed-variable relationships --------------------------------------------



## relationship between speed and the number of images in the sequence

p1 <- ggplot(movdata, aes(x = n, y = speed))+
  geom_point()
p1
# --> higher speeds associated with fewer images in the sequence


## relationship between speed and distance

p2 <- ggplot(movdata, aes(x = dist, y = speed))+
  geom_point()
p2
# --> weird pattern
# --> but generally higher speeds associated with greater distances travelled


## relationship between speed and location

p3 <- ggplot(movdata, aes(x = location, y = speed))+
  geom_point()
p3




# Tortuosity --------------------------------------------------------------




## how does tortuosity relate to speed?

# work out tortuosity for each path: define it as gross distance divided by net distance travelled (seems to be how it's usually done in the literature)


# net distance = distance in movdata

net_dist <- movdata[movdata$sequence==33,]$dist

# gross distance:

# - measure the straight-line distances between successive positions in a sequence then add them up
#--> PROBLEM: is it ok to assume that animals are moving in straight lines between positions?

# use cosine rule:

# distance between point A and B when:
# A is r1 distance away from CT at angle theta1
# B is r2 distance away from CT at angle theta2
# gross distance = sqrt(r1^2 + r2^2 - 2r1r2cos(theta1 - theta2))

# so for example: Fox's first 2 positions at the start of seq33:

r1 <- 2.3791630
r2 <- 2.3187103
theta1 <- 0.001537844
theta2 <- -0.102266613

gross_dist <- sqrt(r1^2 + r2^2 - 2*r1*r2*cos(theta1 - theta2))


# tortuosity = gross dist divided by net dist:
t <- gross_dist / net_dist
t

# loop:

# subset per sequence number
t_all <- list(for (i in unique(posdata$sequence)) {
  k <- posdata[posdata$sequence==i,]
  if (nrow(k)==1){
    next
  }
  n <- movdata[movdata$sequence==i,]$dist
  r1 <- k[1,]$radius
  theta1 <- k[1,]$angle
  r2 <- k[2,]$radius
  theta2 <- k[2,]$angle
  g <- sqrt(r1^2 + r2^2 - 2*r1*r2*cos(theta1 - theta2))
  t <- g / n
})

ta <- for (i in 3){
  posdata[posdata$sequence==i,]
}

x <- for (i in 3){
  2 + i
}


k <- posdata[posdata$sequence==33,]
n <- movdata[movdata$sequence==33,]$dist
if (nrow(k)==1) {
  print("hi")
}
else {
  print("there")
}
r1 <- k[1,]$radius
theta1 <- k[1,]$angle
r2 <- k[2,]$radius
theta2 <- k[2,]$angle
g <- sqrt(r1^2 + r2^2 - 2*r1*r2*cos(theta1 - theta2))
g / n





# if there is one row then skip to the next one (there was only one position in this sequence)
# if nrow==1 in k:
# next

# get net dist:
# n <- k$dist 

# work out gross dist:

# r1 <- m[1,]$radius
# theta1 <- m[1,]$angle
# r2 <- m[2,]$radius
# theta2 <- m[2,]$angle

# g <- sqrt(r1^2 + r2^2 - 2*r1*r2*cos(theta1 - theta2))








