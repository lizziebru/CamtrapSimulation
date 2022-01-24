######### DATA EXPLORATION ############

# TO DO
# - tortuosity q1
# - speed distribution q1
# - speed distribution q2
# - distance-speed relationship q1
# - distance-speed relationshpi q2

# TO DISCUSS
# - tortuosity stuff: 
#   -- method of working out tortuosity?
#   -- observed trends with speed?
#   -- that one super tortuous fox
# - looking at distribution of speeds to think about biases
#   -- thoughts on how best to determine whether there are biases in these speeds (i.e. compare what we have here to how fast foxes/hedgehogs usually move)


# Set-up ------------------------------------------------------------------



require(ggplot2)
require(ggpubr)
require(dplyr)

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
# certain locations look to be associated with certain speeds



# Tortuosity --------------------------------------------------------------

## 1. how could we use this to improve our simulations?






## 2. investing a potential bias: how does tortuosity relate to speed?

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


tortuosity <- list()
for (i in unique(posdata$sequence)) {
  # subset per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # if there is one row then skip to the next one (there was only one position in this sequence):
  if (nrow(k)==1){
    next
  }
  
  # get net dist:
  n <- movdata[movdata$sequence==i,]$dist
  
  # work out gross dist
  gross_dists <- c()
  for (j in 1:(nrow(k)-1)){
    d <- sqrt((k[j,]$radius)^2 + (k[(j+1),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+1),]$radius)*cos(k[j,]$angle - k[(j+1),]$angle))
    gross_dists <- c(gross_dists, d)
  }
  
  # sum gross distances
  g <- sum(gross_dists)
  
  # work out tortuosity
  t <- g / n
  
  # fill tortuosity list
  tortuosity[[paste0("sequence", i)]] <- t
}
tortuosity

# problem: gross distance = net distance for every one
# ask Marcus about how they work out distance?

# what I think net distance is is actually probably gross distance

# so need to work out net distance:


tortuosity2 <- c()
for (i in unique(posdata$sequence)) {
  # subset per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # if there is one row then skip to the next one (there was only one position in this sequence):
  if (nrow(k)==1){
    next
  }
  
  # get gross dist:
  g <- movdata[movdata$sequence==i,]$dist
  
  # work out net dist = distance between first and last
  x <- nrow(k)
  n <- sqrt((k[1,]$radius)^2 + (k[x,]$radius)^2 - 2*(k[1,]$radius)*(k[(x),]$radius)*cos(k[1,]$angle - k[(x),]$angle))
    
  # work out tortuosity
  t <- g / n
  
  # fill tortuosity list
  tortuosity2 <- c(tortuosity2,t)
}
tortuosity2


# the higher the value of t, the more tortuous
t_df <- as.data.frame(cbind(movdata$speed, tortuosity2))
colnames(t_df) <- c("speed", "tortuosity")

t_plot <- ggplot(t_df, aes(x = tortuosity, y = speed))+
  geom_point()
t_plot

# there's a weird one with super high tortuosity: find which one it is:

# it's the 83rd one

# this should correspond to the 83rd row in movdata:
View(movdata[83,])
# --> fox at location 19, sequence 1331

# have a look at the position data for this fox:
View(posdata[posdata$sequence==1331,])

# it looks like it goes back on itself - this is probably what's causing such high tortuosity

# would be nice to plot its actual movement path
# could just use the x & y pixel positions in the image?

tortuous_fox <- ggplot(posdata[posdata$sequence==1331,], aes(x = x, y = y))+
  geom_point()+
  geom_text(aes(label = markerid), hjust = 0.5, vjust = -0.5)
tortuous_fox



# either way: looks like that fox has a pretty tortuous path


# test for relationship between speed and tortuosity


# data are not normal

ggdensity(t_df$speed,
          xlab = 'speed')

ggqqplot(t_df$speed)

## - deviates quite a lot from the normal distribution

shapiro.test(t_df$speed)
# - pretty significantly not normally distributed


# could use Spearman's correlation coefficient: doesn't assume normality
# assumptions: data must be at least ordinal and the scores on one variable must be monotonically related to the other variable

cor.test(t_df$tortuosity, t_df$speed, method = c("spearman"))
#--> significant correlation between them

# plot:

ggscatter(t_df, x = "tortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Tortuosity", ylab = "Speed")



## RE-DO ALL THIS WITHOUT THE OUTLIER FOX

t_df2 <- t_df[-83,]

# could use Spearman's correlation coefficient: doesn't assume normality
# assumptions: data must be at least ordinal and the scores on one variable must be monotonically related to the other variable

cor.test(t_df2$tortuosity, t_df2$speed, method = c("spearman"))
#--> still significant

# plot:

ggscatter(t_df2, x = "tortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Tortuosity", ylab = "Speed")



###--> but: because tortuosities are super clustered at low values, hard to tell whether there is really a correlation
# (i.e. non-similarity of tortuosity values is skewing the relationship)



# Speed distribution ------------------------------------------------------


## 1. what is the distribution of speeds in these data? How does this compare to usual speeds of movements for these animals? Does this suggest there are any biases in what's captured by CTs?

# look at distribution of speeds to start thinking about one of the biases we want to investigate (that speed is potentially proportional to trap rate)

ggdensity(t_df$speed,
          xlab = 'speed')
# tails off at high speeds - might well be missed by CTs then?

# would be good to get a better idea of how well these speeds reflect usual speeds of foxes & hedgehogs in these habitats 
#--> BUT: do we have enough data available for this?

#--> do we have GPS data that we could compare this to? - i.e. compare distribution of speeds measured by CTs vs GPS data


## 2. can we estimate an upper limit on speeds which can be detected by CTs? Then compare this to usual max speeds of these animals to determine whether high speeds could indeed be being missed by CTs

# thinking about the bias of very high speeds being likely to be missed by cameras:
# have a look at speeds associated with movements for which have only 2 or 3 positions

# this could maybe give us an idea of the upper limit of speeds which can be captured

# subset into speeds with 2 or 3 positions

high_speeds_df <- data.frame(stringsAsFactors = F)

for (i in unique(posdata$sequence)) {
  # subset per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # if there are fewer than 2 or more than 3 rows, skip to the next one
  if (nrow(k) < 2){
    next
  }
  if (nrow(k) > 3){
    next
  }
  
  # fill the empty dataframe with whatever makes it past the 2 if statements:
  
  high_speeds_df <- rbind(high_speeds_df, k)
  
}


# look at the speeds corresponding to these positions:

high_speed_seqs <- unique(high_speeds_df$sequence)

# get the speeds for these sequences from movdata:
high_speeds <- high_speeds_df %>% 
  filter(sequence == high_speed_seqs)
  # need to select rows for which sequence is in high_speed_seqs then extract the speed value in that row
  # use x %in% y maybe??










# Distance-speed relationship ---------------------------------------------

# potential bias: effective detection distance is related weakly negatively to speed (i.e. if moving faster less likely to get detected at greater distance)
# found by Rowcliffe et al. 2011 when using the random encounter model to estimate abundance from CT data


## 1. is effective detection distance related to speed in these data?





## 2. how could we incorporate this into the simulation to investigate this potential bias?
