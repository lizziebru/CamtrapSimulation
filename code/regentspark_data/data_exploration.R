######### DATA EXPLORATION ############

# TO DO
# - tortuosity q1
# - distance-speed relationship q2

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
require(minpack.lm)

## load in and format data

posdata <- read.csv('data/posdat.csv')
movdata <- read.csv('data/movdat.csv')


movdata$sequence <- as.numeric(movdata$sequence)
posdata$sequence <- as.numeric(posdata$sequence)


# Speed-variable relationships --------------------------------------------



## relationship between speed and the number of images in the sequence

ggplot(movdata, aes(x = n, y = speed))+
  geom_point()

# --> higher speeds associated with fewer images in the sequence


## relationship between speed and distance

ggplot(movdata, aes(x = dist, y = speed))+
  geom_point()

# --> weird pattern
# --> but generally higher speeds associated with greater distances travelled - makes sense


## relationship between speed and location

ggplot(movdata, aes(x = location, y = speed))+
  geom_point()

# certain locations look to be associated with certain speeds








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

high_speeds <- movdata[movdata$sequence %in% high_speed_seqs,]
# separate into foxes & hedgehogs:
high_speeds_f <- high_speeds[high_speeds$species=='Fox',]$speed
high_speeds_h <- high_speeds[high_speeds$species=='Hedgehog',]$speed


# comparing this to max speeds they might be attaining:

# do we have any good GPS data/do you have any ideas of how best to compare these speeds to more realistic data?

# hedgehogs: generally reach max speeds of 4mph == around 1.79m/s

# foxes: generally reach max speeds of 50km/h == around 13.89m/s
#--> but then they wouldn't really be going that fast past the CTs necessarily







# Distance-speed relationship ---------------------------------------------

# potential bias: effective detection distance is related weakly negatively to speed (i.e. if moving faster less likely to get detected at greater distance)
# found by Rowcliffe et al. 2011 when using the random encounter model to estimate abundance from CT data


## 1. is effective detection distance related to speed in these data?

# i.e. are indivs which move faster detected at shorter distances than indivs that move more slowly?

ggplot(movdata, aes(x = radius, y = speed))+
  geom_point()

# test for a correlation between radius & speed?:

# check for normality:

ggdensity(movdata$speed,
          xlab = 'speed')

ggqqplot(movdata$speed)

## - deviates quite a lot from the normal distribution

shapiro.test(movdata$speed)
# - pretty significantly not normally distributed


# same for radius:

ggdensity(movdata$radius,
          xlab = 'radius')

ggqqplot(movdata$radius)

## - deviates quite a lot from the normal distribution

shapiro.test(movdata$radius)
# - pretty significantly not normally distributed


# could use Spearman's correlation coefficient:

cor.test(movdata$radius, movdata$speed, method = c("spearman"))
#--> significant correlation between them

# plot:

ggscatter(movdata, x = "radius", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Radius", ylab = "Speed")

# correlation is positive though: higher speeds seem to be correlated with larger distances
# this makes sense intuitively

# harder question is regarding whether really high speeds are missed - discuss how best to tackle this?


## 2. how could we incorporate this into the simulation to investigate this potential bias?
