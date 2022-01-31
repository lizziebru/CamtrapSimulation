## TORTUOSITY AND MOVEMENT SPEED

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




# using gross/net distance ------------------------------------

## investing a potential bias: how does tortuosity relate to speed and how does this compare across different species?

# work out tortuosity for each path: define it as gross distance divided by net distance travelled (seems to be how it's usually done in the literature)

# net distance = distance in movdata

net_dist <- movdata[movdata$sequence==33,]$dist

# gross distance:

# - measure the straight-line distances between successive positions in a sequence then add them up
#--> PROBLEM: is it ok to assume that animals are moving in straight lines between positions? - probably?

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

# taking logs:
t_df5 <- cbind(t_df, logtortuosity, logspeed)
ggscatter(t_df5, x = "logtortuosity", y = "logspeed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "log(Tortuosity)", ylab = "log(Speed)")


## RE-DO ALL THIS WITHOUT THE OUTLIER FOX

t_df2 <- t_df[-83,]

# could use Spearman's correlation coefficient: doesn't assume normality
# assumptions: data must be at least ordinal and the scores on one variable must be monotonically related to the other variable

cor.test(t_df2$tortuosity, t_df2$speed, method = c("spearman"))

cor.test(t_df3$logtortuosity, t_df3$speed, method = c("spearman"))

cor.test(t_df4$logtortuosity, t_df4$logspeed, method = c("spearman"))
#--> still significant

logtortuosity <- log(t_df2$tortuosity)

t_df3 <- cbind(t_df2, logtortuosity)

logspeed <- log(t_df3$speed)

t_df4 <- cbind(t_df3, logspeed)

# plot:

ggscatter(t_df2, x = "tortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Tortuosity", ylab = "Speed")

ggscatter(t_df3, x = "logtortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "log(Tortuosity)", ylab = "Speed")

ggscatter(t_df4, x = "logtortuosity", y = "logspeed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "log(Tortuosity)", ylab = "log(Speed)")

###--> but: because tortuosities are super clustered at low values, hard to tell whether there is really a correlation
# (i.e. non-similarity of tortuosity values is skewing the relationship)

# TO DO: investigate more with logs and try to better understand this complex relationship






### Tortuosity & speed relationship - using turn angles

## investing a potential bias: how does tortuosity relate to speed and how does this compare across different species?


# work out turn angles ----------------------------------------------------

# need minimum of 3 points: 1, 2, 3

# cos(angle) = (b^2 + c^2 - a^2) / 2*b*c
# where: 
# b = dist between point 1 and 2
# c = dist between point 2 and 3
# a = dist between point 1 and 3

turnangles <- c()
seq_no <- c()
for (i in unique(posdata$sequence)) {
  
  # subset per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # if there are fewer than 3 rows then skip to the next one (need 3 positions in the sequence to calculate tortuosity):
  if (nrow(k) < 3){
    next
  }
  
  # vector of sequence numbers that make it past this point:
  seq_no <- c(seq_no, unique(k$sequence))
  
  # select groups of 3 and work out tortuosity for each
  
  x <- c(1:(nrow(k)-2))
  
  angles <- c()
  
  for (j in x) {
    a <- sqrt((k[j,]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+2),]$radius)*cos(k[j,]$angle - k[(j+2),]$angle))
    b <- sqrt((k[j,]$radius)^2 + (k[(j+1),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+1),]$radius)*cos(k[j,]$angle - k[(j+1),]$angle))
    c <- sqrt((k[(j+1),]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[(j+1),]$radius)*(k[(j+2),]$radius)*cos(k[(j+1),]$angle - k[(j+2),]$angle))
    
    angle <- acos((b^2 + c^2 - a^2) / 2*b*c)
    
    angles <- c(angles, angle)
  }
  
  # for each sequence: work out the mean of all the turn angles involved in the sequence
  
  angles_mean <- mean(angles)
  
  turnangles <- c(turnangles, angles_mean)
  
}
seq_no
turnangles
# --> problem: a few NaNs --> probably bc of problems with inverse cos - maybe look into this more though?

# make main df with sequence no, tortuosity, speed, and species


# problem: clearly not all of these sequence numbers appear in movdata - must've had some for which speeds weren't calculated
#--> need to just use the sequences which appear in both

seqs1 <- movdata[movdata$sequence %in% seq_no, ]$sequence

# get tortuosities for just those sequences:
turnangles2 <- c()
for (i in seqs1) {
  
  # subset per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # select groups of 3 and work out tortuosity for each
  
  x <- c(1:(nrow(k)-2))
  
  angles <- c()
  
  for (j in x) {
    a <- sqrt((k[j,]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+2),]$radius)*cos(k[j,]$angle - k[(j+2),]$angle))
    b <- sqrt((k[j,]$radius)^2 + (k[(j+1),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+1),]$radius)*cos(k[j,]$angle - k[(j+1),]$angle))
    c <- sqrt((k[(j+1),]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[(j+1),]$radius)*(k[(j+2),]$radius)*cos(k[(j+1),]$angle - k[(j+2),]$angle))
    
    angle <- acos((b^2 + c^2 - a^2) / 2*b*c)
    
    angles <- c(angles, angle)
  }
  
  # for each sequence: work out the mean of all the turn angles involved in the sequence
  
  angles_mean <- mean(angles)
  
  turnangles2 <- c(turnangles2, angles_mean)
  
}
turnangles2


main_df <- data.frame(sequence = seqs1,
                      tortuosity = turnangles2,
                      speed = movdata[movdata$sequence %in% seq_no, ]$speed,
                      species = as.character(movdata[movdata$sequence %in% seq_no, ]$species)
                      )

# remove rows containing NaNs:
main_df <- na.omit(main_df)








# investigating relationship between speed & tortuosity - using all the data -------------------


# initial plot with separate colours for species:

ggplot(main_df, aes(x = tortuosity, y = speed, colour = species))+
  geom_point()

# with spearman regression lines:

f_s1 <- ggscatter(main_df[main_df$species=="Fox",], x = "tortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Tortuosity (average turn angles)", ylab = "Speed",
          title = "Foxes (spearman)")

h_s1 <- ggscatter(main_df[main_df$species=="Hedgehog",], x = "tortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Tortuosity (average turn angles)", ylab = "Speed",
          title = "Hedgehogs (spearman)")

# quite a few outliers still

# generally +ve relationship between speed & tortuosity

# although the data are pretty clustered around similar tortuosities

# need to investigate this relationship in more detail..


# try again using Pearson's?

f_p <- ggscatter(main_df[main_df$species=="Fox",], x = "tortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tortuosity (average turn angles)", ylab = "Speed",
          title = "Foxes (pearson)")

h_p <- ggscatter(main_df[main_df$species=="Hedgehog",], x = "tortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tortuosity (average turn angles)", ylab = "Speed",
          title = "Hedgehogs (pearson)")

ggarrange(f_s1, h_s1, f_p, h_p, nrow = 2, ncol = 2)

# --> doesn't look that different
# but seems like spearman's is probably better bc seems less affected by the outliers than pearson's is




## try again taking logs:

# add logged columns to df:

main_df <- data.frame(main_df,
                      log_tortuosity = log(main_df$tortuosity),
                      log_speed = log(main_df$speed))



# logging just tortuosity:

f_s2 <- ggscatter(main_df[main_df$species=="Fox",], x = "log_tortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "log Tortuosity (average turn angles)", ylab = "Speed",
          title = "Foxes (spearman)")

h_s2 <- ggscatter(main_df[main_df$species=="Hedgehog",], x = "log_tortuosity", y = "speed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "log Tortuosity (average turn angles)", ylab = "Speed",
          title = "Hedgehogs (spearman)")


# logging just speed:

f_s3 <- ggscatter(main_df[main_df$species=="Fox",], x = "tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Foxes (spearman)")

h_s3 <- ggscatter(main_df[main_df$species=="Hedgehog",], x = "tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Hedgehogs (spearman)")


# logging both:

f_s4 <- ggscatter(main_df[main_df$species=="Fox",], x = "log_tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "log Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Foxes (spearman)")

h_s4 <- ggscatter(main_df[main_df$species=="Hedgehog",], x = "log_tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "log Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Hedgehogs (spearman)")


ggarrange(f_s1, h_s1, f_s2, h_s2, f_s3, h_s3, f_s4, h_s4,
          nrow = 4,
          ncol = 2)

#--> logging tortuosity doesn't seem to do much and logging speed makes things worse


# need to better discern this complex-looking relationship between 2 continuous variables


# try fitting some models:


## Linear models: regression:

# 1. explore data to determine whether LM is a good choice

# looking at correlations & what the data look like - LM looks pretty good


# 2. fit Linear Regression Model to data

# initial logging didn't seem to do much - but probs stil worth investigating whether a logistic model could be any good

# fit the linear (regression) model:

f1 <- lm(speed ~ tortuosity, data = fox_df)

summary(f1)

ggplot(fox_df, aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", geom = "smooth")+
  ggtitle("Foxes - linear model")

# diagnostic plots:
par(mfrow = c(2,2), mar = c(5, 5, 1.5, 1.5))
plot(f1)
# normal Q-Q plot not great - quite a lot of deviation at the ends - residuals are not v normally distributed
# scale location plot also not great - the variance of the residuals does change as a function of the predictor
# a couple of points also have rly high leverage

h1 <- lm(speed ~ tortuosity, data = hedgehog_df)

summary(h1)

ggplot(hedgehog_df, aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", geom = "smooth")+
  ggtitle("Hedgehogs - linear model")

# diagnostic plots:
par(mfrow = c(2,2), mar = c(5, 5, 1.5, 1.5))
plot(h1)
# issues with residuals vs fitted: the distribution of residuals has pretty uneven variance
# --> is this something to deal with?
# scale location plot also not great - the variance of the residuals does change as a function of the predictor
# a couple of points also have rly high leverage


# --> problem: all the data are pretty clustered at low tortuosity values

# the diagnostic plots basically show that the slope coefficient estimates are pretty strongly affected by certain data points

# suggests that could be a good idea to get rid of some outliers...


# other models:

# LINEAR MODELS: 

# cubic

f2 <- lm(fox_df$speed ~ poly(fox_df$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(f2)

h2 <- lm(hedgehog_df$speed ~ poly(hedgehog_df$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(h2)


# quadratic

f3 <- lm(fox_df$speed ~ poly(fox_df$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(f3)

h3 <- lm(hedgehog_df$speed ~ poly(hedgehog_df$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(h3)


# NON-LINEAR MODELS

# logistic

logistic_model <- function(t, r_max, K, N_0){ # The classic logistic equation
  return(N_0 * K * exp(r_max * t)/(K + N_0 * (exp(r_max * t) - 1)))
}

N_0_start_f <- min(fox_df$speed) # lowest speed
K_start_f <- max(fox_df$speed) # highest speed
r_max_start_f <- 0.7143 # use estimate from OLS fitting (model f1)

f4 <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), fox_df,
               list(r_max=r_max_start_f, N_0 = N_0_start_f, K = K_start_f))


N_0_start_h <- min(hedgehog_df$speed) # lowest speed
K_start_h <- max(hedgehog_df$speed) # highest speed
r_max_start_h <- 5.7726 # use estimate from OLS fitting (model h1)

h4 <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), hedgehog_df,
               list(r_max=r_max_start_h, N_0 = N_0_start_h, K = K_start_h))


# plotting all the models for each species:

# foxes:

timepoints_f <- seq(0, max(main_df[main_df$species=="Fox",]$tortuosity, na.rm=T), 0.1)
f_logvals <- data.frame(timepoints = timepoints_f,
                        log_vals = log(logistic_model(r_max=coef(f4)[1], N_0 = coef(f4)[2], K = coef(f4)[3], t = timepoints_f)))

f_models <- ggplot(main_df[main_df$species=="Fox", ], aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), se = FALSE, aes(colour = "#CC79A7")) + #add aes colours to tell them apart
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3, raw = TRUE), se = FALSE, aes(colour = "#D55E00")) +
  geom_smooth(method = "loess", data = f_logvals, formula = y ~ x, aes(timepoints, log_vals, colour = "#0072B2")) +
  scale_color_manual(name = NULL, values = c("#CC79A7", "#D55E00", "#0072B2"), labels = c("Quadratic", "Cubic", "Logistic"))+
  guides(col = guide_legend("Model"))+
  ggtitle("Regent's park - foxes")+
  theme_bw()+
  xlim(min(main_df[main_df$species=="Fox",]$tortuosity, na.rm = T), 3)


# hedgehogs:

timepoints_h <- seq(0, max(main_df[main_df$species=="Hedgehog",]$tortuosity, na.rm=T), 0.1)
h_logvals <- data.frame(timepoints = timepoints_h,
                        log_vals = log(logistic_model(r_max=coef(h4)[1], N_0 = coef(h4)[2], K = coef(h4)[3], t = timepoints_h)))

h_models <- ggplot(main_df[main_df$species=="Hedgehog", ], aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), se = FALSE, aes(colour = "#CC79A7")) + #add aes colours to tell them apart
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3, raw = TRUE), se = FALSE, aes(colour = "#D55E00")) +
  geom_smooth(method = "loess", data = h_logvals, formula = y ~ x, aes(timepoints, log_vals, colour = "#0072B2")) +
  scale_color_manual(name = NULL, values = c("#CC79A7", "#D55E00", "#0072B2"), labels = c("Quadratic", "Cubic", "Logistic"))+
  guides(col = guide_legend("Model"))+
  ggtitle("Regent's park - hedgehogs")+
  theme_bw()+
  xlim(min(main_df[main_df$species=="Hedgehog",]$tortuosity, na.rm = T), 1.9)

ggarrange(f_models, h_models, nrow = 2)

# it all looks pretty skewed by outliers...

# logistic looks tentatively the best though









# investigating relationship between speed & tortuosity - without outliers --------


# make new df without 2 fox outliers and 3 hedgehog ones:

# identify which ones they are:
max(main_df[main_df$species=="Fox",]$tortuosity, na.rm = T)

# separate out the fox tortuosities and identify the 2 largest values:

fox_t <- main_df[main_df$species=="Fox",]$tortuosity
remove_f <- tail(unique(fox_t[order(fox_t)]), 2)

hedg_t <- main_df[main_df$species=="Hedgehog",]$tortuosity
remove_h <- tail(unique(hedg_t[order(hedg_t)]), 3)

remove <- c(remove_f, remove_h)

main_df2 <- main_df[!(main_df$tortuosity==remove[1] | main_df$tortuosity==remove[2] | main_df$tortuosity==remove[3] | main_df$tortuosity==remove[4] | main_df$tortuosity==remove[5]),]


# now re-doing everything:

# initial plot with separate colours for species:

ggplot(main_df2, aes(x = tortuosity, y = speed, colour = species))+
  geom_point()

# with spearman regression lines:

f_s1_2 <- ggscatter(main_df2[main_df2$species=="Fox",], x = "tortuosity", y = "speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                  title = "Foxes (spearman)")

h_s1_2 <- ggscatter(main_df2[main_df2$species=="Hedgehog",], x = "tortuosity", y = "speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                  title = "Hedgehogs (spearman)")


# try again using Pearson's?

f_p_2 <- ggscatter(main_df2[main_df2$species=="Fox",], x = "tortuosity", y = "speed", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                 title = "Foxes (pearson)")

h_p_2 <- ggscatter(main_df2[main_df2$species=="Hedgehog",], x = "tortuosity", y = "speed", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Tortuosity (average turn angles)", ylab = "Speed",
                 title = "Hedgehogs (pearson)")

ggarrange(f_s1_2, h_s1_2, f_p_2, h_p_2, nrow = 2, ncol = 2)

# very similar: probably use spearman's though bc data are not normal




## taking logs:

# logging just tortuosity:

f_s2_2 <- ggscatter(main_df2[main_df2$species=="Fox",], x = "log_tortuosity", y = "speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "log Tortuosity (average turn angles)", ylab = "Speed",
                  title = "Foxes (spearman)")

h_s2_2 <- ggscatter(main_df2[main_df2$species=="Hedgehog",], x = "log_tortuosity", y = "speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "log Tortuosity (average turn angles)", ylab = "Speed",
                  title = "Hedgehogs (spearman)")


# logging just speed:

f_s3_2 <- ggscatter(main_df2[main_df2$species=="Fox",], x = "tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Foxes (spearman)")

h_s3_2 <- ggscatter(main_df2[main_df2$species=="Hedgehog",], x = "tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Hedgehogs (spearman)")


# logging both:

f_s4_2 <- ggscatter(main_df2[main_df2$species=="Fox",], x = "log_tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "log Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Foxes (spearman)")

h_s4_2 <- ggscatter(main_df2[main_df2$species=="Hedgehog",], x = "log_tortuosity", y = "log_speed", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "log Tortuosity (average turn angles)", ylab = "log Speed",
                  title = "Hedgehogs (spearman)")


ggarrange(f_s1_2, h_s1_2, f_s2_2, h_s2_2, f_s3_2, h_s3_2, f_s4_2, h_s4_2,
          nrow = 4,
          ncol = 2)

#--> logging tortuosity doesn't seem to do much but logging speed does do something - looks like an asymptotal relatioship..




### try fitting some models:

# linear regression model looked pretty crap before so don't do it again here


# other models:

# LINEAR MODELS: 

# cubic

f2_2 <- lm(main_df2[main_df2$species=="Fox",]$speed ~ poly(main_df2[main_df2$species=="Fox",]$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(f2_2)

h2_2 <- lm(main_df2[main_df2$species=="Hedgehog",]$speed ~ poly(main_df2[main_df2$species=="Hedgehog",]$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(h2_2)


# quadratic

f3_2 <- lm(main_df2[main_df2$species=="Fox",]$speed ~ poly(main_df2[main_df2$species=="Fox",]$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(f3_2)

h3_2 <- lm(main_df2[main_df2$species=="Hedgehog",]$speed ~ poly(main_df2[main_df2$species=="Hedgehog",]$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(h3_2)


# NON-LINEAR MODELS

# logistic

logistic_model <- function(t, r_max, K, N_0){ # The classic logistic equation
  return(N_0 * K * exp(r_max * t)/(K + N_0 * (exp(r_max * t) - 1)))
}

N_0_start_f_2 <- min(main_df2[main_df2$species=="Fox",]$speed) # lowest speed
K_start_f_2 <- max(main_df2[main_df2$species=="Fox",]$speed) # highest speed
r_max_start_f_2 <- 2.4577  # use estimate from OLS fitting:
f1_2 <- lm(speed ~ tortuosity, data = main_df2[main_df2$species=="Fox",])

f4_2 <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), main_df2[main_df2$species=="Fox",],
            list(r_max=r_max_start_f_2, N_0 = N_0_start_f_2, K = K_start_f_2))


N_0_start_h_2 <- min(main_df2[main_df2$species=="Hedgehog",]$speed) # lowest speed
K_start_h_2 <- max(main_df2[main_df2$species=="Hedgehog",]$speed) # highest speed
r_max_start_h_2 <- 14.139 # use estimate from OLS fitting (model h1_2):
h1_2 <- lm(speed ~ tortuosity, data = main_df2[main_df2$species=="Hedgehog",])

h4 <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), main_df2[main_df2$species=="Hedgehog",],
            list(r_max=r_max_start_h_2, N_0 = N_0_start_h_2, K = K_start_h_2))
# starting values aren't good enough for a model to converge - need to find better ones


# plotting all the models for each species:

# foxes:

timepoints_f_2 <- seq(0, max(main_df2[main_df2$species=="Fox",]$tortuosity, na.rm=T), 0.1)
f_logvals_2 <- data.frame(timepoints = timepoints_f,
                        log_vals = log(logistic_model(r_max=coef(f4_2)[1], N_0 = coef(f4_2)[2], K = coef(f4_2)[3], t = timepoints_f)))

f_models_2 <- ggplot(main_df2[main_df2$species=="Fox", ], aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), se = FALSE, aes(colour = "#CC79A7")) + #add aes colours to tell them apart
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3, raw = TRUE), se = FALSE, aes(colour = "#D55E00")) +
  geom_smooth(method = "loess", data = f_logvals_2, formula = y ~ x, aes(timepoints, log_vals, colour = "#0072B2")) +
  scale_color_manual(name = NULL, values = c("#CC79A7", "#D55E00", "#0072B2"), labels = c("Quadratic", "Cubic", "Logistic"))+
  guides(col = guide_legend("Model"))+
  ggtitle("Regent's park - foxes")+
  theme_bw()+
  xlim(min(main_df2[main_df2$species=="Fox",]$tortuosity, na.rm = T), 3)
f_models

# logistic looks pretty promising!






# TO DO

# for foxes:
# - scrap quadratic
# - keep cubic & logistic
# - find another model that might better describe the asymptote


# hedgehogs:
# --> haven't done this yet cause haven't been able to fit the logistic model 
# - if this stuff is useful then invest more time in it and try to fit logistic & other models



# also maybe try more transformations?







# considering the no. of images per sequence ----------------------------------

### look at the distribution of no. of images per sequence

# add column about this to main_df2:

# for every sequence no. in main_df2: count the number of rows with that sequence value in posdata:

images <- c()
for (i in unique(main_df2$sequence)) {
  
  # subset posdata per sequence number:
  k <- posdata[posdata$sequence==i,] 
  
  # count the number of rows and fill images with it
  r <- nrow(k)
  
  images <- c(images, r)
  
}

main_df2 <- cbind(main_df2, images)

ggdensity(main_df2$images,
          xlab = "Number of images per sequence")

# separate out by species:

f_imag <- ggdensity(main_df2[main_df2$species=="Fox",]$images,
                    xlab = "Number of images per sequence",
                    title = "Foxes")

h_imag <- ggdensity(main_df2[main_df2$species=="Hedgehog",]$images,
                    xlab = "Number of images per sequence",
                    title = "Hedgehog")

ggarrange(f_imag, h_imag, nrow = 2)
# there are more foxes moving faster (i.e. that have small no. of images per sequence) than hedgehogs
# both peak at around 10 images per sequence




# Could think about no. of points - do analysis which includes the no. of points in the sequence to address those with low speeds & low tortuosity
# --> not sure about what Chris means by this exactly - discuss










# extra bits of code not currently using ----------------------------------

# separate out into species:

posdata_fox <- posdata[posdata$species=="Fox",]

posdata_hedgehog <- posdata[posdata$species=="Hedgehog",]


# re-do tortuosity stuff for each species:

turnangles_f <- c()
seq_no_f <- c()
for (i in unique(posdata_fox$sequence)) {
  
  # subset per sequence number:
  k <- posdata_fox[posdata_fox$sequence==i,] 
  
  # if there are fewer than 3 rows then skip to the next one (need 3 positions in the sequence to calculate tortuosity):
  if (nrow(k) < 3){
    next
  }
  
  # vector of sequence numbers that make it past this point:
  seq_no_f <- c(seq_no_f, unique(k$sequence))
  
  # select groups of 3 and work out mean tortuosity for each
  
  x <- c(1:(nrow(k)-2))
  
  angles <- c()
  
  for (j in x) {
    a <- sqrt((k[j,]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+2),]$radius)*cos(k[j,]$angle - k[(j+2),]$angle))
    b <- sqrt((k[j,]$radius)^2 + (k[(j+1),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+1),]$radius)*cos(k[j,]$angle - k[(j+1),]$angle))
    c <- sqrt((k[(j+1),]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[(j+1),]$radius)*(k[(j+2),]$radius)*cos(k[(j+1),]$angle - k[(j+2),]$angle))
    
    angle <- acos((b^2 + c^2 - a^2) / 2*b*c)
    
    angles <- c(angles, angle)
  }
  
  angles_mean <- mean(angles)
  
  turnangles_f <- c(turnangles_f, angles_mean)
  
}

tortuosity_fox <- cbind(seq_no_f, turnangles_f)

##  looking at relationship between speed & tortuosity using turnangles as a measure of tortuosity:

fox_df <- as.data.frame(cbind(movdata[movdata$sequence %in% seq_no_f, ]$speed, tortuosity_fox))

colnames(fox_df) <- c("speed", "sequence", "tortuosity")

f_plot <- ggscatter(fox_df, 
                    x = "tortuosity", 
                    y = "speed", 
                    add = "reg.line", 
                    conf.int = TRUE, 
                    cor.coef = TRUE, 
                    cor.method = "spearman",
                    xlab = "Tortuosity (average turn angles)", 
                    ylab = "Speed",
                    title = "Regent's park: foxes")


turnangles_h <- c()
seq_no_h <- c()
for (i in unique(posdata_hedgehog$sequence)) {
  
  # subset per sequence number:
  k <- posdata_hedgehog[posdata_hedgehog$sequence==i,] 
  
  # if there are fewer than 3 rows then skip to the next one (need 3 positions in the sequence to calculate tortuosity):
  if (nrow(k) < 3){
    next
  }
  
  # vector of sequence numbers that make it past this point:
  seq_no_h <- c(seq_no_h, unique(k$sequence))
  
  # select groups of 3 and work out mean tortuosity for each
  
  x <- c(1:(nrow(k)-2))
  
  angles <- c()
  
  for (j in x) {
    a <- sqrt((k[j,]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+2),]$radius)*cos(k[j,]$angle - k[(j+2),]$angle))
    b <- sqrt((k[j,]$radius)^2 + (k[(j+1),]$radius)^2 - 2*(k[j,]$radius)*(k[(j+1),]$radius)*cos(k[j,]$angle - k[(j+1),]$angle))
    c <- sqrt((k[(j+1),]$radius)^2 + (k[(j+2),]$radius)^2 - 2*(k[(j+1),]$radius)*(k[(j+2),]$radius)*cos(k[(j+1),]$angle - k[(j+2),]$angle))
    
    angle <- acos((b^2 + c^2 - a^2) / 2*b*c)
    
    angles <- c(angles, angle)
  }
  
  angles_mean <- mean(angles)
  
  turnangles_h <- c(turnangles_h, angles_mean)
  
}

tortuosity_hedgehog <- cbind(seq_no_h, turnangles_h)

##  looking at relationship between speed & tortuosity using turnangles as a measure of tortuosity:

hedgehog_df <- as.data.frame(cbind(movdata[movdata$sequence %in% seq_no_h, ]$speed, tortuosity_hedgehog))

colnames(hedgehog_df) <- c("speed", "sequence", "tortuosity")

h_plot <- ggscatter(hedgehog_df, 
                    x = "tortuosity", 
                    y = "speed", 
                    add = "reg.line", 
                    conf.int = TRUE, 
                    cor.coef = TRUE, 
                    cor.method = "spearman",
                    xlab = "Tortuosity (average turn angles)", 
                    ylab = "Speed",
                    title = "Regent's park: hedgehogs")

# make multi-panel plot for the different species:

ggarrange(f_plot, h_plot, nrow = 2)


# try again using Pearson's?

f_plot5 <- ggscatter(fox_df, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "pearson",
                     xlab = "Tortuosity (average turn angles)", 
                     ylab = "Speed",
                     title = "Regent's park: foxes (Pearson's)")

h_plot5 <- ggscatter(hedgehog_df, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "pearson",
                     xlab = "Tortuosity (average turn angles)", 
                     ylab = "Speed",
                     title = "Regent's park: hedgehogs (Pearson's)")


ggarrange(f_plot5, h_plot5, nrow = 2)

# --> doesn't look that different
# but seems like spearman's is probably better bc seems less affected by the outliers than pearson's is





## try again taking logs:

# logging tortuosity:

fox_df2 <- fox_df

fox_df2$tortuosity <- log(fox_df2$tortuosity)

f_plot2 <- ggscatter(fox_df2, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "log(tortuosity) (average turn angles)", 
                     ylab = "Speed",
                     title = "Regent's park: foxes")



hedgehog_df2 <- hedgehog_df

hedgehog_df2$tortuosity <- log(hedgehog_df2$tortuosity)

h_plot2 <- ggscatter(hedgehog_df2, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "log(tortuosity) (average turn angles)", 
                     ylab = "Speed",
                     title = "Regent's park: hedgehogs")

ggarrange(f_plot2, h_plot2, nrow = 2)


# logging speed:

fox_df3 <- fox_df

fox_df3$speed <- log(fox_df3$speed)

f_plot3 <- ggscatter(fox_df3, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "Tortuosity (average turn angles)", 
                     ylab = "log(speed)",
                     title = "Regent's park: foxes")



hedgehog_df3 <- hedgehog_df

hedgehog_df3$speed <- log(hedgehog_df3$speed)

h_plot3 <- ggscatter(hedgehog_df3, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "Tortuosity (average turn angles)", 
                     ylab = "log(speed)",
                     title = "Regent's park: hedgehogs")

ggarrange(f_plot3, h_plot3, nrow = 2)




# logging both:

fox_df4 <- fox_df

fox_df4$speed <- log(fox_df4$speed)

fox_df4$tortuosity <- log(fox_df4$tortuosity)

f_plot4 <- ggscatter(fox_df4, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "log(tortuosity) (average turn angles)", 
                     ylab = "log(speed)",
                     title = "Regent's park: foxes")



hedgehog_df4 <- hedgehog_df

hedgehog_df4$speed <- log(hedgehog_df4$speed)

hedgehog_df4$tortuosity <- log(hedgehog_df4$tortuosity)

h_plot4 <- ggscatter(hedgehog_df4, 
                     x = "tortuosity", 
                     y = "speed", 
                     add = "reg.line", 
                     conf.int = TRUE, 
                     cor.coef = TRUE, 
                     cor.method = "spearman",
                     xlab = "log(tortuosity) (average turn angles)", 
                     ylab = "log(speed)",
                     title = "Regent's park: hedgehogs")

ggarrange(f_plot4, h_plot4, nrow = 2)


# need to better discern this complex-looking relationship between 2 continuous variables


## Linear models: regression:

# 1. explore data to determine whether LM is a good choice

# looking at correlations & what the data look like - LM looks pretty good


# 2. fit Linear Regression Model to data

# initial logging didn't seem to do much - but probs stil worth investigating whether a log linear model could be any good

# fit the linear (regression) model:

f1 <- lm(speed ~ tortuosity, data = fox_df)
summary(f1)
ggplot(fox_df, aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", geom = "smooth")+
  ggtitle("Foxes - linear model")
par(mfrow = c(2,2), mar = c(5, 5, 1.5, 1.5))
plot(f1)
# normal Q-Q plot not great - quite a lot of deviation at the ends - residuals are not v normally distributed
# scale location plot also not great - the variance of the residuals does change as a function of the predictor
# a couple of points also have rly high leverage

h1 <- lm(speed ~ tortuosity, data = hedgehog_df)
summary(h1)
ggplot(hedgehog_df, aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", geom = "smooth")+
  ggtitle("Hedgehogs - linear model")
par(mfrow = c(2,2), mar = c(5, 5, 1.5, 1.5))
plot(h1)
# issues with residuals vs fitted: the distribution of residuals has pretty uneven variance
# --> is this something to deal with?
# scale location plot also not great - the variance of the residuals does change as a function of the predictor
# a couple of points also have rly high leverage


# --> problem: all the data are pretty clustered at low tortuosity values

# the diagnostic plots basically show that the slope coefficient estimates are pretty strongly affected by certain data points

# other models could do:

# LINEAR MODELS 

# cubic

f2 <- lm(fox_df$speed ~ poly(fox_df$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(f2)

h2 <- lm(hedgehog_df$speed ~ poly(hedgehog_df$tortuosity, 3, raw = TRUE), silent = TRUE)
summary(h2)


# quadratic

f3 <- lm(fox_df$speed ~ poly(fox_df$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(f3)

h3 <- lm(hedgehog_df$speed ~ poly(hedgehog_df$tortuosity, 2, raw = TRUE), silent = TRUE)
summary(h3)


# NON-LINEAR MODELS

# logistic

logistic_model <- function(t, r_max, K, N_0){ # The classic logistic equation
  return(N_0 * K * exp(r_max * t)/(K + N_0 * (exp(r_max * t) - 1)))
}

N_0_start_f <- min(fox_df$speed) # lowest speed
K_start_f <- max(fox_df$speed) # highest speed
r_max_start_f <- 0.7143 # use estimate from OLS fitting (model f1)

f_log <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), fox_df,
               list(r_max=r_max_start_f, N_0 = N_0_start_f, K = K_start_f))


N_0_start_h <- min(hedgehog_df$speed) # lowest speed
K_start_h <- max(hedgehog_df$speed) # highest speed
r_max_start_h <- 5.7726 # use estimate from OLS fitting (model h1)

h_log <- nlsLM(speed ~ logistic_model(t = tortuosity, r_max, K, N_0), hedgehog_df,
               list(r_max=r_max_start_h, N_0 = N_0_start_h, K = K_start_h))


# plotting all the models for each species:

# foxes:

timepoints <- seq(0, max(fox_df$tortuosity, na.rm=T), 0.1)
f_logvals <- data.frame(timepoints = timepoints,
                        log_vals = log(logistic_model(r_max=coef(f_log)[1], N_0 = coef(f_log)[2], K = coef(f_log)[3], t = timepoints)))

f_models <- ggplot(fox_df, aes(x = tortuosity, y = speed))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), se = FALSE, aes(colour = "#CC79A7")) + #add aes colours to tell them apart
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3, raw = TRUE), se = FALSE, aes(colour = "#D55E00")) +
  geom_smooth(method = "loess", data = f_logvals, formula = y ~ x, aes(timepoints, log_vals, colour = "#0072B2")) +
  scale_color_manual(name = NULL, values = c("#CC79A7", "#D55E00", "#0072B2"), labels = c("Quadratic", "Cubic", "Logistic"))+
  guides(col = guide_legend("Model"))+
  ggtitle("Regent's park - foxes")+
  theme_bw()+
  xlim(min(fox_df$tortuosity, na.rm = T), 3)
f_models

# it all still looks pretty skewed by outliers...

# would potentially be good to get rid of some: could get rid of the 2 with super high tortuosity potentially

# make new fox df without those 2 outliers:



# 3. determine whether the model fits adequately the data



