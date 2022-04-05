
require(circular)

## rautonorm
# this deffo gives you random autocorrelated numbers which are good to use - doesn't need improvement
# generates a set of autocorrelated random normal variates - corresponding to different steps
# wanna simulate steps of an animal with random variation in speed throughout - but this speed variation isn't totally random hence the autocorrelation
# INPUTS:
# n: number of variates to generate
# mean, sd: mean and standard deviation of the normal distribution
# r: the autocorrelation coefficient (between 0 and 1)

rautonorm <- function(n,mean=0,sd=1,r){
  ranfunc <- function(i,z,r) sqrt(1-r^2) * sum(z[2:(i+1)]*r^(i-(1:i))) + z[1]*r^i # for the autocorrelation
  # this is a known eqn that someone's worked out somewhere - he's lost the reference now though
  z <- rnorm(n)
  mean + sd*c(z[1], sapply(1:(n-1), ranfunc, z, r))
  }



## pathgen

## generates a path of x, y positions using a correlated random walk

# INPUTS:
# n: number of steps
# pTurn: probability of turning at each step
# kTurn: mean vonMises concentration parameter (kappa) for turn angle (higher = more concentrated) -- just like SD for normal distribution: how concentrated it is about the mean
# logspeed: mean log speed
# speedSD: standard deviation of log speed
# speedCor: autocorrelation in speed
# kCor: whether to correlate kappa with speed
# xlim, ylim: x and y axis limits within which to pick the starting point
# wrap: whether to wrap the path

# OUTPUT:
# A list with elements:
# path: a dataframe with columns x and y (path co-ordinates) and, if wrap=TRUE, breaks indicating where wrap breaks occur
# turn, absturn: radian (absolute) turn angles for each step (turn ranging 0 to 2pi; absturn ranging 0 to pi)
# speed: step speeds

pathgen <- function(n, kTurn=0, logspeed=0, speedSD=0, speedCor=0, kCor=TRUE, pTurn=1, xlim=c(0,0), ylim=xlim, wrap=FALSE){
  spds <- exp(rautonorm(n, logspeed, speedSD, speedCor)) # generates set of autocorrelated variates
  # exp bc: the speed chunks we see tend to be log normally distributed
  # so you're generating a normal distribution of variates on the log scale (using logspeed)
  # so take exp to get them back to linear scale
  tTurn <- rbinom(n,1,pTurn) # generates set of n (= no of steps) numbers either 1 and 0 where higher probability of turning at each step = more likely to have 1
  if(kCor==TRUE){ # if we want to correlate kappa with speed:
    kappas <- kTurn * spds / mean(spds)
    deviates <- sapply(kappas, function(x) as.numeric(rvonmises(1,circular(0),x)))
  } 
  else 
      deviates <- as.numeric(rvonmises(n, circular(0), kTurn)) # get one turning number per speed - must be some sort of turning number corresponding to each speed so that speed change and turning are correlated
  deviates[tTurn==0] <- 0 # wherever you shouldn't turn at all, set deviate to 0 so that you don't turn
  angles <- runif(1)*2*pi + cumsum(deviates) # transforms deviates into angles corresponding to the amount you turn at each step
  x <- c(0, cumsum(spds*sin(angles))) + runif(1,xlim[1],xlim[2]) # spds is being used as the hypotenuse for each step -- so acts like distance
  y <- c(0, cumsum(spds*cos(angles))) + runif(1,ylim[1],ylim[2])
  absdevs <- deviates
  i <- absdevs>pi
  absdevs[i] <- 2*pi-absdevs[i]
  absdevs <- abs(absdevs)
  res <- list(path=data.frame(x,y), turn=deviates, absturn=absdevs, speed=spds)
  if(wrap) res <- wrap(res, xlim, ylim)
  res
}



## wrap

# Takes a path object created with pathgen and wraps the co-ordinates within given limits
# this means: constrains where the animal goes: it could wander off away from you or it could stay in the same-ish spot
# wrapping is for convenience: so you can define a region you're working within
# but if an animal leaves it can go back in from the other side - it's toroidal in shape
# just a convenient way to keep the animal within a confined arena
# benefit here: can set your detection zone to cover a decent amount of space
# this makes things more computationally small & feasible

# INPUTS:
# pth: a two column array of x,y positions defining the path
# xlim, ylim: the x,y limits within which to wrap the path

# OUTPUT:
# A path object with x,y co-ordinates wrapped and breaks column added indicating wrap breaks

wrap <- function(path, xlim, ylim=xlim){
  pth <- path$path
  n <- nrow(pth)
  brkpnts <- vector()
  repeat{
    xout <- which(pth$x<xlim[1] | pth$x>xlim[2])[1]
    yout <- which(pth$y<ylim[1] | pth$y>ylim[2])[1]
    if(is.na(xout) & is.na(yout)) break else {
      if(!is.na(xout)){
        brkpnts <- c(brkpnts, xout)
        addn <- if(pth$x[xout]<xlim[1]) diff(xlim) else -diff(xlim) 
        pth$x[xout:n] <- pth$x[xout:n]+addn
      }
      if(!is.na(yout)){
        brkpnts <- c(brkpnts,yout)
        addn <- if(pth$y[yout]<ylim[1]) diff(ylim) else -diff(ylim) 
        pth$y[yout:n] <- pth$y[yout:n]+addn
      }
    }
  }
  brkn <- diff(c(1, sort(brkpnts), n+1))
  breaks <- rep(1:length(brkn), brkn)
  path$path <- cbind(pth, breaks)
  path
}


## plot_wrap

# Plots a wrapped path

# INPUTS:
# path: a wrapped path object created by pathgen
# type: l(ine), p(oint) or b(oth)
# add: add to existing plot or create new one
# axisargs, lineargs, pointargs: lists of arguments to control axis lines or point characteristics

plot_wrap <- function(path, type=c("l","p","b"), add=FALSE, axisargs=list(), lineargs=list(), pointargs=list()){
  type <- match.arg(type)
  if(!"xlab" %in% names(axisargs)) axisargs <- c(xlab="", axisargs)
  if(!"ylab" %in% names(axisargs)) axisargs <- c(ylab="", axisargs)
  if(!add) do.call("plot", c(list(path$path[,1:2], type="n"), axisargs, asp=1))
  for(i in unique(path$path$breaks)) {
    j <- path$path$breaks==i
    xy <- subset(path$path, j)
    pargs <- lapply(pointargs, function(x) if(length(x)==nrow(path$path)) x[j] else x)
    if(type %in% c("l", "b")) do.call("lines", c(list(xy$x, xy$y), lineargs))
    if(type %in% c("p", "b")) do.call("points", c(list(xy$x, xy$y), pargs))
  }
}


## is_in_dz
# Defines whether points are within detection zones
# INPUT:
# point: a two column x,y array of point positions
# dzone: four column array of parameters defining a sector-shaped detection zone
#        required column headings:
#           x,y: x,y coordinates of camera
#           r, th: detection zone radius and angle
#           dir: radian direction in which the camera is facing
# OUTPUT
# A logical array defining whether each point (rows) is in each detection zone (columns)
## ORIGINAL FUNCTION:
# is_in_dz <- function(point, dzone){
#   ij <- expand.grid(1:nrow(point), 1:nrow(dzone)) # expanding rows for each point and dzone
#   pt <- point[ij$Var1, ] # looks just like 'points' did - so what was the purpose of these steps?
#   dz <- dzone[ij$Var2, ] # looks different to dzone - so there probably was a purpose to the previous steps
#   dist <- sqrt((pt[, 1]-dz$x)^2 + (pt[, 2]-dz$y)^2) # distance from camera to each point
#   bear <- atan((pt[, 1]-dz$x) / (pt[, 2]-dz$y)) + # bearing from camera to each point (from the horizontal line)
#     ifelse(pt[, 2]<dz$y, # test: is y-coord less than the d-zone y coord?
#            # if yes, bear = pi:
#            pi,
#            # if no:
#            ifelse(pt[, 1]< dz$x, # test: is x-coord less than the d-zone x coord?
#                   # if yes, bear = 2 pi
#                   2*pi,
#                   # if no, bear = 0:
#                   0))
#   beardif <- (bear-dz$dir) %% (2*pi) # abs angle between bear and dzone centre line
#   beardif <- ifelse(beardif>pi,
#                     2*pi-beardif, # if beardif > pi: set beardif to be 2pi - beardif
#                     beardif) # if not: just keep as it was
#   # conditions for it to be in the dz:
#   # beardif is less than half the detection zone angle - but why half the detection angle?
#   # dist is less than the detection zone radius
#   res <- ifelse(beardif < dz$th/2 & dist < dz$r, # this is the line to probably change
#                 TRUE,
#                 FALSE)
#   # return matrix with TRUE or FALSE for each point
#   return(matrix(res, nrow=nrow(point)))
# }




# edits to is_in_dz to include distance as a probability density:


# to test it:
# path <- pathgen(5e3, kTurn=2, kCor=TRUE, pTurn=1,
#                 logspeed=-2, speedSD=1, speedCor=0,
#                 xlim=c(0,10), wrap=TRUE)
# point <- path$path[,1:2]
# 
# dzone <- data.frame(x=5, y=2, r=6, th=1, dir=0)
  
## TAKE 1
## incorporating distance PDF using the regents' park data -- but only did this for distance, not angle yet
# is_in_dz2 <- function(point, dzone){
#   ij <- expand.grid(1:nrow(point), 1:nrow(dzone)) # expanding rows for each point and dzone
#   pt <- point[ij$Var1, ] # looks just like 'points' did - so what was the purpose of these steps?
#   dz <- dzone[ij$Var2, ] # looks different to dzone - so there probably was a purpose to the previous steps
#   dist <- sqrt((pt[, 1]-dz$x)^2 + (pt[, 2]-dz$y)^2) # distance from camera to each point
#   bear <- atan((pt[, 1]-dz$x) / (pt[, 2]-dz$y)) + # bearing from camera to each point (from the horizontal line)
#     ifelse(pt[, 2]<dz$y, # test: is y-coord is less than the d-zone y coord?
#            # if yes, bear = pi:
#            pi,
#            # if no:
#            ifelse(pt[, 1]< dz$x, # test: if x-coord less than the d-zone x coord?
#                   # if yes, bear = 2 pi
#                   2*pi,
#                   # if no, bear = 0:
#                   0))
#   beardif <- (bear-dz$dir) %% (2*pi) # abs angle between bear and dzone centre line
#   beardif <- ifelse(beardif>pi,
#                     2*pi-beardif, # if beardif > pi: set beardif to be 2pi - beardif
#                     beardif) # if not: just keep as it was
#   # conditions for it to be in the dz:
#   # beardif is less than half the detection zone angle
#   # dist is less than the detection zone radius
#   res <- ifelse(beardif < dz$th/2 & dist < dz$r,
#                 TRUE,
#                 FALSE)
#   # make df of distances of each point and whether they're true or false
#   df <- data.frame(res = as.factor(res),
#                    dist = dist)
# 
#   # now for the ones which are true:
# 
#   # reassign them as true based on the probability density estimated from the data (normal distribution with mean = 0.9976297 and sd = 0.5452)
# 
#   dist_prob <- function(x){ # == the probability density function estimated from fox & hedgehog data combined
#     dnorm(x, mean = 0.9976297, sd = 0.5452, log = F)
#   } # --> BUT: probably need to make separate functions for diff spp
# 
#   # select only those which are TRUE
#   for (i in 1:nrow(df)) {
#     d <- df[i,]
#     if (d$res==TRUE) { # ignore if res is FALSE
# 
#       # work out the probability of being detected based on the estimated probability density:
#       prob <- dist_prob(d$dist)
# 
#       # generate either TRUE or FALSE with prob of getting TRUE = prob of being detected and replace this in the main df
#       df[i,]$res <- sample(c(TRUE,FALSE), 1, prob = c(prob, 1-prob))
#     }
#   }
# 
#   # return matrix with TRUE or FALSE for each point
#   return(matrix(as.logical(df$res), nrow=nrow(point)))
# 
# }
# athough maybe need to set a threshold instead? e.g. like if it's at the bottom 30% tail end then only assign TRUE to 50% of them?

# for large species:
is_in_dz_large <- function(point, dzone){
  ij <- expand.grid(1:nrow(point), 1:nrow(dzone)) # expanding rows for each point and dzone
  pt <- point[ij$Var1, ] # looks just like 'points' did - so what was the purpose of these steps?
  dz <- dzone[ij$Var2, ] # looks different to dzone - so there probably was a purpose to the previous steps
  dist <- sqrt((pt[, 1]-dz$x)^2 + (pt[, 2]-dz$y)^2) # distance from camera to each point
  bear <- atan((pt[, 1]-dz$x) / (pt[, 2]-dz$y)) + # bearing from camera to each point (from the horizontal line)
    ifelse(pt[, 2]<dz$y, # test: is y-coord is less than the d-zone y coord?
           # if yes, bear = pi:
           pi,
           # if no:
           ifelse(pt[, 1]< dz$x, # test: if x-coord less than the d-zone x coord?
                  # if yes, bear = 2 pi
                  2*pi,
                  # if no, bear = 0:
                  0))
  beardif <- (bear-dz$dir) %% (2*pi) # abs angle between bear and dzone centre line
  beardif <- ifelse(beardif>pi,
                    2*pi-beardif, # if beardif > pi: set beardif to be 2pi - beardif
                    beardif) # if not: just keep as it was
  # conditions for it to be in the dz:
  # beardif is less than half the detection zone angle
  # dist is less than the detection zone radius
  res <- ifelse(beardif < dz$th/2 & dist < dz$r,
                TRUE,
                FALSE)
  # make df of distances of each point and whether they're true or false
  isindz_df <- data.frame(res = as.factor(res),
                   radius = dist,
                   angle = beardif) 
  isindz_df2 <- isindz_df
  
  # now for the ones which are true: reassign them as true based on probability density
  
  # model for large species' radius: hazard rate with logistic mix
  large_radius <- function(radius){
    (1 - exp(-(3.3509736/radius)^6.3920311))/(1 + exp(0.9969682*(3.3422355 - radius)))
  }
  # model for large species' angle: normal --> get rid of this for now
  # large_angle <- function(angle){
  #   dnorm(angle, mean = 0.01114079, sd = 0.21902793)
  # }
  for (i in 1:nrow(isindz_df2)) {
    d <- isindz_df2[i,]
    if (d$res==TRUE) { # select those which are TRUE
      prob_radius <- large_radius(d$radius) * 2.767429 # probability of being detected based on the estimated probability density for the radius
      if (prob_radius>1){
        prob_radius <- 1
      }
      #prob_angle <- large_angle(d$angle)
      #total_prob <- prob_radius * prob_angle # total probability = multiply both
      isindz_df2[i,]$res <- sample(c(TRUE,FALSE), 1, prob = c(prob_radius, 1-prob_radius)) # generate either TRUE or FALSE with prob of getting TRUE = prob of being detected and replace this in the main df
    }
  }
  isindz_all <- data.frame(indz = isindz_df$res,
                           detected = isindz_df2$res)
  return(isindz_all)
}

# for small species:
is_in_dz_small <- function(point, dzone){
  ij <- expand.grid(1:nrow(point), 1:nrow(dzone)) # expanding rows for each point and dzone
  pt <- point[ij$Var1, ] # looks just like 'points' did - so what was the purpose of these steps?
  dz <- dzone[ij$Var2, ] # looks different to dzone - so there probably was a purpose to the previous steps
  dist <- sqrt((pt[, 1]-dz$x)^2 + (pt[, 2]-dz$y)^2) # distance from camera to each point
  bear <- atan((pt[, 1]-dz$x) / (pt[, 2]-dz$y)) + # bearing from camera to each point (from the horizontal line)
    ifelse(pt[, 2]<dz$y, # test: is y-coord is less than the d-zone y coord?
           # if yes, bear = pi:
           pi,
           # if no:
           ifelse(pt[, 1]< dz$x, # test: if x-coord less than the d-zone x coord?
                  # if yes, bear = 2 pi
                  2*pi,
                  # if no, bear = 0:
                  0))
  beardif <- (bear-dz$dir) %% (2*pi) # abs angle between bear and dzone centre line
  beardif <- ifelse(beardif>pi,
                    2*pi-beardif, # if beardif > pi: set beardif to be 2pi - beardif
                    beardif) # if not: just keep as it was
  # conditions for it to be in the dz:
  # beardif is less than half the detection zone angle
  # dist is less than the detection zone radius
  res <- ifelse(beardif < dz$th/2 & dist < dz$r,
                TRUE,
                FALSE)
  # make df of distances of each point and whether they're true or false
  isindz_df <- data.frame(res = res,
                   radius = dist,
                   angle = beardif)
  isindz_df2 <- isindz_df
  # now for the ones which are true: reassign them as true based on probability density
  # model for small species' radius: hazard rate with logistic mix
  small_radius <- function(radius){
    (1 - exp(-(1.266202/radius)^1.882447))/(1 + exp(2.604066*(1.401516 - radius)))
  }
  # model for small species' angle: normal 
  # small_angle <- function(angle){
  #   dnorm(angle, mean = 0.01114079, sd = 0.21902793)
  # }
  for (i in 1:nrow(isindz_df2)) {
    d <- isindz_df2[i,]
    if (d$res==TRUE) { # select those which are TRUE
      prob_radius <- small_radius(d$radius) * 3.340884 # probability of being detected based on the estimated probability density for the radius
      if (prob_radius>1){
        prob_radius <- 1
      }
      # prob_angle <- small_angle(d$angle)
      # total_prob <- prob_radius * prob_angle # total probability = multiply both
      isindz_df2[i,]$res <- sample(c(TRUE,FALSE), 1, prob = c(prob_radius, 1-prob_radius)) # generate either TRUE or FALSE with prob of getting TRUE = prob of being detected and replace this in the main df
    }
  }
  isindz_all <- data.frame(indz = isindz_df$res,
                           detected = isindz_df2$res)
  return(isindz_all)
}









# plot_dzone
# Convenience function for plotting detection zones (adds to existing plot)
# INPUT:
# dzone: a four column array of parameters as defined above
plot_dzone <- function(dzone, ...){
  for(i in 1:nrow(dzone)){
    sq <- with(dzone[i, ], seq(dir-th/2, dir+th/2, len=50))
    poly <- with(dzone[i, ], cbind(x + c(0, r*sin(sq)), y + c(0, r*cos(sq)))) # set of x and y coords representing the points on perimeter of the dz polygon
    polygon(poly, ...) # draws the dz polygon
  }
}


# simulation happens between pathgen (simulates a path) and sequence data (simulating detection process)

## sequence_data
# 1. Takes dataframes defining a path and a detection zone (as defined above)
# 2. Filters the path points falling within the detection zone
# 3. Assigns each contiguous sequence of points a unique sequence identifier
# 4. calculates the distances between points with sequences
# finds which points on the path are in the detection zone
# and assigns sequence identifiers to each snippet (so there are multiple within one path)
# returns parts of a path that are in the detection zone
# INPUT
# path: a path object
# a detection zone array
# OUTPUT
# A data frame with columns:
# x,y: x,y co-ordinates of sequence points in detection zones
# sequenceID: integer sequence identifier
# distance: distance traveled for each step between points
# original function:
# sequence_data <- function(pth, dzone){
#   pth <- pth$path[, c("x","y")] # format path into df with sequence of x and y
#   isin <- is_in_dz(pth, dzone) # returns TRUE or FALSE for whether each point in the path intersects with the dzone
#   isin <- as.vector(isin$detected)
#   isin[1] <- FALSE
#   pth <- pth[rep(1:nrow(pth), nrow(dzone)), ] # what does this line do??
#   newseq <- tail(isin, -1) > head(isin, -1)
#   seqid <- c(0, cumsum(newseq))[isin]
#   xy <- pth[isin, ]
#   dist <- sqrt(diff(xy$x)^2 + diff(xy$y)^2)
#   newseq <- tail(seqid, -1) > head(seqid, -1)
#   dist[newseq] <- NA
#   dist <- c(NA, dist)
#   data.frame(xy, 
#              sequenceID=seqid, 
#              distance=dist)
# }

# # these work:
# pth <- pathgen(5e3, kTurn=2, kCor=TRUE, pTurn=1,
#                  logspeed=-2, speedSD=1, speedCor=0,
#                  xlim=c(0,10), wrap=TRUE)
# dzone <- data.frame(x=5, y=2, r=6, th=1, dir=0)
# 
# # these don't:
# pth <- pathgen(n=5e3, kTurn=2, kCor=TRUE, pTurn=1,
#                 logspeed=speed_parameter[1], speedSD=0.05, speedCor=0,
#                 xlim=xlim,
#                 ylim = ylim, ##--> this is causing the issue --> no clue why though?
#                 wrap=TRUE)
# dzone <- data.frame(x=10, y=5, r=10, th=1.65, dir=0)
# pth <- pathgen(n=step_no, 
#                 kTurn=2, 
#                 kCor=TRUE, 
#                 pTurn=1, 
#                 logspeed=speed_parameter[1], 
#                 speedSD=0.05, 
#                 speedCor=0, 
#                 xlim=c(0,20),
#                 wrap=TRUE)
# dzone <- data.frame(x=10, y=5, r=10, th=1.65, dir=0) 

#--> also: issue seems to be caused by speeds being too low sometimes so it doesn't cross the dz at all
# --> keep the number of steps pretty high to mitigate against this



# for small species:
sequence_data_small <- function(pth, dzone){
  pth <- pth$path[, c("x","y")] # format path into df with sequence of x and y
  isin_all <- is_in_dz_small(pth, dzone) # returns true or false for whether each position in the path is in the detection zone
  
  # to get xy, seqID, and dist for those that actually do get detected
  isin_detected <- as.vector(isin_all$detected)
  isin_detected[1] <- FALSE
  pth <- pth[rep(1:nrow(pth), nrow(dzone)), ] # what does this line do??
  newseq <- tail(isin_detected, -1) > head(isin_detected, -1)
  seqid <- c(0, cumsum(newseq))[isin_detected]
  xy <- pth[isin_detected, ]
  dist <- sqrt(diff(xy$x)^2 + diff(xy$y)^2)
  newseq <- tail(seqid, -1) > head(seqid, -1)
  dist[newseq] <- NA
  dist <- c(NA, dist)
  df1 <- data.frame(xy, sequenceID = seqid, distance = dist)

  # to get xy, seqID, and dist for all those that fall in dz regardless of getting detected:
  isin_indz <- as.vector(isin_all$indz)
  isin_indz[1] <- FALSE
  pth2 <- pth[rep(1:nrow(pth), nrow(dzone)), ] # what does this line do??
  newseq2 <- tail(isin_indz, -1) > head(isin_indz, -1)
  seqid2 <- c(0, cumsum(newseq2))[isin_indz]
  xy2 <- pth2[isin_indz, ]
  dist2 <- sqrt(diff(xy2$x)^2 + diff(xy2$y)^2)
  newseq2 <- tail(seqid2, -1) > head(seqid2, -1)
  dist2[newseq2] <- NA
  dist2 <- c(NA, dist2)
  df2 <- data.frame(xy2, sequenceID = seqid2, distance = dist2)
  
  df2["detected"] <- NA
  for (i in 1:nrow(df2)){
    d <- df2[i,]
    if (d$x %in% df1$x & d$y %in% df1$y){ # if x and y coords are in df1, detected = TRUE
      df2[i,]$detected <- TRUE
    }
    else{
      df2[i,]$detected <- FALSE
    }
  }
  
  return(df2)
}

# for large species:
sequence_data_large <- function(pth, dzone){
  pth <- pth$path[, c("x","y")] # format path into df with sequence of x and y
  isin_all <- is_in_dz_large(pth, dzone) # returns true or false for whether each position in the path is in the detection zone
  
  # to get xy, seqID, and dist for those that actually do get detected
  isin_detected <- as.vector(isin_all$detected)
  isin_detected[1] <- FALSE
  pth <- pth[rep(1:nrow(pth), nrow(dzone)), ] # what does this line do??
  newseq <- tail(isin_detected, -1) > head(isin_detected, -1)
  seqid <- c(0, cumsum(newseq))[isin_detected]
  xy <- pth[isin_detected, ]
  dist <- sqrt(diff(xy$x)^2 + diff(xy$y)^2)
  newseq <- tail(seqid, -1) > head(seqid, -1)
  dist[newseq] <- NA
  dist <- c(NA, dist)
  df1 <- data.frame(xy, sequenceID = seqid, distance = dist)
  
  # to get xy, seqID, and dist for all those that fall in dz regardless of getting detected:
  isin_indz <- as.vector(isin_all$indz)
  isin_indz[1] <- FALSE
  pth2 <- pth[rep(1:nrow(pth), nrow(dzone)), ] # what does this line do??
  newseq2 <- tail(isin_indz, -1) > head(isin_indz, -1)
  seqid2 <- c(0, cumsum(newseq2))[isin_indz]
  xy2 <- pth2[isin_indz, ]
  dist2 <- sqrt(diff(xy2$x)^2 + diff(xy2$y)^2)
  newseq2 <- tail(seqid2, -1) > head(seqid2, -1)
  dist2[newseq2] <- NA
  dist2 <- c(NA, dist2)
  df2 <- data.frame(xy2, sequenceID = seqid2, distance = dist2)
  
  df2["detected"] <- NA
  for (i in 1:nrow(df2)){
    d <- df2[i,]
    if (d$x %in% df1$x & d$y %in% df1$y){ # if x and y coords are in df1, detected = TRUE
      df2[i,]$detected <- TRUE
    }
    else{
      df2[i,]$detected <- FALSE
    }
  }
  
  return(df2)
}








## calc_speed
# Summarises speeds for a dataframe of position sequences

# INPUT
# dat: a dataframe of position observations created by sequence_data (above)

# OUTPUT
# A dataframe with a row per sequence and columns:
# sequenceID: integer sequence identifier
# distance: total distance travelled during the sequence
# points: number of points in the sequence
# speed: overall sequence speed

calc_speed <- function(dat){
  dist <- with(dat, tapply(distance, sequenceID, sum, na.rm=TRUE))
  points <- with(dat, tapply(distance, sequenceID, length))
  speed <- dist/(points-1)
  data.frame(sequenceID=unique(dat$sequenceID), distance=dist, points=points, speed=speed)
}



# run the functions
# then look inside them
# see if you're happy with how they're working
# then could see ways to modify them - e.g. the choices he gave for the movement path aren't enough
# same goes for the detection process - could find ways to make it more realistic
# also have a look at the data (mirrors what we wanna simulate)
# understand the format so that can make sure the simulation is good - based on realistic patterns in the data


# AIMS
# we wanna simulate where we know the actual speeds
# then be able to detect the biases
# currently: these simulations don't monitor all the potential biases
# e.g. missing high speeds
# it's all about quantifying bias
# comparing estimation outcomes with the truth

