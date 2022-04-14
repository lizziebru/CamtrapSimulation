
require(circular)

# to test things:
# path <- pathgen(5e3, kTurn=2, kCor=TRUE, pTurn=1,
#                 logspeed=-2, speedSD=1, speedCor=0,
#                 xlim=c(0,10), wrap=TRUE)
# point <- path$path[,1:2]
# 
# dz <- data.frame(x=5, y=2, r=6, th=1, dir=0)




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


## is_in_dz - see bottom for original function
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
# for small species:
sequence_data_small <- function(pth, dzone){
  pth <- pth$path[, c("x","y")] # format path into df with sequence of x and y
  isin_all <- is_in_dz_small(pth, dzone) # returns true or false for whether each position in the path is in the detection zone
  
  # to get xy, seqID, and dist for those that actually do get detected
  isin_detected <- as.vector(isin_all$detected)
  isin_detected[1] <- FALSE
  pth <- pth[rep(1:nrow(pth), nrow(dzone)), ]
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



## outside_buffer
# INPUT:
# x1, y1, x2, y2, = coords of two points (each should just be a number)
# dz = four column array of parameters defining a sector-shaped detection zone
#       required column headings: x, y (xy coords of the camera), r (radius), th (angle)
# max_real = max realised speed for this simulation run
# OUTPUT:
# TRUE = one or both of the points lies outside the buffer
# FALSE = both points lie inside the buffer
outside_buffer <- function(x1, y1, x2, y2, dz, max_real){
  counter <- 0
  xc <- dz[1,1] # x coord of the camera
  yc <- dz[1,2] # y coord of the camera
  r <- dz[1,3] # radius of dz
  th <- dz[1,4] # angle of dz
  xbuffer_left <- xc - r*sin(th) - max_real
  xbuffer_right <- xc + r*sin(th) + max_real
  ybuffer_top <- yc + r + max_real
  ybuffer_bottom <- yc - max_real
  if (x1 < xbuffer_left | x1 > xbuffer_right | y1 > ybuffer_top | y1 < ybuffer_bottom){
    counter <- counter + 1 # if x1,y1 lies outside the buffer
  }
  if (x2 < xbuffer_left | x2 > xbuffer_right | y2 > ybuffer_top | y2 < ybuffer_bottom){
    counter <- counter + 1 # if x2, y2 lies outside the buffer
  }
  if (counter > 0){
    return(TRUE) # one or both points lies outside the buffer
  }
  else{
    return(FALSE)
  }
}


## zero_frame
# INPUT:
# paired points = 6-column dataframe containing paired points and whether they break the loop
#       required column headings: x1, y2, breaks1, x2, y2, breaks2
# dz = four column array of parameters defining a sector-shaped detection zone
#       required column headings: x, y (xy coords of the camera), r (radius), th (angle)
# posdat_all = dataframe of all points that fell into the dz
#       required column headings: x, y (xy coords of the point), sequenceID, distance, detected (TRUE or FALSE for whether it got detected by the camera)
# OUTPUT:
# vector of TRUE or FALSE for whether each pair of points in paired_points is a zero-frame (i.e. whether the animal crossed the dz without getting detected when moving between the two points)
zero_frame <- function(paired_points, dz, posdat_all, max_real){
  x1 <- paired_points[1]
  y1 <- paired_points[2]
  breaks1 <- paired_points[3]
  x2 <- paired_points[4]
  y2 <- paired_points[5]
  breaks2 <- paired_points[6]
  p_line <- data.frame(x1 = x1, y1 = y1, x2 = x2, y2 = y2) # line between the two points
  dzx1 <- dz[1,1] # x coord of the camera
  dzy1 <- dz[1,2] # y coord of the camera
  r <- dz[1,3] # radius of dz
  th <- dz[1,4] # angle of dz
  xdiff <- r*sin(th)
  ydiff <- r*cos(th)
  dzx2 <- dzx1 - xdiff # x coord of left tip of dz
  dzy2 <- dzy1 + ydiff # y coord of left tip of dz
  dzx3 <- dzx1 + xdiff # x coord of right tip of dz
  dzy3 <- dzy2 # y coord of right tip of dz
  dz_line1 <- data.frame(x1 = dzx1, y1 = dzy1, x2 = dzx2, y2 = dzy2) # LHS line of dz
  dz_line2 <- data.frame(x1 = dzx1, y1 = dzy1, x2 = dzx3, y3 = dzy3) # RHS line of dz
  dz_arc <- data.frame(x = dzx1, y = dzy1, r = r, th = th) # arc of the dz
  
  if ((x1 %in% posdat_all$x & y1 %in% posdat_all$y) | (x2 %in% posdat_all$x & y2 %in% posdat_all$y)){
    zero <- FALSE # not a zero frame if one or both points fall in the dz 
  }
  else{
    if (breaks1 != breaks2){
      zero <- FALSE # not a zero frame if the animal looped round the back to get between the points
    }
    else{
      if (outside_buffer(x1, y1, x2, y2, dz, max_real)){
        zero <- FALSE # not a zero frame if one of the points lies outside the buffer outside which crossing the dz at that speed wouldn't be possible
      }
      else{ # for all remaining points:
        cross1 <- lines_cross(p_line, dz_line1) # = 1 if they intersect, = 0 if they don't
        cross2 <- lines_cross(p_line, dz_line2) # ditto
        cross3 <- line_arc_cross(p_line, dz_arc) # ditto
        cross_sum <- sum(cross1, cross2, cross3) 
        if (cross_sum > 1){ # if the line between the two points intersects 2 or more lines outlining the dz: assign as a zero frame
          zero <- TRUE
        }
        else{
          zero <- FALSE
        }
      }
    }
  }
  return(zero)
}


## extract_realised
# extract sets of speeds of length r_lengths
# INPUTS:
# realised_speeds = all speeds in the path (1 speed between each pair of consecutive points, with speed == distance between the points due to fixed time interval)
# r_lengths = mean length of observed speed sequences
# OUTPUTS:
# set of realised speeds of length r_length
extract_realised <- function(realised_speeds, r_lengths){ # function to extract one set of realised speeds
  firstIndex <- sample(seq(length(realised_speeds) - r_lengths + 1), 1)
  realised_speeds[firstIndex:(firstIndex + r_lengths -1)]
}


# seq_dat
# runs the simulation: generates a path and dz, then position data, then observed speeds of each sequence (sequence = one path which crosses the CT dz and is captured at at least 2 points)
# INPUTS:
# speed_parameter = vector of 10 input logged speeds
# step_no = number of steps for the animal's path
# size = size of the animal (1 = large, 0 = small)
# xlim = in form (x1, x2): sets the limits of the arena in which the simulated path stays (e.g. (0,40) == arena of size 40x40m)
# speedSD = standard deviation of input speed (i.e. how much the animal varies its speed about the mean input speed)
# speedCor = autocorrelation in speed
# kTurn = mean vonMises concentration parameter (kappa) for turn angle (higher = more concentrated) -- just like SD for normal distribution: how concentrated it is about the mean
# r = radius of detection zone
# th = angle of detection zone
# plot_path = TRUE or FALSE - if TRUE, plots the path & dz
# twoCTs = TRUE or FALSE - if TRUE, sets up 2 CTs next to each other
# OUTPUT:
# dataframe containing: 
# realised speeds
# observed speeds (same number of observed and realised speeds)
# lengths of observed speed sequences
# no. of single frames (just one number but repeated to fill the length of the dataframe)
# no. of zero frames (ditto)
# no. of points detected by the camera (ditto)
# + also a plot if plot_path = TRUE
seq_dat <- function(speed_parameter, step_no, size, xlim, speedSD, pTurn, speedCor, kTurn, kCor, x, y, r, th, plot_path = TRUE, twoCTs = FALSE){
  xlim <- xlim
  path <- pathgen(n=step_no, kTurn=kTurn, kCor=kCor, pTurn=pTurn, logspeed=speed_parameter, speedSD=speedSD, speedCor=speedCor, xlim=xlim, wrap=TRUE)
  if (twoCTs == FALSE){
    dz <- data.frame(x=x, y=y, r=r, th=th, dir=0) # initially set radius to 10m and theta to 1.65 - based on distributions of radii & angles in regent's park data -- then M & C said angle isn't usually more than 1 so set to 1
    if (size == 1){
      posdat_all <- sequence_data_large(path, dz) # posdat_all == all of those that fell in the detection zone (+ column saying whether or not it got detected)
    }
    if (size == 0){
      posdat_all <- sequence_data_small(path, dz)
    }
  }
  if (twoCTs == TRUE){
    dz1 <- data.frame(x=x, y=y, r=r, th=th, dir=0)
    dz2 <- data.frame(x = (x + r*sin(th)), y = (y + r*cos(th)), r = r, th = th, dir = 1)
    if (size == 1){
      posdat_all1 <- sequence_data_large(path, dz1) # posdat_all == all of those that fell in the detection zone (+ column saying whether or not it got detected)
      posdat_all2 <- sequence_data_large(path, dz2)
      posdat_all <- rbind(posdat_all1, posdat_all2)
    }
    if (size == 0){
      posdat_all1 <- sequence_data_small(path, dz1)
      posdat_all2 <- sequence_data_small(path, dz2)
      posdat_all <- rbind(posdat_all1, posdat_all2)
    }
  }
  posdat <- posdat_all[posdat_all$detected==TRUE,] # only the points which do actually get detected by the camera
  v <- calc_speed(posdat) # speeds of sequences
  
  ### realised speeds:
  obs_lengths <- c() # lengths of the observed speed sequences
  for (i in 1:length(unique(posdat$sequenceID))){
    p <- posdat[posdat$sequenceID==i,]
    obs_lengths <- c(obs_lengths, nrow(p))
  }
  r_lengths <- round(mean(obs_lengths)) # use mean of lengths of observed speed sequences as the number of position data points to use in realised speed segments
  realised_spds <- replicate(length(v$speed),{
    mean(extract_realised(path$speed, r_lengths)) # function to select sets of speeds of length r_lengths
  })
  
  ### number of single-frame sequences:
  t <- data.frame(table(posdat$sequenceID))
  n_singles <- nrow(t[t$Freq==1,]) # count the number of single-occurring numbers in the sequenceID column of posdat
  
  ### number of zero-frame sequences:
  path_df <- path$path
  path_df2 <- path_df
  path_df <- path_df[-nrow(path_df),] # remove last row
  path_df2 <- path_df2[-1,] # remove first row
  path_df_paired <- cbind(path_df, path_df2) # paired points
  colnames(path_df_paired) <- c("x1", "y1", "breaks1", "x2", "y2", "breaks2")
  max_real <- max(realised_spds) # max realised speed in this simulation run (used for buffer)
  zeros <- apply(path_df_paired, 1, zero_frame, dz = dz, posdat_all = posdat_all, max_real = max_real)
  n_zeros <- length(zeros[zeros==TRUE])
  
  df <- data.frame(realised = realised_spds, # realised speeds
                   observed = v$speed, # observed speeds
                   obs_lengths = c(obs_lengths, rep(NA, (length(v$speed)-length(obs_lengths)))),
                   n_singles = c(n_singles, rep(NA, (length(v$speed) - 1))), # no. of single frames
                   n_zeros = c(n_zeros, rep(NA, (length(v$speed) - 1))), # no. of zero frames
                   n_points = c(nrow(posdat), rep(NA, (length(v$speed) - 1)))) # total no. of position datapoints detected by the camera
  if (plot_path == TRUE){
    plot_wrap(path, lineargs = list(col="grey"))
    plot_dzone(dz, border=2)
    points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
  }
  return(df)
}



# mcsapply
# mc-version of sapply: (mclapply is the parallel version for lapply but there isn't an equivalent for sapply
# INPUTS: 
# same as usual for sapply: the vector of values on which to apply the function, the function to apply, and the value of any additional parameters needed for the function
# also add in mc.cores = (number of cores in your laptop)
mcsapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
  FUN <- match.fun(FUN)
  answer <- parallel::mclapply(X = X, FUN = FUN, ...)
  if (USE.NAMES && is.character(X) && is.null(names(answer)))
    names(answer) <- X
  if (!isFALSE(simplify) && length(answer))
    simplify2array(answer, higher = (simplify == "array"))
  else answer
}



# AIMS
# we wanna simulate where we know the actual speeds
# then be able to detect the biases
# currently: these simulations don't monitor all the potential biases
# e.g. missing high speeds
# it's all about quantifying bias
# comparing estimation outcomes with the truth


# extra bits of code not needed -------------------------------------------

### is_in_dz
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






### sequence_data
# original function:
# sequence_data <- function(pth, dzone){
#   pth <- pth$path[, c("x","y")] # format path into df with sequence of x and y
#   isin <- is_in_dz(pth, dzone) # returns TRUE or FALSE for whether each point in the path intersects with the dzone
#   isin <- as.vector(isin)
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










## plot_sim - not needed anymore bc now incorporated into seq_dat
# plots the path, dz, and points captured by the camera for a simulation
# INPUTS:
# speed_parameter = vector of 10 input logged speeds
# step_no = number of steps for the animal's path
# size = size of the animal (1 = large, 0 = small)
# xlim = in form (x1, x2): sets the limits of the arena in which the simulated path stays (e.g. (0,40) == arena of size 40x40m)
# speedSD = standard deviation of input speed (i.e. how much the animal varies its speed about the mean input speed)
# speedCor = autocorrelation in speed
# kTurn = mean vonMises concentration parameter (kappa) for turn angle (higher = more concentrated) -- just like SD for normal distribution: how concentrated it is about the mean
# r = radius of detection zone
# th = angle of detection zone
# OUTPUT:
# plot
# plot_sim <- function(speed_parameter, step_no, size, xlim, speedSD, pTurn, speedCor, kTurn, x, y, r, th){
#   xlim <- xlim
#   path <- pathgen(n=step_no, 
#                   kTurn=kTurn, 
#                   kCor=TRUE, 
#                   pTurn=pTurn, 
#                   logspeed=speed_parameter, 
#                   speedSD=speedSD, 
#                   speedCor=speedCor, 
#                   xlim=xlim,
#                   wrap=TRUE)
#   dz <- data.frame(x=x, y=y, r=r, th=th, dir=0)
#   
#   if (size == 1){
#     posdat <- sequence_data_large(path, dz)
#   }
#   if (size == 0){
#     posdat <- sequence_data_small(path, dz)
#   }
#   
#   plot_wrap(path, lineargs = list(col="grey"))
#   plot_dzone(dz, border=2)
#   points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
# }
# 
# 
# 
# ## plot_sim_2CTs_1 - ditto - now incorporated into seq_dat_2CTs_!
# # plots the path, dz, and points captured by the camera for a simulation
# # INPUTS:
# # speed_parameter = vector of 10 input logged speeds
# # step_no = number of steps for the animal's path
# # size = size of the animal (1 = large, 0 = small)
# # xlim = in form (x1, x2): sets the limits of the arena in which the simulated path stays (e.g. (0,40) == arena of size 40x40m)
# # speedSD = standard deviation of input speed (i.e. how much the animal varies its speed about the mean input speed)
# # speedCor = autocorrelation in speed
# # kTurn = mean vonMises concentration parameter (kappa) for turn angle (higher = more concentrated) -- just like SD for normal distribution: how concentrated it is about the mean
# # r = radius of detection zone
# # th = angle of detection zone
# # OUTPUT:
# # plot
# plot_sim_2CTs_1 <- function(speed_parameter, step_no, size, xlim, speedSD, pTurn, speedCor, kTurn, x, y, r, th){
#   xlim <- xlim
#   path <- pathgen(n=step_no, 
#                   kTurn=kTurn, 
#                   kCor=TRUE, 
#                   pTurn=pTurn, 
#                   logspeed=speed_parameter, 
#                   speedSD=speedSD, 
#                   speedCor=speedCor, 
#                   xlim=xlim,
#                   wrap=TRUE)
#   dz1 <- data.frame(x=x, y=y, r=r, th=th, dir=0)
#   dz2 <- data.frame(x = (x + r*sin(th)), y = (y + r*cos(th)), r = r, th = th, dir = 1)
#   
#   if (size == 1){
#     posdat1 <- sequence_data_large(path, dz1)
#     posdat2 <- sequence_data_large(path, dz2)
#     posdat <- rbind(posdat1, posdat2)
#   }
#   if (size == 0){
#     posdat1 <- sequence_data_small(path, dz1)
#     posdat2 <- sequence_data_small(path, dz2)
#     posdat <- rbind(posdat1, posdat2)
#   }
#   
#   plot_wrap(path, lineargs = list(col="grey"))
#   plot_dzone(dz, border=2)
#   points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
# }


# seq_dat_2CTs_1 - not needed anymore - incorporated into seq_dat
# runs the simulation: generates a path and dz, then position data, then observed speeds of each sequence (sequence = one path which crosses the CT dz and is captured at at least 2 points)
# INPUTS:
# speed_parameter = vector of 10 input logged speeds
# step_no = number of steps for the animal's path
# size = size of the animal (1 = large, 0 = small)
# xlim = in form (x1, x2): sets the limits of the arena in which the simulated path stays (e.g. (0,40) == arena of size 40x40m)
# speedSD = standard deviation of input speed (i.e. how much the animal varies its speed about the mean input speed)
# speedCor = autocorrelation in speed
# kTurn = mean vonMises concentration parameter (kappa) for turn angle (higher = more concentrated) -- just like SD for normal distribution: how concentrated it is about the mean
# r = radius of detection zone
# th = angle of detection zone
# plot_path = TRUE or FALSE - if TRUE, plots the path & dz
# OUTPUT:
# dataframe containing: 
# realised speeds
# observed speeds (same number of observed and realised speeds)
# no. of single frames (just one number but repeated to fill the length of the dataframe)
# no. of zero frames (ditto)
# no. of points detected by the camera (ditto)
# seq_dat_2CTs_1 <- function(speed_parameter, step_no, size, xlim, speedSD, pTurn, speedCor, kTurn, x, y, r, th, plot_path = TRUE){
#   xlim <- xlim
#   path <- pathgen(n=step_no,
#                   kTurn=kTurn,
#                   kCor=TRUE,
#                   pTurn=pTurn,
#                   logspeed=speed_parameter,
#                   speedSD=speedSD, # check each time you simulate a speed to make sure speedSD doesn't cause v unrealistic speeds
#                   speedCor=speedCor,
#                   xlim=xlim,
#                   wrap=TRUE)
#   dz1 <- data.frame(x=x, y=y, r=r, th=th, dir=0)
#   dz2 <- data.frame(x = (x + r*sin(th)), y = (y + r*cos(th)), r = r, th = th, dir = 1)
#   if (size == 1){
#     posdat_all1 <- sequence_data_large(path, dz1) # posdat_all == all of those that fell in the detection zone (+ column saying whether or not it got detected)
#     posdat_all2 <- sequence_data_large(path, dz2)
#     posdat_all <- rbind(posdat_all1, posdat_all2)
#   }
#   if (size == 0){
#     posdat_all1 <- sequence_data_small(path, dz1)
#     posdat_all2 <- sequence_data_small(path, dz2)
#     posdat_all <- rbind(posdat_all1, posdat_all2)
#   }
#   posdat <- posdat_all[posdat_all$detected==TRUE,] # only the points which do actually get detected by the camera
#   v <- calc_speed(posdat) # speeds of sequences
#   
#   ### realised speeds:
#   obs_lengths <- c() # lengths of the observed speed sequences
#   for (i in 1:length(unique(posdat$sequenceID))){
#     p <- posdat[posdat$sequenceID==i,]
#     obs_lengths <- c(obs_lengths, nrow(p))
#   }
#   r_lengths <- round(mean(obs_lengths)) # use mean of lengths of observed speed sequences as the number of position data points to use in realised speed segments
#   realised_spds <- replicate(length(v$speed),{
#     mean(extract_realised(path$speed, r_lengths)) # function to select sets of speeds of length r_lengths
#   })
#   
#   ### number of single-frame sequences:
#   t <- data.frame(table(posdat$sequenceID))
#   n_singles <- nrow(t[t$Freq==1,]) # count the number of single-occurring numbers in the sequenceID column of posdat
#   
#   ### number of zero-frame sequences:
#   path_df <- path$path
#   path_df2 <- path_df
#   path_df <- path_df[-nrow(path_df),] # remove last row
#   path_df2 <- path_df2[-1,] # remove first row
#   path_df_paired <- cbind(path_df, path_df2) # paired points
#   colnames(path_df_paired) <- c("x1", "y1", "breaks1", "x2", "y2", "breaks2")
#   max_real <- max(realised_spds) # max realised speed in this simulation run (used for buffer)
#   zeros <- apply(path_df_paired, 1, zero_frame, dz = dz, posdat_all = posdat_all, max_real = max_real)
#   n_zeros <- length(zeros[zeros==TRUE])
#   
#   df <- data.frame(realised = realised_spds, # realised speeds
#                    observed = v$speed, # observed speeds
#                    n_singles = c(rep(n_singles, length(v$speed))), # no. of single frames
#                    n_zeros = c(rep(n_zeros, length(v$speed))), # no. of zero frames
#                    n_points = c(rep(nrow(posdat), length(v$speed)))) # total no. of position datapoints detected by the camera
#   if (plot_path == TRUE){
#     plot_wrap(path, lineargs = list(col="grey"))
#     plot_dzone(dz, border=2)
#     points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
#   }
#   return(df)
# }



