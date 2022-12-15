## CamtrapSimulation

# all code required to run the simulation described in the travel speed manuscript

require(circular)
require(parallel)
require(bbmle)
require(MASS)

# Simulating a moving animal ------------------------

## FUNCTIONS ##

## pathgen
## generates a path of x, y positions using a correlated random walk
# INPUTS
# n: number of steps
# pTurn: probability of turning at each step
# kTurn: mean vonMises concentration parameter (kappa) for turn angle (higher = more concentrated about the mean)
# Mb: body mass of the simulated animal
# speedCor: autocorrelation in speed
# kCor: logical. Whether to correlate kappa with speed
# xlim: x axis limits within which to pick the starting point (y axis limits are then assigned as the same as xlims)
# wrapped: logical. Whether to wrap the path
# bimodal: logical. Whether to simulate bimodal movement (slow, more tortuous vs fast, less tortuous movement)
# mov_prop: if bimodal movement is being simulated, this is the proportion of time spent in the fast, less tortuous movement (decimal between 0 and 1)
# pTurn_mov: if bimodal movement is being simulated, this is the probability of turning in the fast, less tortuous movement behaviour
# pTurn_feed: if bimodal movement is being simulated, this is the probability of turning in the slow, more tortuous movement behaviour
# OUTPUT
# A list with elements:
# path: a dataframe with columns x and y (path co-ordinates) and, if wrapped=TRUE (default), breaks indicating where wrap breaks occur
# turn, absturn: radian (absolute) turn angles for each step (turn ranging 0 to 2pi; absturn ranging 0 to pi)
# speed: step speeds
pathgen <- function(n, kTurn=0, Mb, speedCor=0, kCor=TRUE, pTurn=0.5, xlim=c(0,0), wrapped=TRUE, bimodal=FALSE, mov_prop=1, pTurn_mov=0.3, pTurn_feed=0.8){
  ylim=xlim
  vmax <- (8.356367*(Mb^0.25892))/(Mb^(0.06237*log10(Mb))) # set maxspeed - using body mass relationship from Garland 1983
  logspeedSD <- 0.8546151 # set fixed logspeedSD (calculated using Regent's park & Panama data)
  
  if (bimodal==FALSE){ # for unimodal movement:
    pTurn = 0.5
    logspeed <- log(0.1357288*(Mb^0.197178)) # set logspeed - using body mass-travel speed relationship derived from regent's park & panama data (fitting a log-normal distribution to estimate travel speed (Rowcliffe et al. 2016) and using body masses from the PanTHERIA database (Jones et al. 2009))
    spds <- exp(rautonorm(n, logspeed, logspeedSD, speedCor)) # generates set of autocorrelated variates - generate speeds on a log scale (bc lognormally distributed) then exponentiate to get them back to normal space
    spds <- spds[spds<vmax] # cap those spds at the max speed
    n_capped <- length(spds) # set new number of steps based on how may speeds you now have
    tTurn <- rbinom(n_capped,1,pTurn) # generates set of n (= no of steps) numbers either 1 and 0 where higher probability of turning at each step = more likely to have 1
    if(kCor==TRUE){ # if we want to correlate kappa with speed:
      kappas <- kTurn * spds / mean(spds)
      deviates <- sapply(kappas, function(x) as.numeric(rvonmises(1,circular(0),x)))
    } 
    else 
      deviates <- as.numeric(rvonmises(n_capped, circular(0), kTurn)) # get one turning number per speed 
    deviates[tTurn==0] <- 0 # wherever you shouldn't turn at all, set deviate to 0 so that you don't turn
    angles <- runif(1)*2*pi + cumsum(deviates) # transforms deviates into angles corresponding to the amount you turn at each step
    
    x <- c(0, cumsum(spds*sin(angles))) + runif(1,xlim[1],xlim[2]) # spds is being used as the hypotenuse for each step -- so acts like distance
    y <- c(0, cumsum(spds*cos(angles))) + runif(1,ylim[1],ylim[2])
    absdevs <- deviates
    i <- absdevs>pi
    absdevs[i] <- 2*pi-absdevs[i]
    absdevs <- abs(absdevs)
    res <- list(path=data.frame(x,y), turn=deviates, absturn=absdevs, speed=spds)
    if(wrapped) res <- wrap(res, xlim, ylim)
  }
  
  if (bimodal==TRUE){ # for bimodal movement:
    
    # number of steps to do in each behaviour:
    n_mov <- mov_prop*n
    n_feed <- n-n_mov
    
    # parameters for each distribution
    logspeed_feed <- log(0.1357288*(Mb^0.197178)*0.7) # 30% decrease in speed - using the same scaling relationship as for unimodal speeds
    logspeed_mov <- log(0.1357288*(Mb^0.197178)*1.3) # 30% increase in speed
    
    # generate and cap speeds for each distribution
    spds_mov <- exp(rautonorm(n_mov, logspeed_mov, logspeedSD, speedCor)) 
    spds_mov <- spds_mov[spds_mov<vmax] 
    n_mov_capped <- length(spds_mov) 
    
    spds_feed <- exp(rautonorm(n_feed, logspeed_feed, logspeedSD, speedCor))
    spds_feed <- spds_feed[spds_feed<vmax] 
    n_feed_capped <- length(spds_feed) 
    
    
    # generate path for each distribution:
    
    # fast behaviour:
    tTurn_mov <- rbinom(n_mov_capped,1,pTurn_mov) 
    if(kCor==TRUE){ 
      kappas_mov <- kTurn * spds_mov / mean(spds_mov)
      deviates_mov <- sapply(kappas_mov, function(x) as.numeric(rvonmises(1,circular(0),x)))
    } 
    else 
      deviates_mov <- as.numeric(rvonmises(n_mov_capped, circular(0), kTurn)) 
    deviates_mov[tTurn_mov==0] <- 0 
    angles_mov <- runif(1)*2*pi + cumsum(deviates_mov) 
    x_mov <- c(0, cumsum(spds_mov*sin(angles_mov))) + runif(1,xlim[1],xlim[2]) 
    y_mov <- c(0, cumsum(spds_mov*cos(angles_mov))) + runif(1,ylim[1],ylim[2])
    absdevs_mov <- deviates_mov
    i <- absdevs_mov>pi
    absdevs_mov[i] <- 2*pi-absdevs_mov[i]
    absdevs_mov <- abs(absdevs_mov)
    
    # slow behaviour:
    tTurn_feed <- rbinom(n_feed_capped,1,pTurn_feed) 
    if(kCor==TRUE){ 
      kappas_feed <- kTurn * spds_feed / mean(spds_feed)
      deviates_feed <- sapply(kappas_feed, function(x) as.numeric(rvonmises(1,circular(0),x)))
    } 
    else 
      deviates_feed <- as.numeric(rvonmises(n_feed_capped, circular(0), kTurn))
    deviates_feed[tTurn_feed==0] <- 0 
    angles_feed <- runif(1)*2*pi + cumsum(deviates_feed) 
    x_feed <- c(0, cumsum(spds_feed*sin(angles_feed))) + runif(1,xlim[1],xlim[2]) 
    y_feed <- c(0, cumsum(spds_feed*cos(angles_feed))) + runif(1,ylim[1],ylim[2])
    absdevs_feed <- deviates_feed
    i <- absdevs_feed>pi
    absdevs_feed[i] <- 2*pi-absdevs_feed[i]
    absdevs_feed <- abs(absdevs_feed)
    
    # combine the 2 into the final list
    x <- c(x_mov, x_feed)
    y <- c(y_mov, y_feed)
    deviates <- c(deviates_mov, deviates_feed)
    absdevs <- c(deviates_mov, deviates_feed)
    spds <- c(spds_mov, spds_feed)
    res <- list(path=data.frame(x,y), turn=deviates, absturn=absdevs, speed=spds)
    if(wrapped) res <- wrap(res, xlim, ylim)
  }
  return(res)
}

## rautonorm
# generates a set of autocorrelated random normal variates - used in pathgen to correspond to different steps
# INPUTS
# n: number of variates to generate
# mean, sd: mean and standard deviation of the normal distribution
# r: the autocorrelation coefficient (between 0 and 1)
# OUTPUT
# vector of autocorrelated random normal variates
rautonorm <- function(n,mean=0,sd=1,r){
  ranfunc <- function(i,z,r) sqrt(1-r^2) * sum(z[2:(i+1)]*r^(i-(1:i))) + z[1]*r^i # for the autocorrelation
  z <- rnorm(n)
  mean + sd*c(z[1], sapply(1:(n-1), ranfunc, z, r))
}

## wrap
# Takes a path object created with pathgen and wraps the co-ordinates within given limits. Toroidal in shape: if the path leaves on one side loops back round to the opposite side
# INPUTS
# path: a two column array of x,y positions defining the path
# xlim, ylim: the x,y limits within which to wrap the path
# OUTPUT
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
# plots a wrapped path
# INPUTS
# path: a wrapped path object created by pathgen
# type: l(ine), p(oint) or b(oth)
# add: add to existing plot or create new one
# axisargs, lineargs, pointargs: lists of arguments to control axis lines or point characteristics
# OUTPUT
# plot of the wrapped path
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

# plot_dzone
# convenience function for plotting detection zones (adds to existing plot generated by plot_wrap)
# INPUT
# dzone: a four column array of parameters as defined above
# OUTPUT
# addition of detection zone to the plot generated by plot_wrap
plot_dzone <- function(dzone, ...){
  for(i in 1:nrow(dzone)){
    sq <- with(dzone[i, ], seq(dir-th/2, dir+th/2, len=50))
    poly <- with(dzone[i, ], cbind(x + c(0, r*sin(sq)), y + c(0, r*cos(sq)))) # set of x and y coords representing the points on perimeter of the dz polygon
    polygon(poly, ...) # draws the dz polygon
  }
}


## DEFINE PARAMETERS ##

Mb = 1 # body mass in kilograms
xlim = c(0,40) # limits of the arena (in metres)
n = 5e5 # number of steps
speedCor = 0.9 # autocorrelation in speed
kTurn = 2 # kappa
kCor = TRUE # whether to correlate kappa with speed
pTurn = 0.5 # for unimodal speeds
mov_prop = 0.1 # simulate: 0.1, 0.25, 0.4, 0.75, 0.9
pTurn_mov = 0.8 # for bimodal speeds
pTurn_feed = 0.3 # for bimodal speeds
bimodal = TRUE # if TRUE, simulates bimodal speed. If FALSE, simulates unimodal speed


## GENERATE THE PATH ##

path <- pathgen(n=n, kTurn=kTurn, kCor=kCor, pTurn=pTurn, Mb=Mb, speedCor=speedCor, xlim=xlim, bimodal=bimodal, mov_prop=mov_prop, pTurn_mov=pTurn_mov, pTurn_feed=pTurn_feed)


# Simulating detection by a camera trap -----------------------------------

# runs the simulated camera trap on a set of 100 paths (all repeats with the same parameters - using functions above)

## FUNCTIONS ##

# run_simulation
# simulates the detection of an animal movement path by a CT, generating information on the position datapoints detected by the CT, and plotting the first repeat of a given simulation run
# INPUTS
# path: path generated by pathgen function (see above)
# parentfolder: path to folder containing the pre-generated paths
# Mb: body mass of the animal in the simulated path
# r: radius of detection zone
# th: angle of detection zone
# plot_path: logical. If TRUE, plots the path & dz
# OUTPUT
# dataframe containing: 
# position data info in a dataframe containing: x,y coords of detected points and the distances between detected points
# x,y: x,y co-ordinates of sequence points in detection zones
# sequenceID: integer sequence identifier
# distance: distance traveled for each step between points
# --> for both all points falling inside the dz as well as just the points which actually get detected (based on probability of detection modeled by hazard rate function)
# observed speeds: mean of speeds in movement sequences captured by the CT 
# lengths of observed speed sequences
# no. of points detected by the camera trap
# + also a plot if plot_path = TRUE
run_simulation <- function(path, parentfolder, Mb, r, th, plot_path = TRUE){
  
  ##### generate speed sequences ################################################################################################################################################
  dz <- data.frame(x=20, y=10, r=r, th=th, dir=0) # initially set radius to 10m and theta to 1.65 - based on distributions of radii & angles in regent's park data -- then M & C said angle isn't usually more than 1 so set to 1
  posdat_all <- sequence_data(path, dz, Mb=Mb) # posdat_all == all of those that fell in the detection zone (+ column saying whether or not it got detected)

  posdat <- posdat_all[posdat_all$detected==TRUE,] # only the points which do actually get detected by the camera
  v <- calc_speed(posdat) # speeds of sequences (== observed speeds)
  
  observed <- v$speed
  observed <- observed[is.finite(observed)]
  
  ##### work out things to store in the output list #############################################################################################################
  
  ### observed speed sequence lengths:
  obs_lengths <- c()
  for (i in unique(posdat$sequenceID)){
    p <- posdat[posdat$sequenceID==i,]
    if (nrow(p)>1){ # don't count the single-frame sequences
      obs_lengths <- c(obs_lengths, nrow(p))
    }
  }
  
  # make output list
  output_list <- list(posdat_all = posdat_all,
                      posdat = posdat,
                      v = v,
                      observed = observed,
                      obs_lengths = obs_lengths,
                      n_points = nrow(posdat)) # total number of position datapoints detected by the CT
  
  # plot path
  if (plot_path == TRUE){
    png(file= paste0(parentfolder, "Mb", Mb, "/plot.png"),
        width=700, height=650)
    plot_wrap(path, lineargs = list(col="grey"))
    plot_dzone(dz, border=2)
    points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
    dev.off()
  }
  
  return(output_list)
}


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
# dzone: a detection zone array
# Mb: body mass
# OUTPUT
# A data frame with columns:
# x,y: x,y co-ordinates of sequence points in detection zones
# sequenceID: integer sequence identifier
# distance: distance traveled for each step between points
sequence_data <- function(pth, dzone, Mb){
  pth <- pth$path[, c("x","y")] # format path into df with sequence of x and y
  isin_all <- is_in_dz(pth, dzone, Mb=Mb) # returns true or false for whether each position in the path is in the detection zone
  
  # to get xy, seqID, and dist for those that actually do get detected
  isin_detected <- as.vector(isin_all$detected)
  isin_detected[1] <- FALSE
  isin_detected <- as.logical(isin_detected)
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
  isin_indz <- as.logical(isin_indz)
  pth2 <- pth[rep(1:nrow(pth), nrow(dzone)), ] # what does this line do??
  newseq2 <- tail(isin_indz, -1) > head(isin_indz, -1)
  seqid2 <- c(0, cumsum(newseq2))[isin_indz]
  xy2 <- pth2[isin_indz, ] # THIS ONE TOO
  dist2 <- sqrt(diff(xy2$x)^2 + diff(xy2$y)^2)
  newseq2 <- tail(seqid2, -1) > head(seqid2, -1)
  dist2[newseq2] <- NA
  dist2 <- c(NA, dist2)
  df2 <- data.frame(xy2, sequenceID = seqid2, distance = dist2)
  
  df2["detected"] <- FALSE
  df2$detected[which(df2$x %in% df1$x & df2$y %in% df1$y)] <- TRUE
  
  return(df2)
}


## is_in_dz
# Defines whether points are within the hard boundary of the CT detection zone
# INPUT:
# point: a two column x,y array of point positions
# dzone: four column array of parameters defining a sector-shaped detection zone
#        required column headings:
#           x,y: x,y coordinates of camera
#           r, th: detection zone radius and angle
#           dir: radian direction in which the camera is facing
# OUTPUT
# A logical array defining whether each point (rows) is in the detection zone (columns)
is_in_dz <- function(point, dzone, Mb){
  ij <- expand.grid(1:nrow(point), 1:nrow(dzone)) # expanding rows for each point and dzone
  pt <- point[ij$Var1, ] 
  dz <- dzone[ij$Var2, ] 
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
  
  # for the ones which are true: reassign them as true based on probability of being detected at that distance from the CT
  new_reses <- apply(isindz_df, 1, reassign_prob, Mb=Mb)
  
  isindz_all <- data.frame(indz = isindz_df$res,
                           detected = new_reses)
  return(isindz_all)
}

## reassign_prob
# for all the points that cross the dz, reassign them as TRUE or FALSE based on probability of getting detected at that distance from the CT
# INPUTS
# a row in isindz_df2 dataframe from isindz function
# Mb: body mass
# OUTPUT
# vector of TRUE or FALSE for whether that point got detected
reassign_prob <- function(isindz_row, Mb){
  if (isindz_row[[1]]==FALSE){ # if the point didn't fall in the detection zone in the first place, don't need to do anything
    return(FALSE)
  }
  else {
    prob_radius <- hz_radius(as.numeric(isindz_row[[2]]), Mb=Mb) * 1.377284 # probability of being detected based on the estimated probability density for the radius - need to multiply by 1.377284 bc the max probability in the PDF is 0.7260668 so need to scale everything so that max probability of detection is 1
    if (prob_radius > 1){ # need this here otherwise it bugs
      prob_radius <- 1
    }
    new_res <- sample(c(TRUE,FALSE), 1, prob = c(prob_radius, 1-prob_radius)) # generate either TRUE or FALSE with prob of getting TRUE = prob of being detected and replace this in the main df
    return(new_res)
  }
}

## hz_radius
# hazard rate function for distance detection probability
# INPUTS
# radius: radial distance of the point from the CT
# Mb: body mass
# OUTPUT
# probability of being detected at that radius
hz_radius <- function(radius, Mb){
  a <- 2.183057*(Mb^0.5214121) # work out coefficients of hz function using scaling relationships with body mass
  g <- 31.99669*(Mb^0.5784485)
  prob <- 1 - exp(-(a/radius)^g)

  if (prob > 1){
    prob <- 1
  }
  if (prob < 0){
    prob <- 0
  }
  return(prob)
}


## calc_speed
# summarises speeds for a dataframe of position sequences
# INPUT
# dat: a dataframe of position observations created by sequence_data (above)
# OUTPUT
# A dataframe with a row per sequence and columns:
# sequenceID: integer sequence identifier
# distance: total distance travelled during the sequence
# points: number of points in the sequence
# speed: overall sequence speed
calc_speed <- function(dat){
  dist <- with(dat, tapply(distance, sequenceID, sum, na.rm=TRUE)) # for those that have the same sequence ID, sum their distances to get total distance travelled within the detection zone
  points <- with(dat, tapply(distance, sequenceID, length)) # number of points in the sequence
  speed <- dist/(points-1) # time = n_points-1 bc time between each point is 1s
  data.frame(sequenceID=unique(dat$sequenceID), distance=dist, points=points, speed=speed)
}


## DEFINE PARAMATERS ##

path
parentfolder <- paste0("") # parent folder containing pre-generated path

Mb = 1 # body mass
r = 9 # CT radius
th = 0.7 # CT angle
plot_path = TRUE # logical. whether or not to plot the simulated path & CT

## OVERLAY THE SIMULATED PATH WITH THE CAMERA TRAP ## 

seq_dats <- run_simulation(path, parentfolder, Mb=j, r=r, th=th, plot_path=plot_path) # generates sequence data of the information stored by the simulated camera trap


