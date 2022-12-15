## CamtrapSimulation.R ##

# contains all functions needed to run the simulation and analyse its results to investigate the two biases in average speed estimation

# abbreviations:
# MRS = mean realised speed (mean speed across the whole simulated animal movement path)
# MOS = mean observed speed (mean speed of the animal movement path using only what is captured by the camera trap)
# hmean = average speed estimated using a harmonic mean
# lnorm = average speed estimated by fitting a size-biased lognormal distribution
# gamma = average speed estimated by fitting a size-biased gamma distribution
# weibull = average speed estimated by fitting a size-biased weibull distribution

require(circular)
require(parallel)
require(rlist)
require(future.apply)
require(colortools) # for generating contrasting colours - use wheel("blue, 3) etc
require(ggnewscale)
require(bbmle)
require(MASS)



# SIMULATING THE ANIMAL MOVEMENT PATH -------------------------------------------------------------------------------------------------------------------------------------

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
# path: a dataframe with columns x and y (path co-ordinates) and, if wrap=TRUE, breaks indicating where wrap breaks occur
# turn, absturn: radian (absolute) turn angles for each step (turn ranging 0 to 2pi; absturn ranging 0 to pi)
# speed: step speeds
pathgen <- function(n, kTurn=0, Mb, speedCor=0, kCor=TRUE, pTurn=0.5, xlim=c(0,0), wrapped=TRUE, bimodal=FALSE, mov_prop=1, pTurn_mov=0.3, pTurn_feed=0.8){
  ylim=xlim
  vmax <- (8.356367*(Mb^0.25892))/(Mb^(0.06237*log10(Mb))) # set maxspeed - using body mass relationship from Garland 1983
  logspeedSD <- 0.8546151 # set fixed logspeedSD (calculated using regent's park & panama data)
  
  if (bimodal==FALSE){ # for unimodal movement:
    pTurn = 0.5
    logspeed <- log(0.1357288*(Mb^0.197178)) # set logspeed - using body mass relationship derived from regent's park & panama data (fitting lnorm)
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
    
    # moving:
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
    
    # feeding:
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


## wrap
# Takes a path object created with pathgen and wraps the co-ordinates within given limits. Toroidal in shape: if the path leaves one side loops back round to the opposite side
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



# SIMULATING THE DETECTION OF THE PATH BY A CAMERA TRAP ----------------------------------------------------------------------------------------------------------

## generate_seqdats
# run the simulation on each of the 100 paths for each repeat and save the seq_dats outputs to the seq_dats folder
# INPUTS
# path_nos: range of iter numbers to run the simulation on (vary depending on computational ability of local machine) - e.g. course laptop can take about 10 at once max
# effdist: logical. Whether or not to use body mass scaling of effective detection distance
# OUTPUT
# saved seq_dats.RData files in seq_dats folder
generate_seqdats <- function(parentfolder, outputfolder, Mb_range, path_nos, r, th, twoCTs, connectedCTs=FALSE, path_cutby = 1, scaling){
  for (j in Mb_range){
    for (i in path_nos){
      load(paste0(parentfolder,"Mb", j, "/iter", i, ".RData"))
      if (i == 1){
        plot_path <- TRUE # only plot the first one to save some time
      }
      else {
        plot_path <- FALSE
      }
      if (path_cutby == 1){
        seq_dats <- run_simulation(path, parentfolder, Mb=j, r=r, th=th, scaling=scaling, plot_path=plot_path, twoCTs=twoCTs, connectedCTs=connectedCTs)
      }
      else { # if want to cut the path short to make it computationally easier:
        path <- list(path$path[1:(500000*path_cutby+1),], path$turn[1:500000*path_cutby], path$absturn[1:500000*path_cutby], path$speed[1:500000*path_cutby])
        seq_dats <- run_simulation(path, parentfolder, Mb=j, r=r, th=th, scaling=scaling, plot_path=plot_path, twoCTs=twoCTs, connectedCTs=connectedCTs)
      }
      save(seq_dats, file = paste0(outputfolder, "Mb", j, "iter", i, ".RData"))
      rm(list = c("path", "seq_dats"))
    }
  }
}


# run_simulation
# simulates the detection of an animal movement path by a CT, generating information on the position datapoints detected by the CT, and plotting the first repeat of a given simulation run
# INPUTS
# path: path generated by pathgen function (see above)
# parentfolder: path to folder containing the pre-generated paths
# Mb: body mass of the animal in the simulated path
# r: radius of detection zone
# th: angle of detection zone
# scaling: logical. Whether or not to scale hz function with body mass
# plot_path: logical. If TRUE, plots the path & dz
# twoCTs: logical. Whether or not the simulation simulated two camera traps placed side-by-side
# connectedCTs: logical. If twoCTs = TRUE. Whether or not the two CTs are set up in a connected way such that the detection zones are triangles side-by-side facing opposite ways (hence maximising their area of contact and making one large rectangular-ish shaped dz)
# OUTPUT
# dataframe containing: 
  # position data info in a dataframe containing: x,y coords of detected points and the distances between detected points
    # x,y: x,y co-ordinates of sequence points in detection zones
    # sequenceID: integer sequence identifier
    # distance: distance traveled for each step between points
    # --> for both all points falling inside the dz as well as just the points which actually get detected (based on probability of detection modeled by hazard rate function)
  # realised speeds: set of realised speeds calculated by selecting sections of the path of equal length to average length of observed speed sequences and calculating the arithmetic mean of the mean speeds of these sections
  # observed speeds: mean of speeds in movement sequences captured by the CT 
  # lengths of observed speed sequences
  # no. of points detected by the camera (ditto)
  # + also a plot if plot_path = TRUE
run_simulation <- function(path, parentfolder, Mb, r, th, scaling, plot_path = TRUE, twoCTs = FALSE, connectedCTs = FALSE){
  
  ##### generate speed sequences ################################################################################################################################################
  if (twoCTs == FALSE){
    dz <- data.frame(x=20, y=10, r=r, th=th, dir=0) # initially set radius to 10m and theta to 1.65 - based on distributions of radii & angles in regent's park data -- then M & C said angle isn't usually more than 1 so set to 1
    posdat_all <- sequence_data(path, dz, Mb=Mb, scaling=scaling) # posdat_all == all of those that fell in the detection zone (+ column saying whether or not it got detected)
  }
  if (twoCTs == TRUE){
    dz1 <- data.frame(x=12, y=5, r=r, th=th, dir=0)
    if (connectedCTs == TRUE){
      dz2 <- data.frame(x = (dz1[1,1] + r*sin(th)), y = (dz1[1,2] + r*cos(th)), r = r, th = th, dir = 1) # place it directly next to the other CT
    }
    if (connectedCTs == FALSE){ 
      dz2 <- data.frame(x=27, y=25, r=r, th=th, dir=0)
    }
    posdat_all1 <- sequence_data(path, dz1, Mb=Mb, scaling=scaling) # posdat_all == all of those that fell in the detection zone (+ column saying whether or not it got detected)
    posdat_all2 <- sequence_data(path, dz2, Mb=Mb, scaling=scaling)
    posdat_all <- rbind(posdat_all1, posdat_all2)
  }
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
  
  ### realised speeds:
  r_lengths <- round(mean(obs_lengths)) # use mean of lengths of observed speed sequences as the number of position data points to use in realised speed segments
  realised_spds <- replicate(length(observed),{ # set number of realised speeds to select as the number of observed speeds (having removed NaNs and Inf)
    mean(extract_realised(path$speed, r_lengths)) # extract_realised: function to select sets of speeds of length r_lengths
  })
  
  
  # make output list
  output_list <- list(posdat_all = posdat_all,
                      posdat = posdat,
                      v = v,
                      realised = realised_spds,
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
# scaling: logical. Whether or not to scale hz function with body mass
# OUTPUT
# A data frame with columns:
# x,y: x,y co-ordinates of sequence points in detection zones
# sequenceID: integer sequence identifier
# distance: distance traveled for each step between points
sequence_data <- function(pth, dzone, Mb, scaling){
  pth <- pth$path[, c("x","y")] # format path into df with sequence of x and y
  isin_all <- is_in_dz(pth, dzone, Mb=Mb, scaling=scaling) # returns true or false for whether each position in the path is in the detection zone
  
  # to get xy, seqID, and dist for those that actually do get detected
  isin_detected <- as.vector(isin_all$detected)
  isin_detected[1] <- FALSE
  isin_detected <- as.logical(isin_detected)
  pth <- pth[rep(1:nrow(pth), nrow(dzone)), ]
  newseq <- tail(isin_detected, -1) > head(isin_detected, -1)
  seqid <- c(0, cumsum(newseq))[isin_detected] ## this is the problem
  xy <- pth[isin_detected, ] ## THIS LINE TAKES A WHILE
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
# scaling: logical. Whether or not to scale hz function with body mass
# OUTPUT
# A logical array defining whether each point (rows) is in the detection zone (columns)
is_in_dz <- function(point, dzone, Mb, scaling){
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
  
  # now for the ones which are true: reassign them as true based on probability of being detected at that distance from the CT
  new_reses <- apply(isindz_df, 1, reassign_prob, Mb=Mb, scaling=scaling)
  
  isindz_all <- data.frame(indz = isindz_df$res,
                           detected = new_reses)
  return(isindz_all)
}

## reassign_prob
# for all the points that cross the dz, reassign them as TRUE or FALSE based on probability of getting detected at that distance from the CT
# INPUTS
# a row in isindz_df2 dataframe from isindz function
# Mb: body mass
# scaling: logical. Whether or not to scale hz function with body mass
# OUTPUT
# vector of TRUE or FALSE for whether that point got detected
reassign_prob <- function(isindz_row, Mb, scaling){
  if (isindz_row[[1]]==FALSE){ # if the point didn't fall in the detection zone in the first place, don't need to do anything
    return(FALSE)
  }
  else {
    prob_radius <- hz_radius(as.numeric(isindz_row[[2]]), Mb=Mb, scaling=scaling) * 1.377284 # probability of being detected based on the estimated probability density for the radius - need to multiply by 1.377284 bc the max probability in the PDF is 0.7260668 so need to scale everything so that max probability of detection is 1
    if (prob_radius > 1){ # need this here otherwise it bugs
      prob_radius <- 1
    }
    new_res <- sample(c(TRUE,FALSE), 1, prob = c(prob_radius, 1-prob_radius)) # generate either TRUE or FALSE with prob of getting TRUE = prob of being detected and replace this in the main df
    return(new_res)
  }
}

## hz_radius
# hz function for distance detection probability
# INPUTS
# radius: radial distance of the point from the CT
# Mb: body mass
# scaling: logical. Whether or not to scale hz function with body mass
# OUTPUT
# probability of being detected at that radius
hz_radius <- function(radius, Mb, scaling){
  if (scaling==FALSE){
    prob <- 1 - exp(-(0.4255583/radius)^0.7369158) # general hz function parameterized using all species in Panama dataset
  }
  if (scaling==TRUE){
    a <- 2.183057*(Mb^0.5214121) # work out coefficients of hz function using scaling relationships with body mass
    g <- 31.99669*(Mb^0.5784485)
    prob <- 1 - exp(-(a/radius)^g)
  }
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



# GENERATE PLOTTING VARIABLES FROM PATHS AND SEQ_DATS ----------------------------------------------------------------------------------------------

## generate_plotting_variables
# analyses simulations from a range of input body masses and repeats of each, generating an .RData file of plotting variables to use to make plots
# INPUTS
# parentfolder: path to which main results folder to use (containing all the paths, seq_dats, and desired plotting_variables output folder)
# pathfolder: path to folder which the paths are in (to get info about the simulated paths)
# seq_datsfolder: path to folder where the seq_dats .RData files are (contain info about what the camera traps captured when the detection process was simulated)
# outputfolder: path to folder where to store the plotting variables .RData files
# Mb_range: vector containing the range of body masses to analyse data from
# iter_range: vector containing the range of iterations of each body mass to analyse data from
# r: radius of CT detection zone
# th: angle of CT detection zone
# part_of_wedge: which part of the wedge to take points from: 0=whole, 1=bottom third, 2 = middle third, 3 = top third
# twoCTs: logical. Whether or not the simulation simulated two camera traps placed side-by-side
# connectedCTs: logical. If twoCTs = TRUE. Whether or not the two CTs are set up in a connected way such that the detection zones are triangles side-by-side facing opposite ways (hence maximising their area of contact and making one large rectangular-ish shaped dz)
# scaling: logical. Whether to scale the hazard rate function for detection probability with body mass
# bimodal: logical. If TRUE, the simulated path contains a bimodal distribution of speeds. If FALSE, contains unimodal distribution of speeds.
# n_cores: number of cores in your laptop - to parallelise appropriately
# OUTPUT
# saves a list of plotting variables for each body mass (i.e. pooling together the multiple iterations of each) to be used in plotting_final.R script:
# variables: arithmetic MRS, 
generate_plotting_variables <- function(parentfolder, pathfolder, seq_datsfolder, outputfolder, Mb_range, iter_range, r, th, part_of_wedge=0, twoCTs=FALSE, connectedCTs=FALSE, scaling, bimodal, n_cores){
  
  # generate list of lists for each body mass (+ each list for each body mass containing itself 20 lists (20 = 1 for each iteration repeat))
  outputs_mb <- lapply(Mb_range, gen_plot_var_eachmass, parentfolder=parentfolder, pathfolder=pathfolder, seq_datsfolder=seq_datsfolder, iter_range=iter_range, r=r, th=th, part_of_wedge, twoCTs=twoCTs, connectedCTs=connectedCTs, scaling=scaling, bimodal=bimodal, n_cores)
  
  # save all together
  save(outputs_mb, file = paste0(parentfolder, outputfolder, "wedge", part_of_wedge,  "/Mb_all_iters", iter_range[1], "-", iter_range[length(iter_range)], ".RData"))

}


## gen_plot_var_eachmass
# called in generate_plotting_variables function
# generates plotting variables for each body mass within a category of analysis in parallel, calling gen_plot_var_iters to go through each set of 20 iters (within each body mass) in parallel too
# INPUTS
# Mb: body mass selected from Mb_range - this function runs in parallel across all body masses inputted in generate_plotting_variables function
# parentfolder: path to which main results folder to use (containing all the paths, seq_dats, and desired plotting_variables output folder)
# pathfolder: path to folder which the paths are in (to get info about the simulated paths)
# seq_datsfolder: path to folder where the seq_dats .RData files are (contain info about what the camera traps captured when the detection process was simulated)
# iter_range: vector containing the range of iterations of each body mass to analyse data from
# r: radius of CT detection zone
# th: angle of CT detection zone
# part_of_wedge: which part of the wedge to take points from: 0=whole, 1=bottom third, 2 = middle third, 3 = top third
# twoCTs: logical. Whether or not the simulation simulated two camera traps placed side-by-side
# connectedCTs: logical. If twoCTs = TRUE. Whether or not the two CTs are set up in a connected way such that the detection zones are triangles side-by-side facing opposite ways (hence maximising their area of contact and making one large rectangular-ish shaped dz)
# scaling: logical. Whether to scale the hazard rate function for detection probability with body mass
# bimodal: logical. If TRUE, the simulated path contains a bimodal distribution of speeds. If FALSE, contains unimodal distribution of speeds.
# n_cores: number of cores in your laptop - to parallelise appropriately
# OUTPUT
# dataframe containing variables needed for plotting
gen_plot_var_eachmass <- function(Mb, parentfolder, pathfolder, seq_datsfolder, iter_range, r, th, part_of_wedge, twoCTs=FALSE, connectedCTs=FALSE, scaling, bimodal, n_cores){ # run time = around 3 hours
  
  # work out variables for each iteration
  iters_outputs <- mclapply(iter_range, gen_plot_var_eachiter, Mb=Mb, parentfolder=parentfolder, pathfolder=pathfolder, seq_datsfolder=seq_datsfolder, scaling=scaling, part_of_wedge=part_of_wedge, r=r, th=th, twoCTs=FALSE, connectedCTs=FALSE, bimodal=bimodal, mc.cores=(n_cores-1))
  # produces list of 20 lists (1 for each iter)
  
  return(iters_outputs)
  
}

## gen_plot_var_eachiter
# function to apply to each iteration within a body mass category to produce a list of lists of all the variables needed in generate_plotting_variables to then fill the main vectors needed to be outputted
gen_plot_var_eachiter <- function(iter, Mb, scaling, parentfolder, pathfolder, seq_datsfolder, part_of_wedge, r, th, twoCTs=FALSE, connectedCTs=FALSE, bimodal){
  
  ## load in the path and seq_dats for that simulation run #####################################################################################
  
  load(paste0(parentfolder, seq_datsfolder, "Mb", Mb, "iter", iter, ".RData"))
  
  load(paste0(parentfolder, pathfolder, "Mb", Mb, "/iter", iter, ".RData"))
  
  
  ## mean realised speeds (i.e. true travel speed) ###################################################################################################################################
  
  p_real <- na.omit(path$speed) # point-to-point realised speeds
  p_real <- p_real[is.finite(p_real)]
  aMRS <- mean(p_real) # arithmetic MRS
  # gMRS <- c(gMRS, exp(mean(log(p_real)))) # geometric mean
  
  
  ## number of single frames and speeds of single frame sequences #######################################################################
  
  # make df with all xy coords of the path and their speeds:
  if (bimodal==TRUE){ # need to remove 2 coords to match up with speeds length bc stuck 2 paths together
    path_x <- path$path$x[-1]
    path_x <- path_x[-(length(path_x))]
    path_y <- path$path$y[-1]
    path_y <- path_y[-(length(path_y))]
    path_xy_v <- data.frame(x = path_x, # no speed for the first point nor the first of the second chunk stuck together - but just remove the last set of coords too bc that way each speed is at least matched up to one of the coords it's between
                            y = path_y, # doing this isn't a problem bc don't actually ever need to match up speeds with their exact coords in the right order
                            v = path$speed)
  }
  if (bimodal==FALSE){
    path_xy_v <- data.frame(x = path$path$x[-1], # no speed for the first point
                            y = path$path$y[-1],
                            v = path$speed) 
  }
  
  
  # store posdat_all as a df too:
  
  posdat_all <- seq_dats$posdat_all
  
  if (part_of_wedge==1){ # bottom third of wedge
    posdat_all <- posdat_all[posdat_all$y>=10 & posdat_all$y<13,]
  }
  if (part_of_wedge==2){ # middle third of wedge
    posdat_all <- posdat_all[posdat_all$y>=13 & posdat_all$y<16,]
  }
  if (part_of_wedge==3){ # top third of wedge
    posdat_all <- posdat_all[posdat_all$y>=16 & posdat_all$y<=19,]
  }
  
  # detection zone:
  if (twoCTs == FALSE){
    dz <- data.frame(x=20, y=10, r=r, th=th, dir=0)
  }
  if (twoCTs == TRUE){ # generate two detection zones and use both
    dz1 <- data.frame(x=12, y=5, r=r, th=th, dir=0)
    if (connectedCTs == TRUE){
      dz2 <- data.frame(x = (dz1[1,1] + r*sin(th)), y = (dz1[1,2] + r*cos(th)), r = r, th = th, dir = 1) # place it directly next to the other CT
    }
    if (connectedCTs == FALSE){ 
      dz2 <- data.frame(x=27, y=25, r=r, th=th, dir=0)
    }
  }
  
  
  
  ## speeds of single frames
  
  singles_v <- c() # to store stuff from this upcoming for loop
  singles_count <- 0 # ditto
  
  # select the seqID posdats when you only have one TRUE (i.e. single frame)
  for (k in unique(posdat_all$sequenceID)){
    p <- posdat_all[posdat_all$sequenceID==k,] # subset by sequence ID
    n_true <- nrow(p[p$detected==TRUE,])# subset by TRUE for detection to count the number of points detected by the CT in this sequence
    if (n_true == 1){ # if there is only one point detected by the CT (i.e. it's a single frame)
      
      ## work out the detection probability of that single frame:
      single_x <- p[p$detected==TRUE,]$x # select the x and y coords of the detected single point
      single_y <- p[p$detected==TRUE,]$y
      single_radius <- sqrt((single_y-dz$y)^2 + (single_x-dz$x)^2) # work out radius (distance from CT)
      prob_detect <- hz_radius(single_radius, Mb=Mb, scaling=scaling) # probability of detection using hazard function
      singles_count <- singles_count + prob_detect # add that to the number of single frames (so that it's a value that's taken probabilistic stuff into account)
      
      ## speeds of single frame sequences:
      path_xy_v["rownumber"] <- c(1:nrow(path_xy_v)) # assign rownumbers
      true_rownumber <- path_xy_v[path_xy_v$x==single_x & path_xy_v$y==single_y,]$rownumber # rownumber of the detected single point
      
      if (true_rownumber == 1){ # if the detected point is the first in the whole path, just use the point after it
        below_rownumber <- true_rownumber + 1
        rownumbers_needed <- c(true_rownumber, below_rownumber)
      }
      if (true_rownumber == nrow(p)){ # if the detected point is the last in the whole path, just use that point and the one before it
        above_rownumber <- true_rownumber - 1
        rownumbers_needed <- c(true_rownumber, above_rownumber)
      }
      else { # otherwise, use both the points below and above
        above_rownumber <- true_rownumber - 1
        below_rownumber <- true_rownumber + 1
        rownumbers_needed <- c(true_rownumber, above_rownumber, below_rownumber)
      }
      rows_needed <- path_xy_v[rownumbers_needed,] # isolate just the single frame and rows below and above it
      speed_single <- sum(rows_needed$v)/(nrow(rows_needed)-1) # speed = distance / time
      singles_v <- c(singles_v, speed_single) # store outside of this smaller for loop
    }
  }
  
  singles_v_mean <- mean(singles_v)
  n_singles <- singles_count  # ditto
  
  ## number of zero frames and speeds of zero frame sequences ######################################################################################
  
  ### number of zero-frame sequences:
  path_df <- path$path
  path_df2 <- path_df
  path_df <- path_df[-nrow(path_df),] # remove last row
  path_df2 <- path_df2[-1,] # remove first row
  path_df_paired <- cbind(path_df, path_df2) # paired points
  colnames(path_df_paired) <- c("x1", "y1", "breaks1", "x2", "y2", "breaks2")
  if (part_of_wedge==1){ # use only paired points whose y coords are within the range of the dz you're looking at in this run
    path_df_paired <- path_df_paired[path_df_paired$y1>=10 & path_df_paired$y1<13 & path_df_paired$y2>=10 & path_df_paired$y2<13,]
  }
  if (part_of_wedge==2){
    path_df_paired <- path_df_paired[path_df_paired$y1>=13 & path_df_paired$y1<16 & path_df_paired$y2>=13 & path_df_paired$y2<16,]
  }
  if (part_of_wedge==3){
    path_df_paired <- path_df_paired[path_df_paired$y1>=16 & path_df_paired$y1<=19 & path_df_paired$y2>=16 & path_df_paired$y2<=19,]
  }
  
  max_real <- max(path$speed) # max realised speed in this simulation run (used for buffer)
  if (twoCTs == FALSE){
    dz <- data.frame(x=20, y=10, r=r, th=th, dir=0)
    zeros <- future_apply(path_df_paired, 1, zero_frame, dz = dz, posdat_all = posdat_all, max_real = max_real, Mb=Mb, scaling=scaling)
  }
  if (twoCTs == TRUE){
    dz1 <- data.frame(x=12, y=5, r=r, th=th, dir=0)
    if (connectedCTs == TRUE){
      dz2 <- data.frame(x = (dz1[1,1] + r*sin(th)), y = (dz1[1,2] + r*cos(th)), r = r, th = th, dir = 1) # place it directly next to the other CT
    }
    if (connectedCTs == FALSE){
      dz2 <- data.frame(x=27, y=25, r=r, th=th, dir=0)
    }
    zeros1 <- future_apply(path_df_paired, 1, zero_frame, dz = dz1, posdat_all = posdat_all, max_real = max_real, Mb=Mb, scaling=scaling)
    zeros2 <- future_apply(path_df_paired, 1, zero_frame, dz = dz2, posdat_all = posdat_all, max_real = max_real, Mb=Mb, scaling=scaling)
    zeros <- c(zeros1, zeros2)
  }
  zeros_count <- sum(zeros[1:length(zeros)]) # add up all of the zero counts to get total number of zero frames in that simulation
  
  n_zeros <- zeros_count
  
  ## speeds of zero-frame sequences
  path_df_paired["ZERO"] <- zeros
  zeros_dat <- path_df_paired[path_df_paired$ZERO!=0,] # dataframe with only pairs of points which make a zero frame
  zeros_v <- c() # to save outside of this mini for loop
  for (l in 1:nrow(zeros_dat)){
    z <- zeros_dat[l,]
    speed <- sqrt((z$y2-z$y1)^2 + (z$x2-z$x1)^2) # speed = distance between the two points bc timestep = 1s
    zeros_v <- c(zeros_v, speed)
  }
  
  zeros_v_mean <- mean(zeros_v)
  
  

  
  ## mean observed speeds #################################################################################################################################
  
  posdat <- seq_dats$posdat # position datapoints that fall in dz and get detected
  
  if (part_of_wedge==1){ 
    posdat <- posdat[posdat$y>=10 & posdat$y<13,]
  }
  if (part_of_wedge==2){ # ditto for middle part of the wedge
    posdat <- posdat[posdat$y>=13 & posdat$y<16,]
  }
  if (part_of_wedge==3){ # ditto for top part of the wedge
    posdat <- posdat[posdat$y>=16 & posdat$y<=19,]
  }
  
  # work out observed speeds using mean of means  
  v <- calc_speed(posdat) # calculate speeds of each observed sequence of movement
  m_obs <- v$speed # speeds of sequences (= observed speeds)
  m_obs <- m_obs[is.finite(m_obs)]
  m_obs_sz <- c(m_obs, singles_v, zeros_v) # M's way of working out observed speeds + single & zero frames
  m_obs <- na.omit(m_obs)
  m_obs_sz <- na.omit(m_obs_sz)
  
  amMOS <- mean(m_obs) # arithmetic mean of observed speed sequences
  amMOS_sz <- mean(m_obs_sz) # ditto + adding in singles & zeros
  
  p_obs <- posdat$distance # point-to-point observed speeds irrespective of sequence
  p_obs <- p_obs[is.finite(p_obs)]
  p_obs_sz <- c(p_obs, singles_v, zeros_v) # including singles & zeros too
  p_obs <- na.omit(p_obs)
  p_obs_sz <- p_obs_sz[is.finite(p_obs_sz)]
  p_obs_sz <- na.omit(p_obs_sz)
  
  apMOS <- mean(p_obs)
  apMOS_sz <- mean(p_obs_sz)
  
  ## estimated speeds ###################################################################################################
  
  hmean_m <- (hmean_calc(m_obs))[1] # harmonic mean estimate using M's observed speeds
  hmean_m_sz <- (hmean_calc(m_obs_sz))[1] # including singles & zeros
  hmean_p <- (hmean_calc(p_obs))[1] # using raw point-to-point speeds
  hmean_p_sz <- (hmean_calc(p_obs_sz))[1]
  
  obs_df_m <- data.frame(speed = m_obs)
  obs_df_m_sz <- data.frame(speed = m_obs_sz)
  obs_df_p <- data.frame(speed = p_obs)
  obs_df_p_sz <- data.frame(speed = p_obs_sz)
  
  # fit the SBMs to estimate travel speed using lnorm, gamma, & weibull
  try(mods_m <- sbm3(speed~1, obs_df_m), silent=TRUE) # ignore any error messages (happen when it can't fit the models) and keep going
  
  if (length(mods_m)==0){ # if it couldn't fit the models, assign everything for that run as NA
    lnorm_m <- NA
    gamma_m <- NA
    weibull_m <- NA
  }
  else{
    lnorm_m <- predict.sbm(mods_m[[1]]$lnorm)[1,1]
    gamma_m <- predict.sbm(mods_m[[1]]$gamma)[1,1]
    weibull_m <- predict.sbm(mods_m[[1]]$weibull)[1,1]
  }
  
  # do the same with speed observations where have added singles & zeros back in
  try(mods_m_sz <- sbm3(speed~1, obs_df_m_sz), silent = TRUE)
  
  if (length(mods_m_sz)==0){
    lnorm_m_sz <- NA
    gamma_m_sz <- NA
    weibull_m_sz <- NA
  }
  else{
    lnorm_m_sz <- predict.sbm(mods_m_sz[[1]]$lnorm)[1,1]
    gamma_m_sz <- predict.sbm(mods_m_sz[[1]]$gamma)[1,1]
    weibull_m_sz <- predict.sbm(mods_m_sz[[1]]$weibull)[1,1]
  }
  
  try(mods_p <- sbm3(speed~1, obs_df_p), silent=TRUE)
  
  if (length(mods_p)==0){ # if it couldn't fit the models, assign everything for that run as NA
    lnorm_p <- NA
    gamma_p <- NA
    weibull_p <- NA
  }
  else{
    lnorm_p <- predict.sbm(mods_p[[1]]$lnorm)[1,1]
    gamma_p <- predict.sbm(mods_p[[1]]$gamma)[1,1]
    weibull_p <- predict.sbm(mods_p[[1]]$weibull)[1,1]
  }
  
  
  
  try(mods_p_sz <- sbm3(speed~1, obs_df_p_sz), silent=TRUE)
  
  if (length(mods_p_sz)==0){ # if it couldn't fit the models, assign everything for that run as NA
    lnorm_p_sz <- NA
    gamma_p_sz <- NA
    weibull_p_sz <- NA
  }
  else{
    lnorm_p_sz <- predict.sbm(mods_p_sz[[1]]$lnorm)[1,1]
    gamma_p_sz <- predict.sbm(mods_p_sz[[1]]$gamma)[1,1]
    weibull_p_sz <- predict.sbm(mods_p_sz[[1]]$weibull)[1,1]
  }
  
  
  # no. of detected & non-detected points
  n_points <- nrow(posdat_all)
  n_detected <- nrow(posdat)
  
  output <- data.frame(
    aMRS = aMRS,
    
    amMOS = amMOS,
    amMOS_sz = amMOS_sz,
    
    apMOS = apMOS,
    apMOS_sz = apMOS_sz,
    
    hmean_m = hmean_m,
    hmean_m_sz = hmean_m_sz,
    hmean_p = hmean_p,
    hmean_p_sz = hmean_p_sz,
    
    lnorm_m = lnorm_m,
    lnorm_m_sz = lnorm_m_sz,
    lnorm_p = lnorm_p,
    lnorm_p_sz = lnorm_p_sz,
    
    gamma_m = gamma_m,
    gamma_m_sz = gamma_m_sz,
    gamma_p = gamma_p,
    gamma_p_sz = gamma_p_sz,
    
    weibull_m = weibull_m,
    weibull_m_sz = weibull_m_sz,
    weibull_p = weibull_p,
    weibull_p_sz = weibull_p_sz,
    
    n_zeros = n_zeros,
    n_singles = n_singles,
    singles_v_mean = singles_v_mean,
    zeros_v_mean = zeros_v_mean,
    
    n_points = n_points,
    n_detected = n_detected
  )
  
  return(output)
}


## zero_frame
# INPUT:
# paired points = 6-column dataframe containing paired points and whether they break the loop
#       required column headings: x1, y2, breaks1, x2, y2, breaks2
# dz = four column array of parameters defining a sector-shaped detection zone
#       required column headings: x, y (xy coords of the camera), r (radius), th (angle)
# posdat_all = dataframe of all points that fell into the dz
#       required column headings: x, y (xy coords of the point), sequenceID, distance, detected (TRUE or FALSE for whether it got detected by the camera)
# Mb: body mass
# scaling: logical. Whether or not to scale hz function with body mass
# OUTPUT:
# vector of values for whether each pair of points in paired_points is a zero-frame (i.e. whether the animal crossed the dz without getting detected when moving between the two points)
zero_frame <- function(paired_points, dz, posdat_all, max_real, Mb, scaling){
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
    zero <- 0 # not a zero frame if one or both points fall in the dz 
  }
  else if (breaks1 != breaks2){
    zero <- 0 # not a zero frame if the animal looped round the back to get between the points
  }
  else if (outside_buffer(x1, y1, x2, y2, dz, max_real)){
    zero <- 0 # not a zero frame if one of the points lies outside the buffer outside which crossing the dz at that speed wouldn't be possible
  }
  else { # for all remaining points:
    cross1 <- lines_cross(p_line, dz_line1) # = 1 if they intersect, = 0 if they don't
    cross2 <- lines_cross(p_line, dz_line2) # ditto
    cross3 <- line_arc_cross(p_line, dz_arc) # ditto
    cross_sum <- sum(cross1, cross2, cross3) 
    if (cross_sum > 1){ # if the line between the two points intersects 2 or more lines outlining the dz: assign as a zero frame with a value given by the detection probability of the midpoint between the two points
      mx <- (x1+x2)/2 # midpoint x coord
      my <- (y1+y2)/2 # midpoint y coord
      midpoint_radius <- sqrt((mx-dzx1)^2 + (my-dzy1)^2)
      prob_detect <- hz_radius(midpoint_radius, Mb=Mb, scaling=scaling) # reassign probability of that point being detected based on hz function of detection probability at a distance from CT
      zero <- prob_detect
    }
    else{
      zero <- 0
    }
  }
  return(zero)
}


## outside_buffer
# returns logical array for whether points lies outside the buffer outside which crossing the dz at that speed wouldn't be possible
# used in zero_frame
# INPUTs
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


# Harmonic mean and standard error
# non-parametric
# (not fitting a distribution - just taking an average)
hmean_calc <- function(x){
  mn <- 1/mean(1/x)
  se <- mn^2 * sqrt(var(1/x)/length(x))
  c(mean=mn, se=se)
}


#https://www.geeksforgeeks.org/orientation-3-ordered-points/
orientation <- function(p1, p2, p3){
  sign((p2[,2]-p1[,2])*(p3[,1]-p2[,1]) - (p3[,2]-p2[,2])*(p2[,1]-p1[,1]))
}

#https://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
#INPUT
# lines1, lines2: 4-column arrays of x,y start and end points (ordered x1,y1,x2,y2) with the same number of rows in each line
#VALUE 
# 1 = they intersect
# 0 = they don't intersect
lines_cross <- function(lines1, lines2){
  p1 <- lines1[, 1:2]
  q1 <- lines1[, 3:4]
  p2 <- lines2[, 1:2]
  q2 <- lines2[, 3:4]
  or1 <- orientation(p1, q1, p2)
  or2 <- orientation(p1, q1, q2)
  or3 <- orientation(p2, q2, p1)
  or4 <- orientation(p2, q2, q1)
  res <- ifelse(or1!=or2 & or3!=or4, 1, 0)
  return(res)
}


## quadratic formula:
quad <- function(a, b, c) {
  if ((b^2 - 4 * a * c) > 0){
    solns <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
               (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
    return(solns)
  }
  else{
    return(NA)
  }
}

## line_arc_cross
# whether a line intersects an arc
# INPUT
# line: 4-column array of x,y start and end points (ordered x1,y1,x2,y2)
# arc: 4-column array of x,y centre of circle, radius r, and angle from centre to edge theta
# OUTPUT
# 1 = they intersect
# 0 = they don't intersect
line_arc_cross <- function(line, arc){
  # parametric equations for line:
  x1 <- line[1,1]
  y1 <- line[1,2]
  x2 <- line[1,3]
  y2 <- line[1,4]
  xvec <- x2 - x1
  yvec <- y2 - y1
  # paramx <- x1 + t*xvec
  # paramy <- y2 + t*yvec
  
  # parametric eqns for the circle which the arc is part of:
  xc <- arc[1,1]
  yc <- arc[1,2]
  r <- arc[1,3]
  th <- arc[1,4]
  # cparamx <- xc + rcos(th)
  # cparamy <- yc + rsin(th)
  
  # equate parametric equations for x and y:
  # x1 + t*xvec = xc + rcos(th)
  # y1 + t*yvec = yc + rsin(th)
  # use sin^2 + cos^2 = 1 to get quadratic equation for t, where:
  a <- xvec^2 + yvec^2
  b <- 2*x1*xvec - 2*xc*xvec + 2*y1*yvec - 2*yc*yvec
  c <- x1^2 + xc^2 - 2*x1*xc + y1^2 + yc^2 - 2*y1*yc - r^2
  
  t <- quad(a, b, c) # solve for t using quadratic formula
  
  if (length(t) < 2) {
    if (is.na(t)) { # if no solutions to t: they don't intersect
    answer <- 0
    }
  }
  else{
    # use values of t to find x and y then check if the corresponding theta lies along the arc (i.e. check whether when they do intersect it's along that specific arc of the circle)
    x_soln1 <- x1 + t[1]*xvec
    y_soln1 <- y2 + t[1]*yvec
    x_soln2 <- x1 + t[2]*xvec
    y_soln2 <- y2 + t[2]*yvec
    
    # find value of theta for each solution:
    th_soln1 <- atan((y_soln1 - yc)/(x_soln1 - xc))
    th_soln2 <- atan((y_soln2 - yc)/(x_soln2 - xc))
    
    if ((-th <= th_soln1 & th_soln1 <= th) | (-th <= th_soln2 & th_soln2 <= th)){ # if either solution lies in the correct range of theta, the line does intersect the arc
      answer <- 1
    }
    else{
      answer <- 0
    }
  }
  return(answer)
}


#Predict average speed
#INPUT
# mod: a size biased model created using sbm
# newdata: a dataframe containing covarariate values at which to predict speed
# reps: number of random replicates over which to calculate SE
predict.sbm <- function(mod, newdata=NULL, reps=1000){
  if(length(attr(terms(mod$formula), "term.labels")) > 0 & is.null(newdata))
    stop("Your model has covariates - please provide newdata")
  
  if(is.null(newdata)) newdata <- data.frame(lmean=0) else
    newdata$lmean <- 0
  cfs <- mod$model@coef
  scfs <- mvrnorm(reps, cfs, mod$model@vcov)
  i <- grep("lmean.", colnames(scfs))
  scfs <- scfs[,i]
  cfs <- cfs[i]
  ff <- formula(strsplit(mod$model@formula, ": ")[[1]][2])
  m <- model.frame(ff, newdata)
  nms <- names(m)[sapply(m[, 1:ncol(m)], class) == "factor"]
  for(nm in nms){
    if(nm %in% names(mod$model@data)) lvls <- levels(mod$model@data[[nm]]) else
      lvls <- levels(eval(as.name(nm)))
    levels(m[,nm]) <- lvls
  }
  mat <- model.matrix(ff, m)
  res <- exp(mat %*% t(scfs))
  outp <- data.frame(newdata[, -ncol(newdata)], 
                     est=exp(mat %*% matrix(cfs, ncol=1)),
                     se=apply(res, 1, sd),
                     lcl=apply(res, 1, quantile, 0.025),
                     ucl=apply(res, 1, quantile, 0.975)
  )
  names(outp)[1:(ncol(newdata))-1] <- names(newdata)[-ncol(newdata)]
  outp
}






## Calculating average speeds ##

setClass("sbm", representation("list"))




# Size biased log normal probability density
dsblnorm <- function(x, lmean, lsig, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dlnorm(x, lmean-exp(lsig)^2/2, exp(lsig)) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}  

#Size biased gamma probability density
dsbgamma <- function(x, lmean, lrate, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dgamma(x, exp(lmean)*exp(lrate), exp(lrate)) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}  

#Size biased Weibull probability density
dsbweibull = function(x, lmean, lshape, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dweibull(x, exp(lshape), exp(lmean)/gamma(1+1/exp(lshape))) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}  



#Size biased model
#INPUT
# formula: a model formula with speed variable on the left and covariates (or 1) on the right
# data: a dataframe containing the speed variable and any covariates
# pdf: which (size-biased) distribution to fit
sbm <- function(formula, data, pdf=c("lnorm", "gamma", "weibull"),
                var.range=c(-4,4), trace=FALSE){
  dstrbn=match.arg(pdf)
  y <- model.response(model.frame(formula, data))
  lmn <- log(hmean_calc(y)[1])
  lv <- switch(dstrbn,
               lnorm = log(sd(log(y))),
               gamma = log(exp(lmn)/var(y)),
               weibull = 0)
  startpars <- switch(dstrbn,
                      lnorm = list(lmean=lmn, lsig=lv),
                      gamma = list(lmean=lmn, lrate=lv),
                      weibull = list(lmean=lmn, lshape=lv))
  lwr <- switch(dstrbn,
                lnorm = c(lsig=var.range[1]),
                gamma = c(lrate=var.range[1]),
                weibull = c(lshape=var.range[1]))
  upr <- switch(dstrbn,
                lnorm = c(lsig=var.range[2]),
                gamma = c(lrate=var.range[2]),
                weibull = c(lshape=var.range[2]))
  f1 <- switch(dstrbn,
               lnorm = as.formula(paste(as.character(formula)[2], "~ dsblnorm(lmean, lsig)")),
               gamma = as.formula(paste(as.character(formula)[2], "~ dsbgamma(lmean, lrate)")),
               weibull = as.formula(paste(as.character(formula)[2], "~ dsbweibull(lmean, lshape)"))
  )
  f2 <- as.formula(paste("lmean ~", as.character(formula)[3]))
  model <- mle2(f1, start=startpars, data=data, method="L-BFGS-B",
                lower=lwr, upper=upr, parameters=list(f2), trace=trace)
  
  res <- list(model=model, pdf=dstrbn, formula=formula)
  class(res) <- "sbm"
  res
}




#Fits all three size biased options
#INPUT 
# As for sbm
#OUTPUT
# A list containing:
#  models: a list containing the three fitted sbm models
#  AICtab: a table of AIC and deltaAIC values for each model
sbm3 <- function(formula, data, reps=1000){
  mods <- list(sbm(formula, data, "lnorm"),
               sbm(formula, data, "gamma"),
               sbm(formula, data, "weibull")
  )
  names(mods) <- c("lnorm", "gamma", "weibull")
  AICs <- unlist(lapply(mods, AIC))
  i <- order(AICs)
  tab <- data.frame(AIC=AICs, dAIC=AICs-min(AICs))[i, ]
  rownames(tab) <- names(mods)[i]
  list(models=mods, AICtab=tab)
}

#Extract AIC from a size biased model
AIC.sbm <- function(obj) AIC(obj$model)


#Plot a size-biased model data and fitted distributions
#INPUT
# obj: a size biased model fitted with sbm
# log: whether to plot the distribution log scale
# lpar: plotting paramaters defining fitted line characteristics
# ...: other plotting arguments, if breaks given, passed to hist definition, otherwise passed to plot
##--> NB: I've added a title to this now
plot.sbm <- function(obj, log=TRUE, lpar=list(col="red"), add=FALSE, title, ...){
  if(length(attr(terms(obj$formula), "term.labels")) > 0)
    stop("Cannot plot covariate models")
  
  xname <- as.character(obj$formula)[2]
  dat <- obj$model@data
  if(xname %in% names(obj$model@data)) x <- get(xname, dat) else x <- get(xname)
  dots <- list(...)
  argnames <- names(dots)
  if("breaks" %in% argnames) brks <- dots["breaks"] else brks <- 50
  cfs <- coef(obj$model)
  if(log){
    lnx <- log(x)
    h <- do.call(hist, c(list(x=lnx, plot=FALSE), brks))
    sq <- exp(seq(min(lnx), max(lnx), len=256))
  } else{
    h <- do.call(hist, c(list(x=x, plot=FALSE), brks))
    sq <- seq(1e-10, max(x), len=256)
  }
  h$xname <- "x"
  den <- switch(obj$pdf,
                gamma = dsbgamma(sq, cfs[1], cfs[2], xlog=log),
                lnorm = dsblnorm(sq, cfs[1], cfs[2], xlog=log),
                weibull = dsbweibull(sq, cfs[1], cfs[2], xlog=log)
  )
  dots <- dots[!argnames=="breaks"]
  dots <- c(list(h, freq=FALSE), dots)
  if(!("main" %in% argnames)) dots <- c(dots, main="")
  if(!("xlab" %in% argnames)) dots <- c(dots, xlab=xname)
  if(!("ylim" %in% argnames)) dots <- c(dots, list(ylim=c(0,max(c(den, h$density)))))
  if(!add) do.call(plot, dots)
  if(log) sq <- log(sq)
  do.call(lines, c(list(x=sq, y=den), lpar))
  title(paste(title, "model", sep = " "))
}

## calc_hmean
# work out harmonic mean of a set of observed speeds (i.e. each simulation rep)
# returns a harmonic mean and standard error for each set of speeds
# INPUT:
# number of reps of the simulation - so that can loop through every set of observed speeds
calc_hmean <- function(speed_no){
  s <- seq_dats[,speed_no]
  o <- s$observed
  o <- o[!is.nan(o)]
  hmean(o)
}

## mods_all_fit
# fits all 3 SBMs (lognormal, gamma, Weibull) to each set of observed speeds (i.e. each simulation rep)
# return three models for each set of measured speeds
# INPUT:
# number of reps of the simulation
mods_all_fit <- function(speed_no){
  s <- seq_dats[,i]
  s$observed <- na.omit(s$observed)
  if (length(s$observed) < 2){ # skip the ones where there aren't enough observed speeds
    next
  }
  df <- data.frame(speed = s$observed)
  sbm3(speed~1, df) # fit all three models
}

## predict_lnorm
# predict average speed using a fitted lognormal model for each set of observed speeds (i.e. each simulation rep)
# INPUT:
# number of reps of the simulation
predict_lnorm <- function(speed_no){
  predict.sbm(mods[[1,speed_no]]$lnorm)[1]
}

## predict_gamma
# predict average speed using a fitted gamma model for each set of observed speeds (i.e. each simulation rep)
# INPUT:
# number of reps of the simulation
predict_gamma <- function(speed_no){
  predict.sbm(mods[[1,speed_no]]$gamma)[1]
}

## predict_weibull
# predict average speed using a fitted Weibull model for each set of observed speeds (i.e. each simulation rep)
# INPUT:
# number of reps of the simulation
predict_weibull <- function(speed_no){
  predict.sbm(mods[[1,speed_no]]$weibull)[1]
}

# # lnorm_AIC_extract
# return the AIC for the fitted lognormal model for each set of observed speeds
# INPUT:
# number of reps of the simulation
lnorm_AIC_extract <- function(speed_no){
  a1 <- mods[2,speed_no]
  a2 <- a1$AICtab[1]
  a2["lnorm",]
}

## gamma_AIC_extract
# return the AIC for the fitted gamma model for each set of observed speeds
# INPUT:
# number of reps of the simulation
gamma_AIC_extract <- function(speed_no){
  a1 <- mods[2,speed_no]
  a2 <- a1$AICtab[1]
  a2["gamma",]
}

## weibull_AIC_extract
# return the AIC for the fitted Weibull model for each set of observed speeds
# INPUT:
# number of reps of the simulation
weibull_AIC_extract <- function(speed_no){
  a1 <- mods[2,speed_no]
  a2 <- a1$AICtab[1]
  a2["weibull",]
}

# calculate errors between mean realised and estimated speeds:
hmean_error_real_calc <- function(speed_no){
  as.numeric(harmonics[1, speed_no]) - mean(seq_dats[,speed_no]$realised) #-- negative == means the estimated speed is smaller than the realised speed
}
lnorm_error_real_calc <- function(speed_no){
  as.numeric(mods_predict_lnorm[speed_no]) - mean(seq_dats[,speed_no]$realised)
}
gamma_error_real_calc <- function(speed_no){
  as.numeric(mods_predict_gamma[speed_no]) - mean(seq_dats[,speed_no]$realised)
}
weibull_error_real_calc <- function(speed_no){
  as.numeric(mods_predict_weibull[speed_no]) - mean(seq_dats[,speed_no]$realised)
}