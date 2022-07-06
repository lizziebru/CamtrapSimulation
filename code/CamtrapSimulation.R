
require(circular)
require(parallel)
require(rlist)
require(future.apply)
require(colortools) # for generating contrasting colours - use wheel("blue, 3) etc
require(ggnewscale)

# to test things:


# # # set speed parameter
# # speed_parameter <- log(0.02) # list whatever speeds you want (can be multiple) - and if you want 10 of each need -J 1-40 (needs to be a multiple of number of speeds)
# # #
# # # # set parameters
# # xlim = c(0,40)
# step_no = 5e4
# # size = 0
# # pTurn = 0.5
# # speedCor = 0.9
# # kTurn = 2
# # kCor = TRUE
# # #
# path <- pathgen(n=step_no, kTurn=kTurn, kCor=kCor, pTurn=pTurn, logspeed=speed_parameter, size=size, speedCor=speedCor, xlim=xlim)


# path <- pathgen(5e4, kTurn=2, kCor=TRUE, pTurn=0.5,
#                 logspeed=-2, speedSD=1, speedCor=0.9,
#                 xlim=c(0,40), wrap=TRUE)
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
# Mb: body mass - then defines meanlogspeed and maxlogspeed
# speedCor: autocorrelation in speed
# kCor: whether to correlate kappa with speed
# xlim, ylim: x and y axis limits within which to pick the starting point
# wrapped: whether to wrap the path
# OUTPUT:
# A list with elements:
# path: a dataframe with columns x and y (path co-ordinates) and, if wrap=TRUE, breaks indicating where wrap breaks occur
# turn, absturn: radian (absolute) turn angles for each step (turn ranging 0 to 2pi; absturn ranging 0 to pi)
# speed: step speeds
pathgen <- function(n, kTurn=0, Mb, speedCor=0, kCor=TRUE, pTurn=1, xlim=c(0,0), ylim=xlim, wrapped=TRUE){
  # set fixed logspeedSD (calculated using regent's park & panama data)
  logspeedSD <- 0.8546151
  
  # set logspeed - using body mass relationship derived from regent's park & panama data (fitting lnorm)
  logspeed <- log(0.010067*(Mb^0.1972)) 
  
  # set maxspeed - using body mass relationship from Garland 1983
  vmax <- (30.08292*(Mb^0.25892))/(Mb^(0.06237*log10(Mb)))
  
  spds <- exp(rautonorm(n, logspeed, logspeedSD, speedCor)) # generates set of autocorrelated variates
  
  # cap those spds at the max speed
  spds <- spds[spds<vmax]
  
  # set new number of steps based on how may speeds you now have:
  n_capped <- length(spds)
  
  # exp bc: the speed chunks we see tend to be log normally distributed
  # so you're generating a normal distribution of variates on the log scale (using logspeed)
  # so take exp to get them back to linear scale
  tTurn <- rbinom(n_capped,1,pTurn) # generates set of n (= no of steps) numbers either 1 and 0 where higher probability of turning at each step = more likely to have 1
  if(kCor==TRUE){ # if we want to correlate kappa with speed:
    kappas <- kTurn * spds / mean(spds)
    deviates <- sapply(kappas, function(x) as.numeric(rvonmises(1,circular(0),x)))
  } 
  else 
      deviates <- as.numeric(rvonmises(n_capped, circular(0), kTurn)) # get one turning number per speed - must be some sort of turning number corresponding to each speed so that speed change and turning are correlated
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


# model for small species' radius: hazard rate with logistic mix
small_radius <- function(radius){
  prob <- (1 - exp(-(1.266202/radius)^1.882447))/(1 + exp(2.604066*(1.401516 - radius)))
  if (prob > 1){
    prob <- 1
  }
  if (prob < 0){
    prob <- 0
  }
  return(prob)
}

# model for large species' radius: hazard rate with logistic mix
large_radius <- function(radius){
  prob <- (1 - exp(-(3.3509736/radius)^6.3920311))/(1 + exp(0.9969682*(3.3422355 - radius)))
  if (prob > 1){
    prob <- 1
  }
  if (prob < 0){
    prob <- 0
  }
  return(prob)
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
  dist <- with(dat, tapply(distance, sequenceID, sum, na.rm=TRUE)) # for those that have the same sequence ID, sum their distances to get total distance travelled within the detection zone
  points <- with(dat, tapply(distance, sequenceID, length)) # number of points in the sequence
  speed <- dist/(points-1) # time = n_points-1 bc time between each point is 1s
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

## decimalplaces
# find number of decimal places in a number (from https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r)
decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}


## kl_div_calc
# work out KL divergence of obs_y from real_y (y values for their PDFs)
kl_div_calc <- function(real_y, obs_y){
  for (i in 1:length(real_y)){
    kl <- sum(obs_y[i]*log(real_y[i]/obs_y[i]))
  }
  return(kl)
}

## log_pdf_calc
# work out the log ratio of obs_y from real_y
log_pdf_calc <- function(real_y, obs_y){
  for (i in 1:length(real_y)){
    log_ratio <- log(real_y[i]/obs_y[i])
  }
  return(log_ratio)
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


#Whether a point is on a line (given that orientation is colinear)
point_on_line <- function(ln1, ln2, pt){
  minx <- apply(cbind(ln1[,1], ln2[,1]), 1, min)
  miny <- apply(cbind(ln1[,2], ln2[,2]), 1, min)
  maxx <- apply(cbind(ln1[,1], ln2[,1]), 1, max)
  maxy <- apply(cbind(ln1[,2], ln2[,2]), 1, max)
  pt[,1]>=minx & pt[,2]>=miny & pt[,1]<=maxx & pt[,2]<=maxy
}

#https://www.geeksforgeeks.org/how-to-check-if-a-given-point-lies-inside-a-polygon/
#INPUT
# point, poly: two column arrays of co-ordinates (x,y in that order) defining points and a polygon
#              polygon co-ordinates need not be closed (first and last the same)
point_in_poly <- function(point, poly){
  lns.pnt <- cbind(point, min(poly[,1]-1), point[,2])
  lns.ply <- cbind(poly, rbind(tail(poly, -1), head(poly, 1)))
  cross <- lines_cross(lns.pnt, lns.ply)
  ncross <- apply(cross, 1, sum)
  ncross %% 2 == 1
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



## reassign_prob
# for all the points that cross the dz, reassign them as TRUE or FALSE based on probability of getting detected at that distance from the CT
# INPUTS
# a row in isindz_df2 dataframe from isindz function
# OUTPUT
# vector of TRUE or FALSE for whether that point got detected
reassign_prob <- function(isindz_row){
  if (isindz_row[[1]]==FALSE){
    return(FALSE)
  }
  else {
    # if (species == 0){ - only use large species hazard function for now
    #   prob_radius <- small_radius(as.numeric(isindz_row[[2]])) * 3.340884 # probability of being detected based on the estimated probability density for the radius
    #   if (prob_radius > 1){ # need this in here for some reason - not enough to just have it in small_radius and large_radius functions
    #     prob_radius <- 1
    #   }
    #   new_res <- sample(c(TRUE,FALSE), 1, prob = c(prob_radius, 1-prob_radius)) # generate either TRUE or FALSE with prob of getting TRUE = prob of being detected and replace this in the main df
    #   return(new_res)
    # }
    # if (species == 1){
      prob_radius <- large_radius(as.numeric(isindz_row[[2]])) * 2.767429 # probability of being detected based on the estimated probability density for the radius
      if (prob_radius > 1){
        prob_radius <- 1
      }
      new_res <- sample(c(TRUE,FALSE), 1, prob = c(prob_radius, 1-prob_radius)) # generate either TRUE or FALSE with prob of getting TRUE = prob of being detected and replace this in the main df
      return(new_res)
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
# species = size of the animal (1 = large, 0 = small)
# OUTPUT
# A logical array defining whether each point (rows) is in each detection zone (columns)
is_in_dz <- function(point, dzone){
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
  
  # now for the ones which are true: reassign them as true based on probability density
  # model for large species' angle: normal --> get rid of this for now
  # large_angle <- function(angle){
  #   dnorm(angle, mean = 0.01114079, sd = 0.21902793)
  # }
  
  new_reses <- apply(isindz_df, 1, reassign_prob)
  
  isindz_all <- data.frame(indz = isindz_df$res,
                           detected = new_reses)
  return(isindz_all)
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
# a detection zone array
# species = size of the animal (1 = large, 0 = small)
# OUTPUT
# A data frame with columns:
# x,y: x,y co-ordinates of sequence points in detection zones
# sequenceID: integer sequence identifier
# distance: distance traveled for each step between points
# for small species:
sequence_data <- function(pth, dzone){
  pth <- pth$path[, c("x","y")] # format path into df with sequence of x and y
  isin_all <- is_in_dz(pth, dzone) # returns true or false for whether each position in the path is in the detection zone
  
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
  
  # df2["detected"] <- NA
  # for (i in 1:nrow(df2)){ # THIS TAKES THE LONGEST
  #   d <- df2[i,]
  #   if (d$x %in% df1$x & d$y %in% df1$y){ # if x and y coords are in df1, detected = TRUE
  #     df2[i,]$detected <- TRUE
  #   }
  #   else{
  #     df2[i,]$detected <- FALSE
  #   }
  # }
  
  df2["detected"] <- FALSE
  df2$detected[which(df2$x %in% df1$x & df2$y %in% df1$y)] <- TRUE
  
  
  return(df2)
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
# vector of values for whether each pair of points in paired_points is a zero-frame (i.e. whether the animal crossed the dz without getting detected when moving between the two points)
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
      prob_detect <- large_radius(midpoint_radius) * 2.767429 # just use the hazard rate function (without logistic mix) bc not worrying about species rn
      zero <- prob_detect
    }
    else{
      zero <- 0
    }
  }
  return(zero)
}



# run_simulation
# runs the simulation: generates a path and dz, then position data, then observed speeds of each sequence (sequence = one path which crosses the CT dz and is captured at at least 2 points)
# INPUTS:
# path = path generated by pathgen function using the HPC
# folder = where to save the path to
# species = size of the animal (1 = large, 0 = small)
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
run_simulation <- function(path, parentfolder, pathfolder, r, th, plot_path = TRUE, twoCTs = FALSE, connectedCTs = FALSE){
  
  ##### generate speed sequences ################################################################################################################################################
  if (twoCTs == FALSE){
    dz <- data.frame(x=20, y=10, r=r, th=th, dir=0) # initially set radius to 10m and theta to 1.65 - based on distributions of radii & angles in regent's park data -- then M & C said angle isn't usually more than 1 so set to 1
    posdat_all <- sequence_data(path, dz) # posdat_all == all of those that fell in the detection zone (+ column saying whether or not it got detected)
  }
  if (twoCTs == TRUE){
    dz1 <- data.frame(x=12, y=5, r=r, th=th, dir=0)
    if (connectedCTs == TRUE){
      dz2 <- data.frame(x = (dz1[1,1] + r*sin(th)), y = (dz1[1,2] + r*cos(th)), r = r, th = th, dir = 1) # place it directly next to the other CT
    }
    if (connectedCTs == FALSE){ 
      dz2 <- data.frame(x=27, y=25, r=r, th=th, dir=0)
    }
    posdat_all1 <- sequence_data(path, dz1) # posdat_all == all of those that fell in the detection zone (+ column saying whether or not it got detected)
    posdat_all2 <- sequence_data(path, dz2)
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
  
  # ### speeds of single-frame sequences and number of single-frame sequences - incorrect bc need to use the whole path df -- re-doing this in multispeed_analyse function
  # singles_speeds <- c()
  # n_singles <- 0
  # # select rows in posdat_all which are detected but only in a single frame, as well as the two points above and below each one of those
  # # select the seqID posdats when you only have one TRUE:
  # for (i in unique(posdat_all$sequenceID)){
  #   p <- posdat_all[posdat_all$sequenceID==i,] # subset by sequence ID
  #   n_true <- nrow(p[p$detected==TRUE,])# subset by TRUE for detection to count the number of points detected by the CT in this sequence
  #   if (n_true == 1){ # if there is only one point detected by the CT (i.e. it's a single frame)
  #     
  #     ## work out the detection probability of that single frame:
  #     single_x <- p[p$detected==TRUE,]$x # select the x and y coords of the detected single point
  #     single_y <- p[p$detected==TRUE,]$y
  #     single_radius <- sqrt((single_y-dz$y)^2 + (single_x-dz$x)^2) # work out radius (distance from CT)
  #     if (species == 0){
  #       prob_detect <- small_radius(single_radius) * 3.340884 # probability of detection if the species is small
  #     }
  #     if (species == 1){
  #       prob_detect <- large_radius(single_radius) * 2.767429 # probability of detection if the species is large
  #     }
  #     n_singles <- n_singles + prob_detect # add that to the number of single frames (so that it's a value that's taken probabilistic stuff into account)
  #     
  #     
  #     ## select the row that's TRUE and the row above and below it -- incorrect!!
  #     # assign row numbers to help with this
  #     p["rownumber"] <- c(1:nrow(p))
  #     true_rownumber <- p[p$detected==TRUE,]$rownumber # rownumber of the detected point
  #     if (true_rownumber == 1){ # if the detected point is the first in that sequence, just use that point and the one after it
  #       below_rownumber <- true_rownumber + 1
  #       rownumbers_needed <- c(true_rownumber, below_rownumber)
  #     }
  #     if (true_rownumber == nrow(p)){ # if the detected point is the last in that sequence, just use that point and teh one before it
  #       above_rownumber <- true_rownumber - 1
  #       rownumbers_needed <- c(true_rownumber, above_rownumber)
  #     }
  #     else { # otherwise, use both the points below and above
  #       above_rownumber <- true_rownumber - 1
  #       below_rownumber <- true_rownumber + 1
  #       rownumbers_needed <- c(true_rownumber, above_rownumber, below_rownumber)
  #     }
  #     rows_needed <- p[rownumbers_needed,]
  #     speed_single <- calc_speed(rows_needed)
  #     singles_speeds <- c(singles_speeds, speed_single$speed)
  #   } 
  # }
  # singles_speeds <- singles_speeds[is.finite(singles_speeds)==TRUE] # get rid of the NaNs - occur when the edge of the arena is reached
  # 
  # ### number of zero-frame sequences: -- also incorrect - bc of the realised speeds you're using here (my previous wrong way of doing it) -- re-doing this in multispeed analyse
  # path_df <- path$path
  # path_df2 <- path_df
  # path_df <- path_df[-nrow(path_df),] # remove last row
  # path_df2 <- path_df2[-1,] # remove first row
  # path_df_paired <- cbind(path_df, path_df2) # paired points
  # colnames(path_df_paired) <- c("x1", "y1", "breaks1", "x2", "y2", "breaks2")
  # max_real <- max(realised_spds) # max realised speed in this simulation run (used for buffer)
  # if (twoCTs == FALSE){
  #   dz <- data.frame(x=20, y=10, r=r, th=th, dir=0)
  #   zeros <- future_apply(path_df_paired, 1, zero_frame, dz = dz, posdat_all = posdat_all, max_real = max_real)
  # }
  # if (twoCTs == TRUE){
  #   dz1 <- data.frame(x=12, y=5, r=r, th=th, dir=0)
  #   if (connectedCTs == TRUE){
  #     dz2 <- data.frame(x = (dz1[1,1] + r*sin(th)), y = (dz1[1,2] + r*cos(th)), r = r, th = th, dir = 1) # place it directly next to the other CT
  #   }
  #   if (connectedCTs == FALSE){ 
  #     dz2 <- data.frame(x=27, y=25, r=r, th=th, dir=0)
  #   }
  #   zeros1 <- future_apply(path_df_paired, 1, zero_frame, dz = dz1, posdat_all = posdat_all, max_real = max_real)
  #   zeros2 <- future_apply(path_df_paired, 1, zero_frame, dz = dz2, posdat_all = posdat_all, max_real = max_real)
  #   zeros <- c(zeros1, zeros2)
  # }
  # zeros_vals <- zeros[zeros!=0]
  # n_zeros <- sum(zeros_vals[1:length(zeros_vals)])
  # 
  # 
  # ## speeds of zero-frame sequences
  # # need to get the coords in the right format to call calc_speed - do this once have got the zeros run
  # path_df_paired["ZERO"] <- zeros
  # zeros_dat <- path_df_paired[path_df_paired$ZERO!=0,] # dataframe with only pairs of points which make a zero frame
  # zeros_speeds <- c()
  # for (i in 1:nrow(zeros_dat)){
  #   z <- zeros_dat[i,]
  #   speed <- sqrt((z$y2-z$y1)^2 + (z$x2-z$x1)^2) # speed = distance between the two points bc timestep = 1s
  #   zeros_speeds <- c(zeros_speeds, speed)
  # }
  # 
  # make output list
  output_list <- list(posdat_all = posdat_all,
                      posdat = posdat,
                      v = v,
                      realised = realised_spds,
                      observed = observed,
                      obs_lengths = obs_lengths,
                      # n_singles = n_singles, -- these are all incorrect -- re-doing them in multispeed_analyse
                      # singles_speeds = singles_speeds,
                      # n_zeros = n_zeros,
                      # zeros_speeds = zeros_speeds,
                      n_points = nrow(posdat)) # total number of position datapoints detected by the CT
                      # singles_prop = n_singles/nrow(posdat),
                      # zeros_prop = n_zeros/nrow(posdat))

  # plot path
  if (plot_path == TRUE){
    png(file= paste0(parentfolder, pathfolder, "plot.png"),
        width=700, height=650)
    plot_wrap(path, lineargs = list(col="grey"))
    plot_dzone(dz, border=2)
    points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
    dev.off()
  }
  
  return(output_list)
}


## generate_seqdats
# run the simulation on each of the 100 paths for each repeat and save the seq_dats outputs to the seq_dats folder
# INPUTS
# path_nos: range of iter numbers to run the simulation on (vary depending on computational ability of local machine) - e.g. course laptop can take about 10 at once max
# OUTPUT
# saved seq_dats.RData files in seq_dats folder
generate_seqdats <- function(parentfolder, pathfolder, path_nos, r, th, twoCTs, connectedCTs=FALSE, path_cutby = 1){
  for (i in path_nos){
    load(paste0(parentfolder, pathfolder, "iter", i, ".RData"))
    if (i == 1){
      plot_path <- TRUE # only plot the first one to save some time
    }
    else {
      plot_path <- FALSE
    }
    if (path_cutby == 1){
      seq_dats <- run_simulation(path, parentfolder=parentfolder, pathfolder=pathfolder, r=r, th=th, plot_path=plot_path, twoCTs=twoCTs, connectedCTs=connectedCTs)
    }
    else { # if want to cut the path short to make it computationally easier:
      path <- list(path$path[1:(500000*path_cutby+1),], path$turn[1:500000*path_cutby], path$absturn[1:500000*path_cutby], path$speed[1:500000*path_cutby])
      seq_dats <- run_simulation(path, parentfolder=parentfolder, pathfolder=pathfolder, r=r, th=th, plot_path=plot_path, twoCTs=twoCTs, connectedCTs=connectedCTs)
    }
    metadata_sim <- list(datetime = metadata$datetime,
                         iter = metadata$iter,
                         Mb = metadata$Mb,
                         xlim = metadata$xlim,
                         step_no = metadata$step_no,
                         #speedSD = metadata$speedSD,
                         pTurn = metadata$pTurn,
                         speedCor = metadata$speedCor,
                         kTurn = metadata$kTurn,
                         kCor = metadata$kCor,
                         r = r,
                         th = th,
                         twoCTs = twoCTs,
                         connectedCTs = connectedCTs)
    
    save(seq_dats, metadata_sim, file = paste0(parentfolder, "../seq_dats/Mb", metadata_sim$Mb, "iter", i, ".RData"))
    rm(list = c("path", "seq_dats", "metadata", "metadata_sim"))
  }
}






## obs_meanreal_error_calc
# work out error between an observed speed and the mean realised speed for that simulation run
# INPUTS:
# observed = an observed speed
# mean_real = mean realised speed for the corresponding simulation run
# OUTPUT:
# error between the observed speed and mean realised speed (positive = observed speed is greater than mean realised speed)
obs_meanreal_error_calc <- function(observed, mean_real){
  error <- observed - mean_real
  return(error)
}




## estimates_calc
# for one simulation iteration, works out estimated speeds (using hmean and 3 SBMs) and error between mean realised speed & each estimated speed
# INPUT:
# seq_dats: list of seq_dat outputs for each iteration
# each iteration output is a list containing:
# realised speeds
# observed speeds
# lengths of observed speed sequences 
# number of single frames
# speeds of single frame sequences
# number of zero frames
# speeds of zero frame sequences
# total number of position datapoints recorded
# proportion of single frames (just divided by total no. of datapoints)
# proportion of zero frames (ditto)
# n_cores = number of cores on your laptop - to parallelise sappply
# OUTPUTS:
# list containing:
# mean realised speed
# error between each observed speed and the mean realised speed
# hmean estimated speed
# lognormal estimated speed
# gamma estimated speed
# Weibull estimated speed
# error between each estimated speed and mean realised speed (estimated speed - mean realised speed)
estimates_calc <- function(seq_dats){
  realised <- seq_dats$realised

  observed <- seq_dats$observed
  observed <- observed[is.finite(observed)]

  mMOS_wMRS_error1 <- sapply(observed, obs_meanreal_error_calc, mean_real = wMRS)
  mMOS_wMRS_error1_sz <- sapply(observed_sz, obs_meanreal_error_calc, mean_real = wMRS) # for observed speeds including single & zero-frame speeds
  # 
  # hmean <- (hmean_calc(observed))[1] # harmonic mean estimate
  # hmean_sz <- (hmean_calc(observed_sz))[1]
  # obs_df <- data.frame(speed = observed)
  # obs_df_sz <- data.frame(speed = observed_sz)
  # mods <- sbm3(speed~1, obs_df) # fit all the models
  # mods_sz <- sbm3(speed~1, obs_df_sz)
  # lnorm <- predict.sbm(mods[[1]]$lnorm)[1,1] # lnorm estimate
  # lnorm_sz <- predict.sbm(mods_sz[[1]]$lnorm)[1,1]
  # gamma <- predict.sbm(mods[[1]]$gamma)[1,1] # gamma estimate
  # gamma_sz <- predict.sbm(mods_sz[[1]]$gamma)[1,1]
  # weibull <- predict.sbm(mods[[1]]$weibull)[1,1] # weibull estimate
  # weibull_sz <- predict.sbm(mods_sz[[1]]$weibull)[1,1]
  
  h_wMRS_error <- hmean - wMRS
  h_mMOS_error <- hmean - mMOS
  h_wMRS_error_sz <- hmean_sz - wMRS
  h_mMOS_error_sz <- hmean_sz - mMOS
  l_wMRS_error <- lnorm - wMRS
  l_mMOS_error <- lnorm - mMOS
  l_wMRS_error_sz <- lnorm_sz - wMRS
  l_mMOS_error_sz <- lnorm_sz - mMOS
  g_wMRS_error <- gamma - wMRS
  g_mMOS_error <- gamma - mMOS
  g_wMRS_error_sz <- gamma_sz - wMRS
  g_mMOS_error_sz <- gamma_sz - mMOS
  w_wMRS_error <- weibull - wMRS
  w_mMOS_error <- weibull - mMOS
  w_wMRS_error_sz <- weibull_sz - wMRS
  w_mMOS_error_sz <- weibull_sz - mMOS
  
  output <- list(wMRS=wMRS, mMOS=mMOS, mMOS_sz=mMOS_sz, mMOS=mMOS, gmMOS=gmMOS, aMOS=aMOS, gMOS=gMOS,
                 mMOS_wMRS_error=mMOS_wMRS_error, mMOS_wMRS_error_sz=mMOS_wMRS_error_sz, 
                 hmean=hmean, hmean_sz=hmean_sz, lnorm=lnorm, lnorm_sz=lnorm_sz, gamma=gamma, gamma_sz=gamma_sz, weibull=weibull, weibull_sz=weibull_sz, 
                 h_wMRS_error=h_wMRS_error, h_mMOS_error=h_mMOS_error, h_wMRS_error_sz=h_wMRS_error_sz, h_mMOS_error_sz=h_mMOS_error_sz,
                 l_wMRS_error=l_wMRS_error, l_mMOS_error=l_mMOS_error, l_wMRS_error_sz=l_wMRS_error_sz, l_mMOS_error_sz=l_mMOS_error_sz, 
                 g_wMRS_error=g_wMRS_error, g_mMOS_error=g_mMOS_error, g_wMRS_error_sz=g_wMRS_error_sz, g_mMOS_error_sz=g_mMOS_error_sz,
                 w_wMRS_error=w_wMRS_error, w_mMOS_error=w_mMOS_error, w_wMRS_error_sz=w_wMRS_error_sz, w_mMOS_error_sz=w_mMOS_error_sz) 
  return(output)
}



## generate_plotting_variables
# analyses simulation results from multiple different speed parameters to make summary plots
# INPUTS
# Mb_iters - dataframe of body masses to analyse and number of iters of each to use
# r: radius of CT detection zone
# th: angle of CT detection zone
# twoCTs: whether or not to use two CTs
# connectedCTs: whether or not the two CTs are set up in a connected way such that the detection zones are triangles side-by-side facing opposite ways (hence maximising their area of contact and making one large rectangular-ish shaped dz)
# OUTPUT
# big list of variables which will be used for plotting
generate_plotting_variables <- function(Mb_iters, r, th, twoCTs=FALSE, connectedCTs=FALSE){
  
  ## store variables from the .RData files
  
  # wMRS <- c() # my original (wrong) method of working out MRS
  aMRS <- c() # arithmetic MRS
  # gMRS <- c() # geometric MRS
  
  amMOS <- c() # arithmetic mean of arithmetic mean speeds of sequences
  amMOS_sz <- c() # with singles & zeros
  # gmMOS <- c() # geometric mean of geometric mean speeds of sequences
  # gmMOS_sz <- c() # with singles & zeros
  apMOS <- c() # arithmetic mean of point-to-point speeds regardless of sequence
  apMOS_sz <- c() # with singles & zeros
  # gMOS <- c() # geometric mean of point-to-point speeds regardless of sequence
  # gMOS_sz <- c() # with singles & zeros
  
  hmean_m <- c() # calculated using M's way of working out observed speeds
  hmean_m_sz <- c() # including singles & zeros too
  hmean_p <- c() # calculated using point-to-point observed speeds
  hmean_p_sz <- c() # including singles & zeros too
  
  lnorm_m <- c() # same as for hmean
  lnorm_m_sz <- c()
  lnorm_p <- c()
  lnorm_p_sz <- c()
  
  gamma_m <- c() # ditto
  gamma_m_sz <- c()
  gamma_p <- c()
  gamma_p_sz <- c()
  
  weibull_m <- c() # ditto
  weibull_m_sz <- c()
  weibull_p <- c()
  weibull_p_sz <- c()
  
  n_zeros <- c()
  n_singles <- c()
  # singles_speeds <- c()
  singles_v_mean <- c()
  # zeros_speeds <- c()
  zeros_v_mean <- c()
  

  
  ## loop to fill these variables #############################################################################################################################
  
  for (n in 1:length(Mb_iters$Mb_range)){
    i <- Mb_iters$Mb_range[n]
    iter_range <- c(1:Mb_iters[Mb_iters$Mb_range==i,]$iter)
    for (j in iter_range){
      i <- Mb_iters$Mb_range[n]
      
      ## load in the path and seq_dats for that simulation run #####################################################################################
      
      load(paste0("../Mb_results/seq_dats/Mb", i, "iter", j, ".RData"))
      
      load(paste0("../Mb_results/paths_30Jun22_1727/Mb", i, "/iter", j, ".RData"))
      
      
      ## number of single frames and speeds of single frame sequences #######################################################################
      
      # make df with all xy coords of the path and their speeds:
      path_xy_v <- data.frame(x = path$path$x[-1], # no speed for the first point
                              y = path$path$y[-1],
                              v = path$speed)
      
      # store posdat_all as a df too:
      posdat_all <- seq_dats$posdat_all
      
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
          prob_detect <- large_radius(single_radius) * 2.767429 # probability of detection using hazard function
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
      
      singles_v_mean <- c(singles_v_mean, mean(singles_v)) # store outside of the main loop
      n_singles <- c(n_singles, singles_count)  # ditto
      
      
      ## number of zero frames and speeds of zero frame sequences ######################################################################################
      
      ### number of zero-frame sequences:
      path_df <- path$path
      path_df2 <- path_df
      path_df <- path_df[-nrow(path_df),] # remove last row
      path_df2 <- path_df2[-1,] # remove first row
      path_df_paired <- cbind(path_df, path_df2) # paired points
      colnames(path_df_paired) <- c("x1", "y1", "breaks1", "x2", "y2", "breaks2")
      max_real <- max(path$speed) # max realised speed in this simulation run (used for buffer)
      if (twoCTs == FALSE){
        dz <- data.frame(x=20, y=10, r=r, th=th, dir=0)
        zeros <- future_apply(path_df_paired, 1, zero_frame, dz = dz, posdat_all = posdat_all, max_real = max_real)
      }
      if (twoCTs == TRUE){
        dz1 <- data.frame(x=12, y=5, r=r, th=th, dir=0)
        if (connectedCTs == TRUE){
          dz2 <- data.frame(x = (dz1[1,1] + r*sin(th)), y = (dz1[1,2] + r*cos(th)), r = r, th = th, dir = 1) # place it directly next to the other CT
        }
        if (connectedCTs == FALSE){ 
          dz2 <- data.frame(x=27, y=25, r=r, th=th, dir=0)
        }
        zeros1 <- future_apply(path_df_paired, 1, zero_frame, dz = dz1, posdat_all = posdat_all, max_real = max_real)
        zeros2 <- future_apply(path_df_paired, 1, zero_frame, dz = dz2, posdat_all = posdat_all, max_real = max_real)
        zeros <- c(zeros1, zeros2)
      }
      zeros_count <- sum(zeros[1:length(zeros)]) # add up all of the zero counts to get total number of zero frames in that simulation
      
      n_zeros <- c(n_zeros, zeros_count) # save externally to the main for loop
      
      ## speeds of zero-frame sequences
      path_df_paired["ZERO"] <- zeros
      zeros_dat <- path_df_paired[path_df_paired$ZERO!=0,] # dataframe with only pairs of points which make a zero frame
      zeros_v <- c() # to save outside of this mini for loop
      for (l in 1:nrow(zeros_dat)){
        z <- zeros_dat[l,]
        speed <- sqrt((z$y2-z$y1)^2 + (z$x2-z$x1)^2) # speed = distance between the two points bc timestep = 1s
        zeros_v <- c(zeros_v, speed)
      }

      zeros_v_mean <- c(zeros_v_mean, mean(zeros_v)) # store outside of main loop
      
      
      ## mean realised speeds ###################################################################################################################################
      
      # w_real <- seq_dats$realised # my initial wrong way of working out realised speeds (using selected chunks of the path of length equal to average obs sequence length)
      # wMRS <- c(wMRS, mean(w_real)) # my original way of working out MRS 
      
      p_real <- path$speed # point-to-point realised speeds
      aMRS <- c(aMRS, mean(p_real)) # arithmetic MRS
      # gMRS <- c(gMRS, exp(mean(log(p_real)))) # geometric mean
      
      
      ## mean observed speeds #################################################################################################################################
      
      m_obs <- seq_dats$observed # M's way of working out observed speeds
      m_obs <- m_obs[is.finite(m_obs)]
      m_obs_sz <- c(m_obs, singles_v, zeros_v) # M's way of working out observed speeds + single & zero frames
      
      amMOS <- c(amMOS, mean(m_obs)) 
      amMOS_sz <- c(amMOS_sz, mean(m_obs_sz))
      # gmMOS <- c(gmMOS, exp(mean(log(m_obs))))
      # gmMOS_sz <- c(gmMOS_sz, exp(mean(log(m_obs_sz))))
      
      p_obs <- seq_dats$posdat$distance # point-to-point observed speeds irrespective of sequence
      p_obs <- p_obs[is.finite(p_obs)]
      p_obs_sz <- c(p_obs, singles_v, zeros_v) # including singles & zeros too
      
      apMOS <- c(apMOS, mean(p_obs))
      apMOS_sz <- c(apMOS_sz, mean(p_obs_sz))
      # gMOS <- c(gMOS, exp(mean(log(p_obs))))
      # gMOS_sz <- c(gMOS_sz, exp(mean(log(p_obs_sz))))
      
      ## estimated speeds ###################################################################################################
      
      hmean_m <- c(hmean_m, (hmean_calc(m_obs))[1]) # harmonic mean estimate using M's observed speeds
      hmean_m_sz <- c(hmean_m_sz, (hmean_calc(m_obs_sz))[1]) # including singles & zeros
      hmean_p <- c(hmean_p, (hmean_calc(p_obs))[1]) # using raw point-to-point speeds
      hmean_p_sz <- c(hmean_p_sz, (hmean_calc(p_obs_sz))[1])
      
      obs_df_m <- data.frame(speed = m_obs)
      obs_df_m_sz <- data.frame(speed = m_obs_sz)
      obs_df_p <- data.frame(speed = p_obs)
      obs_df_p_sz <- data.frame(speed = p_obs_sz)
      
      mods_m <- sbm3(speed~1, obs_df_m) # fit all the models
      mods_m_sz <- sbm3(speed~1, obs_df_m_sz)
      mods_p <- sbm3(speed~1, obs_df_p)
      mods_p_sz <- sbm3(speed~1, obs_df_p_sz)
      
      lnorm_m <- c(lnorm_m, predict.sbm(mods_m[[1]]$lnorm)[1,1])
      lnorm_m_sz <- c(lnorm_m_sz, predict.sbm(mods_m_sz[[1]]$lnorm)[1,1])
      lnorm_p <- c(lnorm_p, predict.sbm(mods_p[[1]]$lnorm)[1,1])
      lnorm_p_sz <- c(lnorm_p_sz, predict.sbm(mods_p_sz[[1]]$lnorm)[1,1])
      
      gamma_m <- c(gamma_m, predict.sbm(mods_m[[1]]$gamma)[1,1])
      gamma_m_sz <- c(gamma_m_sz, predict.sbm(mods_m_sz[[1]]$gamma)[1,1])
      gamma_p <- c(gamma_p, predict.sbm(mods_p[[1]]$gamma)[1,1])
      gamma_p_sz <- c(gamma_p_sz, predict.sbm(mods_p_sz[[1]]$gamma)[1,1])
      
      weibull_m <- c(weibull_m, predict.sbm(mods_m[[1]]$weibull)[1,1])
      weibull_m_sz <- c(weibull_m_sz, predict.sbm(mods_m_sz[[1]]$weibull)[1,1])
      weibull_p <- c(weibull_p, predict.sbm(mods_p[[1]]$weibull)[1,1])
      weibull_p_sz <- c(weibull_p_sz, predict.sbm(mods_p_sz[[1]]$weibull)[1,1])
      
      ## filename for storing plots ###############################################################################################################################
      
      filename <- paste0("pTurn", metadata_sim$pTurn, # filename for storing plots
                         "_speedCor", metadata_sim$speedCor,
                         "_kTurn", metadata_sim$kTurn,
                         "_kCor", metadata_sim$kCor,
                         # "_species", metadata_sim$species,
                         "_twoCTs", metadata_sim$twoCTs,
                         "_connectedCTs", metadata_sim$connectedCTs)
      
      rm(list = c("seq_dats", "metadata_sim", "path"))
    }
  }
  
  output <- list(
    filename = filename,
    # wMRS = wMRS,
    aMRS = aMRS,
    # gMRS = gMRS,
    
    amMOS = amMOS,
    amMOS_sz = amMOS_sz,
    # gmMOS = gmMOS,
    # gmMOS_sz = gmMOS_sz,
    apMOS = apMOS,
    apMOS_sz = apMOS_sz,
    # gMOS = gMOS,
    # gMOS_sz = gMOS_sz,
    
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
    # singles_speeds = singles_speeds,
    singles_v_mean = singles_v_mean,
    # zeros_speeds = zeros_speeds,
    zeros_v_mean = zeros_v_mean

    # x_plot = x_plot,
    # y_plot = y_plot,
    # v_plot = v_plot,
    # Mb_plot = Mb_plot,
    # iter_plot = iter_plot,
    
    # mMOS_wMRS_error1 = mMOS_wMRS_error1, - for original plot - but don't need for now
    # mMOS_wMRS_error1_sz = mMOS_wMRS_error1_sz,
    # 
    # mMOS_wMRS_error1_lengths = mMOS_wMRS_error1_lengths,
    # mMOS_wMRS_error1_lengths_sz <- mMOS_wMRS_error1_lengths_sz
  )
  
  save(output, file = "../Mb_results/plotting_data.RData") # add sp range and number of iters too to the name of the output file
  
}

## make_vis_plot
# make visualisation plot for one rep of each simulation with a different body mass
# this is done separately from the other generation of variables (generate_plotting_variables function) and plotting (make_plots function) bc here only one rep of each body mass simulation is used rather than all of them
# INPUTS
# Mb_range: range of body masses to plot for
# r: radius of detection zone
# th: angle of dz
# twoCTs: whether or not to use two CTs
# connectedCTs: whether or not the two CTs are set up in a connected way such that the detection zones are triangles side-by-side facing opposite ways (hence maximising their area of contact and making one large rectangular-ish shaped dz)
make_vis_plot <- function(Mb_range, r, th, twoCTs=FALSE, connectedCTs=FALSE){
  
  # variables needed
  x_plot <- c() # x coord of a position data point falling in the dz
  y_plot <- c() # y coord of that position data point
  v_plot <- c() # speed of the sequence that the point is in
  Mb_plot <- c() # body mass associated with the simulation run that the point is in
  detected_ratio <- c() # ratio of detected to non-detected points to display on visualisation plot
  sz_ratio <- c() # ratio of no. of singles & zeros vs no. of sequences with 2 or more points
  
  
  for (i in Mb_range){

      ## load in the path and seq_dats for that simulation run #####################################################################################
      
      load(paste0("../Mb_results/seq_dats/Mb", i, "iter1.RData"))
      
      load(paste0("../Mb_results/paths_30Jun22_1727/Mb", i, "/iter1.RData"))
      
      ## coords and speeds for visualisation plot #############################################################################################################
      
      # store the coords of all points (including not detected ones) and the speed of the sequence they're associated with
      posdat_all <- seq_dats$posdat_all # all position data points and whether or not they were detected
      posdat_speed_col <- c() # new column with speed of each sequence ID repeated for the length of that sequence ID
      for (j in unique(posdat_all$sequenceID)){
        s <- seq_dats$v[seq_dats$v$sequenceID==j,]$speed # arithmetic mean speed of that sequence ID
        if (is.finite(s)==FALSE || length(s)==0){ # if s is NaN, Inf, or numeric(0) (these happen when there's one or fewer detected points in that sequence)
          s <- NA # assign it as NA so that it still gets put in the posdat_extra_col vector
        }
        p <- posdat_all[posdat_all$sequenceID==j,] # all the position data points with that sequence ID (both detected and not detected)
        posdat_speed_col <- c(posdat_speed_col, rep(s, times = nrow(p))) # in the new speed column: need to repeat the speed of that sequence ID as many times as there are points for that sequence ID
      }
      posdat_all["speed"] <- posdat_speed_col # add this as an extra column of speed of sequence associated with each point (with NA for if the sequence contained one or fewer detected points)
      
      
      ## speeds of single frames (single frame == when one point in a sequence gets detected)
      
      singles_seqIDs <- c() # store the sequence ID of each single frame
      singles_v <- c() # store the speed of each single frame
      
      # define detection zone:
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
      
      # make df with all xy coords of the path and their speeds:
      path_xy_v <- data.frame(x = path$path$x[-1], # no speed for the first point
                              y = path$path$y[-1],
                              v = path$speed)
      
      
      # select the seqID posdats when you only have one TRUE (i.e. single frame)
      for (k in unique(posdat_all$sequenceID)){
        p <- posdat_all[posdat_all$sequenceID==k,] # subset by sequence ID
        n_true <- nrow(p[p$detected==TRUE,])# subset by TRUE for detection to count the number of points detected by the CT in this sequence
        if (n_true == 1){ # if there's only one point detected by the CT (i.e. it's a single frame)
          singles_seqIDs <- c(singles_seqIDs, k) # store the sequence ID of this single frame
          
          # select the x and y coords of the detected single point
          single_x <- p[p$detected==TRUE,]$x 
          single_y <- p[p$detected==TRUE,]$y
          
          ## work out the speed of the single frame by using points above and below it in the whole-path data
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
          singles_v <- c(singles_v, speed_single) # store the speed of this single frame
        }
      }
      
      # add a column saying that these are single frames to the posdat_all df
      extra_singles_col <- c()
      for (l in unique(posdat_all$sequenceID)){
        p <- posdat_all[posdat_all$sequenceID==l,] # subset by sequence ID
        n_true <- nrow(p[p$detected==TRUE,])# subset by TRUE for detection to count the number of points detected by the CT in this sequence
        n_row <- nrow(p) # number of rows of p - need this in case there's only one row but it's not detected
        if (n_true == 1 || n_row == 1){ # if there's only one point detected by the CT (i.e. it's a single frame) or if there's only one point but it's not detected by the CT
        extra_singles_col <- c(extra_singles_col, rep("TRUE", times = nrow(p))) # in the new singles column: need to repeat TRUE (i.e. that it is a single frame) as many times as there are points for that sequence ID
        }
        else { # if it's not a single frame, do the same but with FALSE in that column instead
          extra_singles_col <- c(extra_singles_col, rep("FALSE", times = nrow(p)))
        }
      }
      posdat_all["single"] <- extra_singles_col # add this as an extra column of speed of sequence associated with each point (with NA for if the sequence contained one or fewer detected points)
      
      # also add the speeds of these single frames into the posdat_all df:
      singles_df <- data.frame(seqID = singles_seqIDs,
                               speed = singles_v)
      
      spds_with_singles_col <- c()
      for (m in unique(posdat_all$sequenceID)){
        p <- posdat_all[posdat_all$sequenceID==m,] # subset by sequence ID
        if (m %in% singles_df$seqID){ # if this sequence is a single frame
          v <- singles_df[singles_df$seqID==m,]$speed # extract the single frame speed for that sequence ID
          spds_with_singles_col <- c(spds_with_singles_col, rep(v, times = nrow(p))) # store that speed in the new speed column, repeated the number of times needed to fill out the empty speed cells for that whole sequence ID
        }
        else {
          spds_with_singles_col <- c(spds_with_singles_col, rep(p$speed[1], times = nrow(p))) # otherwise, just use the speed already there
        }
      }
      posdat_all$speed <- spds_with_singles_col # add this as the new column of speed of sequence associated with each point (with NA for if the sequence contained one or fewer detected points)
      
      
      

      ## work out speeds of frames where the point(s) fell in the dz but didn't get detected (need to use the same method as for singles for the single ones?)
      # these are in posdat_all[is.na(posdat_all$speed),]
      spds_with_extras_col <- c() # new speeds column with the added speeds of those points
      
      for (n in unique(posdat_all$sequenceID)){
        p <- posdat_all[posdat_all$sequenceID==n,] # subset by sequence ID
        if (is.na(p$speed)){ # if the speed is NA (bc there were no points at all detected in that sequence)
          if (nrow(p)==1){ # if it's also a single frame, need to use the same method as for single frames to work out speed
            # need to work out v using single-frames method
            
            
            spds_with_extras_col <- c(spds_with_extras_col, rep(v, times = nrow(p)))
          }
          else { # if it's not a single frame, can just work out speed using speed = dist/time
            v <- (sum(na.omit(p$distance)))/(nrow(p)-1)
            spds_with_extras_col <- c(spds_with_extras_col, rep(v, times = nrow(p)))
          }
        }
        else { # otherwise, just use the speed already there
          spds_with_extras_col <- c(spds_with_extras_col, rep(p$speed[1], times = nrow(p)))
        }
      }
      
      ## GO FROM HERE - MIGHT NEED TO FINISH THIS LOOP AND IF NOT START ON ZERO FRAMES
      
      ## speeds of zero frames - need to work out both the coords of these zero frames and their speed (and add both as an extra row in the df)
      
      
      
      
      
      
      
      
      
      
      x_plot <- c(x_plot, posdat$x)
      y_plot <- c(y_plot, posdat$y)
      v_plot <- c(v_plot, posdat$speed)
      Mb_plot <- c(Mb_plot, rep(i, times = length(posdat$x)))
    }
  
  ## visualisation plot 
  vis_df <- data.frame(x = x_plot,
                       y = y_plot,
                       speed = v_plot,
                       sp = sp_plot,
                       iter = iter_plot)
  
  # add column for whether or not it's a single frame:
  vis_df$speed[is.infinite(vis_df$speed)] <- NA
  vis_df$speed[is.nan(vis_df$speed)] <- NA
  new_cols <- apply(vis_df, 1, vis_df_newcols)
  single_col <- sapply(new_cols,"[[", 1)
  x_new <- sapply(new_cols, "[[", 2)
  y_new <- sapply(new_cols, "[[", 3)
  vis_df["single"] <- single_col
  vis_df["x_new"] <- x_new
  vis_df["y_new"] <- y_new
  
  vis_df2 <- vis_df
  vis_df3 <- vis_df2[vis_df2$iter==1,]
  vis_df4 <- vis_df3[vis_df3$single=="multiple",]
  vis_df5 <- vis_df3[vis_df3$single=="single",]
  
  vis_plot <- ggplot()+
    geom_point(data = vis_df4, aes(x = x_new, y = y_new, colour = speed), shape = 1)+
    # facet_grid(vars(sp))+
    facet_wrap(~ sp, ncol = 3)+
    # scale_colour_distiller(direction = 1)+
    # scale_colour_gradient(low = "#56B4E9", high = "dark blue", na.value = NA)+
    scale_colour_gradient(low = "#66CCFF", high = "dark blue", na.value = NA)+
    # scale_shape_manual(values=c(1, 16))+
    # scale_colour_manual(values = c("#0000FF", "#FF0000"))+ # generated using wheel("blue, 3)
    theme_minimal()+
    labs(x = "x", y = "y",
         title = "Locations of position data points for one run\nof a simulation of a given speed parameter")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          title = element_text(size = 13),
          panel.grid.minor = element_blank(),
          legend.title = element_text("Speed (m/s)"),
          plot.title = element_text(hjust = 0.5))+
    
    new_scale_colour()+
    geom_point(data = vis_df5, aes(x = x_new, y = y_new, colour = "single"), shape = 1)+
    scale_colour_manual(name = "",
                        breaks = c("single"),
                        values = c("red"))+
    guides(colour = guide_legend(override.aes = list(size=5)))+
    theme(legend.text = element_text(size = 12))
  vis_plot
  
  png(file=paste0("../results/PLOTS/visualisation_sp", sp_and_iters$speed_parameter[1], "-", sp_and_iters$speed_parameter[nrow(sp_and_iters)], ".png"),
      width=900, height=650)
  print(vis_plot)
  dev.off()
  
  
  # --> need to make changes to this that C suggested though
  
  
  
  
  
}


## vis_df_newcols
# to help make the visualisation plot
# INPUT
# dataframe of points to which in which the column needs to be filled
# OUTPUT
# list containing three new vectors of values for the three new columns: x_new, y_new, and single
vis_df_newcols <- function(df){
  if (is.na(df[[3]])){ # if the speed is Inf or NaN, it means it's a single frame
    single_col <- "single"
  }
  else{
    single_col <- "multiple"
  }
  
  x_new <- as.numeric(df[[1]]) - 20 # so that the centre of the x scale is 0
  y_new <- as.numeric(df[[2]]) - 10 # so that the y scale starts at 0
  
  output <- list(single_col = single_col,
                 x_new = x_new,
                 y_new = y_new)
  
  return(output)
}

## make_plots
# make summary plots using data generated in generate_plotting_variables function
make_plots <- function(){
  
  # load in data generated in generate_plotting_variables function
  load("../results/plotting_data.RData")
  
  ## NO. OF SINGLES & ZEROS AND SPEEDS OF SINGLES & ZEROS AGAINST aMRS ################################################################################################################################################

  ## no. of singles & no. of zeros against aMRS
  n_sz_aMRS_df <- data.frame(aMRS = aMRS,
                             count = c(n_singles, n_zeros),
                             type = c(rep("single", times = 250), rep("zero", times = 250)))
  n_sz_aMRS_plot <- ggplot(n_sz_aMRS_df, aes(x = aMRS, y = count, colour = type))+
    geom_point()+
    geom_smooth(alpha = 0.1)+
    theme_minimal()+
    # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+ # using wheel("red", 5) from colortools package
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 15),
          title = element_text(size = 13))+
    labs(x = "mean realised speed (m/s)",
         y = "count",
         title = "Number of single and zero frame sequences\nfor different mean realised speeds")
  # n_sz_aMRS_plot
  
  ## mean speeds of single & zero frame sequences
  speeds_sz_aMRS_df <- data.frame(aMRS = aMRS,
                                  speed = c(singles_speeds_mean, zeros_speeds_mean),
                                  type = c(rep("single", times = 250), rep("zero", times = 250)))
  speeds_sz_aMRS_plot <- ggplot(speeds_sz_aMRS_df, aes(x = aMRS, y = speed, colour = type))+
    geom_point()+
    geom_smooth(alpha = 0.1)+
    theme_minimal()+
    # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+ # using wheel("red", 5) from colortools package
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 15),
          title = element_text(size = 13))+
    labs(x = "mean realised speed (m/s)",
         y = "mean speed (m/s)",
         title = "Mean speeds of single and zero frame sequences\nfor different mean realised speeds")
  # speeds_sz_aMRS_plot
  
  sz_aMRS_arranged <- ggarrange(n_sz_aMRS_plot, speeds_sz_aMRS_plot, nrow = 2)
  
  png(file=paste0("../results/PLOTS/sz/aMRS_sp", sp_and_iters$speed_parameter[1], "-", sp_and_iters$speed_parameter[nrow(sp_and_iters)], ".png"),
      width=700, height=1000)
  print(sz_aMRS_arranged)
  dev.off()
  
  
  ## no. of singles & no. of zeros against gMRS
  n_sz_gMRS_df <- data.frame(gMRS = gMRS,
                             count = c(n_singles, n_zeros),
                             type = c(rep("single", times = 250), rep("zero", times = 250)))
  n_sz_gMRS_plot <- ggplot(n_sz_gMRS_df, aes(x = gMRS, y = count, colour = type))+
    geom_point()+
    geom_smooth(alpha = 0.1)+
    theme_minimal()+
    # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+ # using wheel("red", 5) from colortools package
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 15),
          title = element_text(size = 13))+
    labs(x = "mean realised speed (m/s)",
         y = "count",
         title = "Number of single and zero frame sequences\nfor different mean realised speeds")
  # n_sz_gMRS_plot
  
  ## mean speeds of single & zero frame sequences
  speeds_sz_gMRS_df <- data.frame(gMRS = gMRS,
                                  speed = c(singles_speeds_mean, zeros_speeds_mean),
                                  type = c(rep("single", times = 250), rep("zero", times = 250)))
  speeds_sz_gMRS_plot <- ggplot(speeds_sz_gMRS_df, aes(x = gMRS, y = speed, colour = type))+
    geom_point()+
    geom_smooth(alpha = 0.1)+
    theme_minimal()+
    # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+ # using wheel("red", 5) from colortools package
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 15),
          title = element_text(size = 13))+
    labs(x = "mean realised speed (m/s)",
         y = "mean speed (m/s)",
         title = "Mean speeds of single and zero frame sequences\nfor different mean realised speeds")
  # speeds_sz_gMRS_plot
  
  sz_gMRS_arranged <- ggarrange(n_sz_gMRS_plot, speeds_sz_gMRS_plot, nrow = 2)
  
  png(file=paste0("../results/PLOTS/sz/gMRS_sp", sp_and_iters$speed_parameter[1], "-", sp_and_iters$speed_parameter[nrow(sp_and_iters)], ".png"),
      width=700, height=1000)
  print(sz_gMRS_arranged)
  dev.off()
  
  
  
  
  
  
  ### wMRS_mMOS1_mEST_e1 PLOTS ###################################################################################################################################################################################
  
  wMRS_repped <- c()
  for (x in 1:length(wMRS)){
    wMRS_repped <- c(wMRS_repped, rep(wMRS[x], mMOS_wMRS_error1_lengths[x]))
  }
  
  wMRS_repped_sz <- c()
  for (x in 1:length(wMRS)){
    wMRS_repped_sz <- c(wMRS_repped_sz, rep(wMRS[x], mMOS_wMRS_error1_lengths_sz[x]))
  }
  
  mMOS_wMRS_repped_df <- data.frame(wMRS = wMRS_repped, # for working out mMOS_wMRS_error1_mean
                            error = mMOS_wMRS_error1)
  mMOS_wMRS_sz_repped_df <- data.frame(wMRS = wMRS_repped_sz, # for working out mMOS_wMRS_error1_mean_sz
                            error = mMOS_wMRS_error1_sz)

  mMOS_wMRS_error1_mean <- c() # mean of each MRS set of errors:
  for (i in unique(mMOS_wMRS_repped_df$wMRS)){
    d <- mMOS_wMRS_repped_df[mMOS_wMRS_repped_df$wMRS==i,]
    mMOS_wMRS_error1_mean <- c(mMOS_wMRS_error1_mean, mean(d$error))
  }
  
  mMOS_wMRS_error1_mean_sz <- c() #mean of each MRS set of errors: - including singles and zeros
  for (i in unique(mMOS_wMRS_sz_repped_df$wMRS)){
    d <- mMOS_wMRS_sz_repped_df[mMOS_wMRS_sz_repped_df$wMRS==i,]
    mMOS_wMRS_error1_mean_sz <- c(mMOS_wMRS_error1_mean_sz, mean(d$error))
  }
  
  
  ## combined plots:
  # real_obs:
  mMOS_wMRS_error1_combined_df <- data.frame(wMRS = c(wMRS, wMRS),
                                             error = c(mMOS_wMRS_error1_mean, mMOS_wMRS_error1_mean_sz),
                                             type = c(rep("raw", times=300), rep("with_sz", times=300)))
  mMOS_wMRS_error1_combined_plot <- ggplot(mMOS_wMRS_error1_combined_df, aes(x = wMRS, y = error, colour = type))+
    geom_point()+
    geom_smooth(alpha=0.3)+
    labs(x = "Mean realised speed (m/s)",
         y = "error (m/s)",
         title = "Mean errors between MRS and observed speeds\n(mean bc there are multiple observed speeds per MRS)\n(+ve: obs > MRS, -ve: MRS > obs)")+
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme_minimal()+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          title = element_text(size = 13),
          legend.text = element_text(size = 13))
  # mMOS_wMRS_error1_combined_plot
  
  # real-est:
  mh_wMRS_e <- hmean_m - wMRS
  mh_wMRS_e_sz <- hmean_m_sz - wMRS
  ml_wMRS_e <- lnorm_m - wMRS
  ml_wMRS_e_sz <- lnorm_m_sz - wMRS
  mg_wMRS_e <- gamma_m - wMRS
  mg_wMRS_e_sz <- gamma_m_sz - wMRS
  mw_wMRS_e <- weibull_m - wMRS
  mw_wMRS_e_sz <- weibull_m_sz - wMRS
  
  est_wMRS_combined_df <- data.frame(wMRS = c(rep(wMRS, times = 8)),
                                     error = -c(mh_wMRS_e, ml_wMRS_e, mg_wMRS_e, mw_wMRS_e, mh_wMRS_e_sz, ml_wMRS_e_sz, mg_wMRS_e_sz, mw_wMRS_e_sz),
                                     method = c(rep("hmean", length(mh_wMRS_e)), rep("lnorm", length(mh_wMRS_e)), rep("gamma", length(mh_wMRS_e)), rep("weibull", length(mh_wMRS_e)), rep("hmean", length(mh_wMRS_e)), rep("lnorm", length(mh_wMRS_e)), rep("gamma", length(mh_wMRS_e)), rep("weibull", length(mh_wMRS_e))),
                                     type = c(rep("raw", times=2400), rep("with_sz", times=2400)))
  
  est_wMRS_combined_plot <- ggplot(est_wMRS_combined_df, aes(x = wMRS, y = error, colour = method))+
    geom_point()+
    facet_grid(type ~ .)+
    geom_smooth(alpha = 0.2)+
    theme_minimal()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
    labs(x = "Mean realised speed (m/s)",
         y = "Error (m/s)",
         title = "Errors between MRS and estimated speeds\n(+ve: MRS > est, -ve: est > MRS)")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 15),
          title = element_text(size = 13),
          strip.text.x = element_text(size = 13))
  # est_wMRS_combined_plot
  
  MRS1_MOS1_s1_e1_arranged <- ggarrange(mMOS_wMRS_error1_combined_plot, est_wMRS_combined_plot, nrow = 2)
  MRS1_MOS1_s1_e1_annotated <- annotate_figure(MRS1_MOS1_s1_e1_arranged, top = text_grob(paste0(filename),
                                                                                         color = "red", face = "bold", size = 14))

  png(file=paste0("../results/PLOTS/MRS1_MOS1_s1_e1/combined_sp", sp_and_iters$speed_parameter[1], "-", sp_and_iters$speed_parameter[nrow(sp_and_iters)], ".png"),
      width=700, height=1000)
  print(MRS1_MOS1_s1_e1_annotated)
  dev.off()
  
  
  
  ### wMRS_mMOS1_mEST_e2 PLOTS ###################################################################################################################################################################################
  
  ## don't make any more plots for now - first decide which ones are the correct ones
  
  
  
  
  
  
  
  
  
  
  # NON-COMBINED PLOTS: (not using atm) (from original MRS1_MOS1_s1_e1) #########################################################################################################################################
  
  # # realised - observed speed errors plot - NOT USING ATM - USING COMBINED PLOT INSTEAD
  # mMOS_wMRS_error1_df <- data.frame(wMRS = wMRS,
  #                                 error = mMOS_wMRS_error1_mean)
  # mMOS_wMRS_error1_plot <- ggplot(mMOS_wMRS_error1_df, aes(x = wMRS, y = error))+
  #   geom_point()+
  #   geom_smooth(alpha=0.3)+
  #   labs(x = "Mean realised speed (m/s)",
  #        y = "error (m/s)",
  #        title = "Mean errors between MRS and observed speeds\n(mean bc there are multiple observed speeds per MRS)\n(+ve: obs > MRS, -ve: MRS > obs)")+
  #   geom_hline(yintercept = 0, linetype = "dashed")+
  #   theme_minimal()+
  #   theme(axis.title = element_text(size=18),
  #         axis.text = element_text(size = 15),
  #         title = element_text(size = 13))
  # # mMOS_wMRS_error1_plot
  # 
  # # estimated speeds plot
  # real_est_df <- data.frame(mean_real=wMRS, 
  #                           error = -c(h_wMRS_error, l_wMRS_error, g_wMRS_errors, w_wMRS_errors),
  #                           method = c(rep("hmean", length(h_wMRS_error)), rep("lnorm", length(l_wMRS_error)), rep("gamma", length(g_wMRS_error)), rep("weibull", length(w_wMRS_error))))
  # 
  # real_est_plot <- ggplot(real_est_df, aes(x = mean_real, y = error, colour = method))+
  #   geom_point()+
  #   geom_smooth(alpha = 0.2)+
  #   theme_minimal()+
  #   geom_hline(yintercept = 0, linetype = "dashed")+
  #   scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  #   labs(x = "Mean realised speed (m/s)",
  #        y = "Error (m/s)",
  #        title = "Errors between MRS and estimated speeds\n(+ve: MRS > est, -ve: est > MRS)")+
  #   theme(axis.title = element_text(size=18),
  #         axis.text = element_text(size = 15),
  #         legend.title = element_text(size = 18),
  #         legend.text = element_text(size = 15),
  #         title = element_text(size = 13))
  # # real_est_plot
  
  # MRS1_MOS1_s1_e1_arranged <- ggarrange(mMOS_wMRS_error1_plot, real_est_plot, nrow = 2)
  # MRS1_MOS1_s1_e1_annotated <- annotate_figure(MRS1_MOS1_s1_e1_arranged, top = text_grob(paste0(filename), 
  #                                                                                        color = "red", face = "bold", size = 14))
  # 
  # png(file=paste0("../results/PLOTS/MRS1_MOS1_s1_e1/multi_sp", sp_and_iters$speed_parameter[1], "-", sp_and_iters$speed_parameter[nrow(sp_and_iters)], ".png"),
  #     width=700, height=1000)
  # print(MRS1_MOS1_s1_e1_annotated)
  # dev.off()
  
  # # realised - observed speed errors plot - WITH SINGLE & ZERO-FRAME SEQUENCES IN
  # real_obs_df_means_sz <- data.frame(mean_real = wMRS,
  #                                 error = mMOS_wMRS_error1_mean_sz)
  # real_obs_plot_means_sz <- ggplot(real_obs_df_means_sz, aes(x = mean_real, y = error))+
  #   geom_point()+
  #   geom_smooth(alpha=0.3)+
  #   labs(x = "Mean realised speed (m/s)",
  #        y = "error (m/s)",
  #        title = "Mean errors between MRS and observed speeds\n(mean bc there are multiple observed speeds per MRS)\n(+ve: obs > MRS, -ve: MRS > obs)")+
  #   geom_hline(yintercept = 0, linetype = "dashed")+
  #   theme_minimal()+
  #   theme(axis.title = element_text(size=18),
  #         axis.text = element_text(size = 15),
  #         title = element_text(size = 13))
  # # real_obs_plot_means_sz
  
  # estimated speeds plot - WITH SINGLE & ZERO FRAME SEQUENCES
  # real_est_df_sz <- data.frame(mean_real=wMRS, 
  #                           error = -c(h_wMRS_error_sz, l_wMRS_error_sz, g_wMRS_error_sz, w_wMRS_errors_sz),
  #                           method = c(rep("hmean", length(h_wMRS_error_sz)), rep("lnorm", length(l_wMRS_error_sz)), rep("gamma", length(g_wMRS_error_sz)), rep("weibull", length(w_wMRS_error_sz))))
  # 
  # real_est_plot_sz <- ggplot(real_est_df_sz, aes(x = mean_real, y = error, colour = method))+
  #   geom_point()+
  #   geom_smooth(alpha = 0.2)+
  #   theme_minimal()+
  #   geom_hline(yintercept = 0, linetype = "dashed")+
  #   scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  #   labs(x = "Mean realised speed (m/s)",
  #        y = "Error (m/s)",
  #        title = "Errors between MRS and estimated speeds\n(+ve: MRS > est, -ve: est > MRS)")+
  #   theme(axis.title = element_text(size=18),
  #         axis.text = element_text(size = 15),
  #         legend.title = element_text(size = 18),
  #         legend.text = element_text(size = 15),
  #         title = element_text(size = 13))
  
  
  # # adding the errors plot: - now changed to adding real-obs errors with obs-est errors -- COMMENTED OUT FOR NOW
  # hmean_added <- c()
  # lnorm_added <- c()
  # gamma_added <- c()
  # weibull_added <- c()
  # for (i in 1:length(wMRS)){ # for each MRS
  #   obsMRS <- mMOS_wMRS_error1_mean[i] # obs <-> MRS error
  #   hmeanMOS <- h_mMOS_error[[i]] # hmean <-> MOS error
  #   lnormMOS <- lnorm_errors_MOS[i] # lnorm <-> MOS error
  #   gammaMOS <- gamma_errors_MOS[i] # gamma <-> MOS error
  #   weibullMOS <- weibull_errors_MOS[i] # weibull <-> MOS error
  #   hmean_added <- c(hmean_added, obsMRS+hmeanMOS)
  #   lnorm_added <- c(lnorm_added, obsMRS+lnormMOS)
  #   gamma_added <- c(gamma_added, obsMRS+gammaMOS)
  #   weibull_added <- c(weibull_added, obsMRS+weibullMOS)
  # }
  # 
  # added_df <- data.frame(MRS = wMRS,
  #                        error = -c(hmean_added, lnorm_added, gamma_added, weibull_added), # make it negative here so that +ve means models over-correct and -ve means models under-correct
  #                        type = c("hmean", "lnorm", "gamma", "weibull"))
  # 
  # added_plot <- ggplot(added_df, aes(x = MRS, y = error, colour = type))+
  #   geom_point()+
  #   geom_smooth(alpha = 0.1)+
  #   theme_minimal()+
  #   scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+ # using wheel("red", 5) from colortools package
  #   geom_hline(yintercept = 0, linetype = "dashed")+
  #   theme(axis.title = element_text(size=18),
  #         axis.text = element_text(size = 15),
  #         legend.title = element_text(size = 18),
  #         legend.text = element_text(size = 15),
  #         title = element_text(size = 13))+
  #   labs(x = "mean realised speed (m/s)",
  #        y = "error (m/s)",
  #        title = "Sum of errors between observed speeds and MRS and\nerrors between each speed estimate and MOS\n(+ve: models over-correct, -ve: models under-correct)")
  # # added_plot
  # 
  # 
  # # adding the errors plot: - WTH SINGLE & ZERO FRAME SEQUENCES
  # hmean_added_sz <- c()
  # lnorm_added_sz <- c()
  # gamma_added_sz <- c()
  # weibull_added_sz <- c()
  # for (i in 1:length(wMRS)){ # for each MRS
  #   obsMRS_sz <- mMOS_wMRS_error1_mean_sz[i] # obs <-> MRS error
  #   hmeanMOS_sz <- h_mMOS_error_sz[[i]] # hmean <-> MOS error
  #   lnormMOS_sz <- lnorm_errors_MOS_sz[i] # lnorm <-> MOS error
  #   gammaMOS_sz <- gamma_errors_MOS_sz[i] # gamma <-> MOS error
  #   weibullMOS_sz <- weibull_errors_MOS_sz[i] # weibull <-> MOS error
  #   hmean_added_sz <- c(hmean_added_sz, obsMRS_sz+hmeanMOS_sz)
  #   lnorm_added_sz <- c(lnorm_added_sz, obsMRS_sz+lnormMOS_sz)
  #   gamma_added_sz <- c(gamma_added_sz, obsMRS_sz+gammaMOS_sz)
  #   weibull_added_sz <- c(weibull_added_sz, obsMRS_sz+weibullMOS_sz)
  # }
  # 
  # added_df_sz <- data.frame(MRS = wMRS,
  #                        error = -c(hmean_added_sz, lnorm_added_sz, gamma_added_sz, weibull_added_sz),
  #                        type = c("hmean", "lnorm", "gamma", "weibull"))
  # 
  # added_plot_sz <- ggplot(added_df_sz, aes(x = MRS, y = error, colour = type))+
  #   geom_point()+
  #   geom_smooth(alpha = 0.1)+
  #   theme_minimal()+
  #   scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+ # using wheel("red", 5) from colortools package
  #   geom_hline(yintercept = 0, linetype = "dashed")+
  #   theme(axis.title = element_text(size=18),
  #         axis.text = element_text(size = 15),
  #         legend.title = element_text(size = 18),
  #         legend.text = element_text(size = 15),
  #         title = element_text(size = 13))+
  #   labs(x = "mean realised speed (m/s)",
  #        y = "error (m/s)",
  #        title = "Sum of errors between observed speeds and MRS and\nerrors between each speed estimate and MOS\n(+ve: models over-correct, -ve: models under-correct)")
  # 
  
  # # save all three together: - WTH SINGLE & ZERO FRAME SPEEDS TOO
  # arranged_sz <- ggarrange(real_obs_plot_means_sz, real_est_plot_sz, added_plot_sz, nrow = 3)
  # annotated_sz <- annotate_figure(arranged_sz, top = text_grob(paste0(filename), 
  #                                                        color = "red", face = "bold", size = 14))
  # 
  # png(file=paste0("../results/PLOTS/MRS1_MOS1_s1_e1/sz_multi_sp", sp_and_iters$speed_parameter[1], "-", sp_and_iters$speed_parameter[nrow(sp_and_iters)], ".png"),
  #     width=700, height=1000)
  # print(annotated_sz)
  # dev.off()
  # 
  # 
  # png(file=paste0("../results/PLOTS/combined_obs_multi_sp", sp_and_iters$speed_parameter[1], "-", sp_and_iters$speed_parameter[nrow(sp_and_iters)], ".png"),
  #     width=900, height=650)
  # print(real_obs_plot_means_combined)
  # dev.off()
  
}



## singlespeed_analyse
# analyse simulation results from one speed parameter and generate combined plot saved to PLOTS folder
# INPUTS
# speed_parameter - which speed parameter to run the analysis for
# iter - range of seqdats to analyse
# OUTPUT
# combined plot saved to PLOTS folder
singlespeed_analyse <- function(speed_parameter, iter){
  # concatenate results into a df:
  
  # concatenate the following from all the repeats:
  mean_reals <- c()
  obs_meanreal_errors <- c() # much longer bc includes 1 error for each observed speed (rather than a mean across all)
  hmean_errors <- c()
  lnorm_errors <- c()
  gamma_errors <- c()
  weibull_errors <- c()
  
  # concatenate the following from just the first rep:
  reals <- c()
  obs <- c() 
  hmean <- c() 
  lnorm <- c() 
  gamma <- c()
  weibull <- c()
  
  for (i in iter){
    load(paste0("../results/seq_dats/sp", speed_parameter, "iter", i, ".RData"))
    if (i == 1){
      reals <- c(reals, seq_dats$realised)
      obs <- c(obs, seq_dats$observed)
      obs <- obs[is.finite(obs)]
      estimates_1 <- estimates_calc(seq_dats)
      hmean <- estimates_1$hmean
      lnorm <- estimates_1$lnorm
      gamma <- estimates_1$gamma
      weibull <- estimates_1$weibull
    }
    estimates <- estimates_calc(seq_dats)
    filename <- paste0("sp", exp(metadata_sim$speed_parameter), # filename for storing plots
                       # "_speedSD", metadata_sim$speedSD,
                       "_pTurn", metadata_sim$pTurn,
                       "_speedCor", metadata_sim$speedCor,
                       "_kTurn", metadata_sim$kTurn,
                       "_kCor", metadata_sim$kCor,
                       "_species", metadata_sim$species,
                       "_twoCTs", metadata_sim$twoCTs,
                       "_connectedCTs", metadata_sim$connectedCTs)
    mean_reals <- c(mean_reals, estimates$mean_real)
    obs_meanreal_errors <- c(obs_meanreal_errors, estimates$obs_meanreal_error)
    hmean_errors <- c(hmean_errors, estimates$hmean_error)
    lnorm_errors <- c(lnorm_errors, estimates$lnorm_error)
    gamma_errors <- c(gamma_errors, estimates$gamma_error)
    weibull_errors <- c(weibull_errors, estimates$weibull_error)
    rm(list = c("seq_dats", "metadata_sim"))
  }
  estimates_df <- data.frame(iter = c(iter), mean_real=mean_reals, hmean_error=hmean_errors, lnorm_error=lnorm_errors, gamma_error=gamma_errors, weibull_error=weibull_errors)
  
  # realised vs observed speeds plot:
  # diff_in_length <- length(reals)-length(obs) # sometimes they're not the same length (bc of NaNs and Inf in obs)
  # real_obs_df <- data.frame(realised = reals,
  #                           observed = c(obs, rep(NA, times = diff_in_length)))
  real_obs_df <- data.frame(speed = c(reals, obs),
                            type = c(rep("realised", length(reals)), rep("observed", length(obs))))
  
  real_obs_plot <- ggplot(real_obs_df, aes(x = speed, colour = type))+
    geom_density(size = 0.8)+
    scale_colour_manual(values = c("red", "blue"))+
    theme_minimal()+
    labs(x = "speed (m/s)",
         title = "Distributions of realised and observed speeds\n(for one simulation run)\nvertical lines = mean")+
    theme(legend.title = element_blank(),
          axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          title = element_text(size = 13),
          legend.text = element_text(size = 15))+
    geom_vline(xintercept = mean(real_obs_df[real_obs_df$type=="realised",]$speed), colour = "blue", linetype = "dashed")+
    geom_vline(xintercept = mean(real_obs_df[real_obs_df$type=="observed",]$speed), colour = "red", linetype = "dashed")
  # real_obs_plot
  
  # realised vs observed speeds errors plot:
  real_obs_errors_df <- data.frame(error = obs_meanreal_errors)
  real_obs_errors_plot <- ggplot(real_obs_errors_df, aes(x = error))+
    geom_density(size = 1)+
    theme_minimal()+
    labs(x = "error (m/s)",
         title = paste0("Errors between MRS and each observed speed\n(for ", length(iter), " repeats of the same speed parameter)\n(+ve: obs > MRS, -ve: MRS > obs)"))+
    geom_vline(xintercept = 0, linetype = "dashed")+
    #geom_text(x = -0.1, y = 1, label = "real > obs", size = 5, colour = "blue")+
    # geom_text(x = 0.1, y = 10, label = "obs > real", size = 5, colour = "blue")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          title = element_text(size = 13))
  
  
  # realised vs estimates plot: - go from here: might need to faff about to get a nice plot working - make it just for the first repeat btw!
  real_est_df <- data.frame(speed = c(reals, obs),
                            type = c(rep("realised", length(reals)), rep("observed", length(obs))))
  # for the purposes of plotting: remove long tail of realised speeds: - commented out now that speeds should be naturally capped to reasonale values
  # capped <- 0.1
  # reals1 <- reals[reals<capped]
  # obs1 <- obs[obs<capped]
  # real_est_df1 <- data.frame(speed = c(reals1, obs1),
  #                           type = c(rep("realised", length(reals1)), rep("observed", length(obs1))))
  
  hline_df <- data.frame(value = c(speed_parameter, mean(reals), mean(obs), hmean, lnorm, gamma, weibull),
                         valtype = c("speed parameter", "MRS", "MOS", "hmean", "lnorm", "gamma", "weibull"),
                         valtype2 = c("other", "other", "other", "estimate", "estimate", "estimate", "estimate"))
  
  real_est_plot <- ggplot(real_est_df, aes(x = speed, y = type))+
    geom_boxplot()+
    theme_minimal()+
    geom_vline(data = hline_df,
               aes(xintercept = value, colour = valtype))+ # could add in linetype = valtype2 but think the dashed lines makes things harder to read
    #scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7"))+
    scale_colour_manual(values = c("#0000FF", "#DB00FF", "#FF0049", "#FF9200", "#92FF00", "#00FF49", "#00DBFF"))+ # generated using wheel("blue", 7) from colortools package
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 15),
          title = element_text(size = 13),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          legend.position = "bottom",
          legend.key.size = unit(1, "cm"))+
    labs(x = "speed (m/s)",
         y = "",
         title = paste0("Distributions of speeds with speed parameter, MRS, MOS, and estimated speeds\n(for one simulation run)"))
  # real_est_plot
  
  real_est_errors_df <- data.frame(error = c(hmean_errors, lnorm_errors, gamma_errors, weibull_errors),
                                   method = c(rep("hmean", length(hmean_errors)), rep("lnorm", length(lnorm_errors)), rep("gamma", length(gamma_errors)), rep("weibull", length(weibull_errors))))
  
  real_est_errors_plot <- ggplot(real_est_errors_df, aes(x = method, y = error, colour = method))+
    geom_boxplot()+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15))+
    guides(colour = "none")+
    geom_hline(yintercept = 0, linetype = "dashed")+
    scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
    #ylim(-0.05, 0.01)+
    # geom_text(x = "hmean", y = -0.02, label = "real > est", size = 5, colour = "blue")+
    #geom_text(y = 0.01, label = "est > real", size = 3)+
    coord_flip()+
    theme_minimal()+
    labs(y = "error (m/s)",
         title = paste0("Errors between MRS and estimated speeds\n(for ", length(iter), " repeats of the same speed parameter)\n(+ve: est > MRS, -ve: MRS > est)"))+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          title = element_text(size = 13))
  #ylim((min(real_est_errors_df$error)-0.2), 0.01)
  
  arranged <- ggarrange(real_obs_plot, real_est_plot, nrow = 2)
  annotated <- annotate_figure(arranged, top = text_grob(paste0(filename), face = "bold", size = 14))
  errors_arranged <- ggarrange(real_obs_errors_plot, real_est_errors_plot, nrow = 2)
  errors_annotated <- annotate_figure(errors_arranged, top = text_grob(paste0(filename), face = "bold", size = 14))
  
  png(file=paste0("../results/PLOTS/sp", speed_parameter, ".png"),
      width=900, height=700)
  print(annotated)
  dev.off()
  
  png(file=paste0("../results/PLOTS/sp", speed_parameter, "_errors.png"),
      width=900, height=700)
  print(errors_annotated)
  dev.off()
}





## path_list
# combined 100 path repeats into two lists and save back into the same folder (2 lists needed bc teh cluster can't run all 100 at once)
# INPUTS:
# folder = folder containing the set of paths to analyse (each set contains iter reps of the same speed & parameters)
# iter = number of paths
# OUTPUT:
# paths.RData == contains list called 'paths' containing all the paths
path_list <- function(folder, iter){
  
  # store metadata
  load(paste0(folder, "/iter", 1, ".RData"))
  meta <- metadata
  
  # load in first 1/2 of the paths
  paths1 <- vector(mode = "list", length = 50)
  for (i in 1:(iter/2)){
    load(paste0(folder, "/iter", i, ".RData"))
    paths1[[i]] <- path
  }
  
  # load in second 1/2 of the paths
  paths2 <- vector(mode = "list", length = 50)
  for (i in ((iter/2)+1):iter){
    load(paste0(folder, "/iter", i, ".RData"))
    paths2[[i]] <- path
  }
  
  # make filename for storing plots:
  filename <- paste0("sp", meta$speed_parameter, "_", format(meta$datetime, "%d%b%y_%H%M"))
  
  save(paths1, paths2, file = paste0(folder, "/", filename, ".RData"))
}





## Calculating average speeds ##

require(bbmle)
require(MASS)

setClass("sbm", representation("list"))

# how max likelihood works: 
# look for parameter values (mean & sd) that maximise the likelihood of the data given the model 
# (i.e. find the parameters to be put in the model to make it fit the best)

# Harmonic mean and standard error
# non-parametric
# (not fitting a distribution - just taking an average)
hmean_calc <- function(x){
  mn <- 1/mean(1/x)
  se <- mn^2 * sqrt(var(1/x)/length(x))
  c(mean=mn, se=se)
}


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



## round_dp
# function for rounding to specific number of decimal places (messes everything up otherwise for some reason)
round_dp <- function(x, k) trimws(format(round(x, k), nsmall=k)) 



# not needed atm ----------------------------------------------------------
## lines_cross - ORIGINAL
#INPUT
# lines1, lines2: 4-column arrays of x,y start and end points (ordered x1,y1,x2,y2)
# expand: if TRUE, calculates for all combinations of lines1 and lines2, otherwise row by row
#         If expand==FALSE, lines1 and lines2 must have the same number of rows.
#VALUE 
# 1 if crossed
# 0 if not crossed
# 0.5 if line 1 skims line 2 (ie an end point of 2 lies on 1)
# lines_cross <- function(lines1, lines2, expand=TRUE){
#   if(expand){
#     nr <- nrow(lines1)
#     ij <- expand.grid(1:nrow(lines1), 1:nrow(lines2))
#     lines1 <- lines1[ij$Var1, ]
#     lines2 <- lines2[ij$Var2, ]
#   }
#   p1 <- lines1[, 1:2]
#   q1 <- lines1[, 3:4]
#   p2 <- lines2[, 1:2]
#   q2 <- lines2[, 3:4]
#   or1 <- orientation(p1, q1, p2)
#   or2 <- orientation(p1, q1, q2)
#   or3 <- orientation(p2, q2, p1)
#   or4 <- orientation(p2, q2, q1)
#   res <- ifelse(or1!=or2 & or3!=or4, 1, 0)
#   skim <- (or1==0 & point_on_line(p1, q1, p2)) | (or2==0 & point_on_line(p1, q1, q2))
#   res <- res - ifelse(skim, 0.5, 0)
#   if(expand) res <- matrix(res, nrow=nr)
#   res
# }




# to test arc function:
# line <- data.frame(x1 = 2,
#                    y1 = 4,
#                    x2 = 8,
#                    y2 = 9)
# 
# arc <- data.frame(x = 2,
#                   y = 4,
#                   r = 10,
#                   th = pi/4)

# AIMS
# we wanna simulate where we know the actual speeds
# then be able to detect the biases
# currently: these simulations don't monitor all the potential biases
# e.g. missing high speeds
# it's all about quantifying bias
# comparing estimation outcomes with the truth



# seq_dat - ORIGINAL
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
# seq_dat <- function(speed_parameter, step_no, size, xlim, speedSD, pTurn, speedCor, kTurn, kCor, x, y, r, th, plot_path = TRUE, twoCTs = FALSE){
#   xlim <- xlim
#   path <- pathgen(n=step_no, kTurn=kTurn, kCor=kCor, pTurn=pTurn, logspeed=speed_parameter, speedSD=speedSD, speedCor=speedCor, xlim=xlim, wrap=TRUE)
#   if (twoCTs == FALSE){
#     dz <- data.frame(x=x, y=y, r=r, th=th, dir=0) # initially set radius to 10m and theta to 1.65 - based on distributions of radii & angles in regent's park data -- then M & C said angle isn't usually more than 1 so set to 1
#     if (size == 1){
#       posdat_all <- sequence_data_large(path, dz) # posdat_all == all of those that fell in the detection zone (+ column saying whether or not it got detected)
#     }
#     if (size == 0){
#       posdat_all <- sequence_data_small(path, dz)
#     }
#   }
#   if (twoCTs == TRUE){
#     dz1 <- data.frame(x=x, y=y, r=r, th=th, dir=0)
#     dz2 <- data.frame(x = (x + r*sin(th)), y = (y + r*cos(th)), r = r, th = th, dir = 1)
#     if (size == 1){
#       posdat_all1 <- sequence_data_large(path, dz1) # posdat_all == all of those that fell in the detection zone (+ column saying whether or not it got detected)
#       posdat_all2 <- sequence_data_large(path, dz2)
#       posdat_all <- rbind(posdat_all1, posdat_all2)
#     }
#     if (size == 0){
#       posdat_all1 <- sequence_data_small(path, dz1)
#       posdat_all2 <- sequence_data_small(path, dz2)
#       posdat_all <- rbind(posdat_all1, posdat_all2)
#     }
#   }
#   posdat <- posdat_all[posdat_all$detected==TRUE,] # only the points which do actually get detected by the camera
#   v <- calc_speed(posdat) # speeds of sequences
#   
#   ### realised speeds:
#   obs_lengths <- c() # lengths of the observed speed sequences
#   for (i in unique(posdat$sequenceID)){
#     p <- posdat[posdat$sequenceID==i,]
#     if (nrow(p)>1){ # don't count the single-frame sequences
#       obs_lengths <- c(obs_lengths, nrow(p))
#     }
#   }
#   mean_obs_length <- mean(obs_lengths)
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
#   zeros_vals <- zeros[zeros!=0]
#   n_zeros <- sum(zeros_vals[1:length(zeros_vals)])
#   
#   df <- data.frame(realised = realised_spds, # realised speeds
#                    observed = v$speed, # observed speeds
#                    mean_obs_length = c(mean_obs_length, rep(NA, (length(v$speed)-1))),
#                    n_singles = c(n_singles, rep(NA, (length(v$speed) - 1))), # no. of single frames
#                    n_zeros = c(n_zeros, rep(NA, (length(v$speed) - 1))), # no. of zero frames
#                    n_points = c(nrow(posdat), rep(NA, (length(v$speed) - 1)))) # total no. of position datapoints detected by the camera
#   if (plot_path == TRUE){
#     plot_wrap(path, lineargs = list(col="grey"))
#     plot_dzone(dz, border=2)
#     points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
#   }
#   return(df)
# }

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



