## EXTRA CAMTRAPSIMULATION FUNCTIONS ##
# functions not used in CamtrapSimulation.R for the purposes of the final project analyses
# but were used previously and might be useful in future work


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


## path_list
# combined 100 path repeats into two lists and save back into the same folder (2 lists needed bc the cluster can't run all 100 at once)
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


## round_dp
# function for rounding to specific number of decimal places (messes everything up otherwise for some reason)
round_dp <- function(x, k) trimws(format(round(x, k), nsmall=k))



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

# MAKING VISUALISATION PLOT OF WHERE POINTS GET DETECTED IN SPACE ------------------------------------------------------------------------------------------

## make_vis_plotting_variables
# make visualisation plotting variables for one rep of each simulation with a different body mass
# this is done separately from the other generation of variables (generate_plotting_variables function) and plotting (make_plots function) bc here only one rep of each body mass simulation is used rather than all of them
# INPUTS
# Mb_range: range of body masses to plot for
# r: radius of detection zone
# th: angle of dz
# twoCTs: whether or not to use two CTs
# connectedCTs: whether or not the two CTs are set up in a connected way such that the detection zones are triangles side-by-side facing opposite ways (hence maximising their area of contact and making one large rectangular-ish shaped dz)
# scaling: logical. Whether or not to scale hz function with body mass
# OUTPUT
# a posdat_all dataframe for each body mass saved in the same folder as the path
make_vis_plotting_variables <- function(Mb_range, r, th, twoCTs=FALSE, connectedCTs=FALSE, scaling){
  
  for (i in Mb_range){
    Mb=i
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
    
    
    ## SINGLE FRAMES (== when one point in a sequence gets detected)
    
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
    
    
    ## work out speeds of single frames and number of single frames
    # select the seqID posdats when you only have one TRUE (i.e. single frame) and work out its speed using points before and after it in the whole path
    singles_count <- 0
    for (k in unique(posdat_all$sequenceID)){
      p <- posdat_all[posdat_all$sequenceID==k,] # subset by sequence ID
      n_true <- nrow(p[p$detected==TRUE,])# subset by TRUE for detection to count the number of points detected by the CT in this sequence
      if (n_true == 1){ # if there's only one point detected by the CT (i.e. it's a single frame)
        singles_seqIDs <- c(singles_seqIDs, k) # store the sequence ID of this single frame
        
        # select the x and y coords of the detected single point
        single_x <- p[p$detected==TRUE,]$x 
        single_y <- p[p$detected==TRUE,]$y
        
        # work out a count for that single frame based on its detection probability
        single_radius <- sqrt((single_y-dz$y)^2 + (single_x-dz$x)^2) # work out radius (distance from CT)
        prob_detect <- hz_radius(single_radius, Mb=Mb, scaling=scaling) # probability of detection using hazard function
        singles_count <- singles_count + prob_detect # add that to the number of single frames (so that it's a value that's taken probabilistic stuff into account)
        
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
    
    
    ## work out speeds of frames where the point(s) fell in the dz but didn't get detected (use the same method as for singles for the single ones)
    # these are in posdat_all[is.na(posdat_all$speed),]
    spds_with_extras_col <- c() # new speeds column with the added speeds of those points
    
    for (n in unique(posdat_all$sequenceID)){
      p <- posdat_all[posdat_all$sequenceID==n,] # subset by sequence ID
      if (is.na(p$speed)){ # if the speed is NA (bc there were no points at all detected in that sequence) - i.e. selects the ones we're interested in here
        if (nrow(p)==1){ # if it's also a single frame, need to use the same method as for single frames to work out speed
          
          # select the x and y coords of the detected single point
          single_x <- p$x 
          single_y <- p$y
          
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
          v <- sum(rows_needed$v)/(nrow(rows_needed)-1) # speed = distance / time
          
          spds_with_extras_col <- c(spds_with_extras_col, rep(v, times = nrow(p))) # add this speed to the new speed column
        }
        else { # if it's not a single frame, can just work out speed as the arithmetic mean of the speeds in that sequence
          v <- (sum(na.omit(p$distance)))/(length(na.omit(p$distance)))
          spds_with_extras_col <- c(spds_with_extras_col, rep(v, times = nrow(p)))
        }
      }
      else { # otherwise, just use the speed already there
        spds_with_extras_col <- c(spds_with_extras_col, rep(p$speed[1], times = nrow(p)))
      }
    }
    
    posdat_all$speed <- spds_with_extras_col
    
    ## there should now be no more NAs in the speed column
    
    ## speeds of zero frames - need to work out both the coords of these zero frames and their speed (and add both as an extra row in the df)
    
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
      zeros1 <- future_apply(path_df_paired, 1, zero_frame, dz = dz1, posdat_all = posdat_all, max_real = max_real)
      zeros2 <- future_apply(path_df_paired, 1, zero_frame, dz = dz2, posdat_all = posdat_all, max_real = max_real)
      zeros <- c(zeros1, zeros2)
    }
    zeros_count <- sum(zeros[1:length(zeros)]) # add up all of the zero counts to get total number of zero frames in that simulation (each count is multiplied by its detection probability though - so this number should be compared to the number of detected points rather than the total number of (detected+undetected) points falling in the dz -- ditto for the singles_count above)
    
    ## speeds of zero-frame sequences
    path_df_paired["ZERO"] <- zeros
    zeros_dat <- path_df_paired[path_df_paired$ZERO!=0,] # dataframe with only pairs of points which make a zero frame
    zeros_v <- c() # speed of each zero frame sequence
    zeros_x <- c() # coords of each zero frame sequence - defined as just the midpoint between the two points either side of the zero frame
    zeros_y <- c()
    zeros_x1 <- c()
    zeros_x2 <- c()
    zeros_y1 <- c()
    zeros_y2 <- c()
    for (l in 1:nrow(zeros_dat)){
      z <- zeros_dat[l,]
      speed <- sqrt((z$y2-z$y1)^2 + (z$x2-z$x1)^2) # speed = distance between the two points bc timestep = 1s
      x_coord <- (z$x1+z$x2)/2
      y_coord <- (z$y1+z$y2)/2
      zeros_v <- c(zeros_v, speed)
      zeros_x <- c(zeros_x, x_coord)
      zeros_y <- c(zeros_y, y_coord)
      zeros_x1 <- c(zeros_x1, z$x1)
      zeros_x2 <- c(zeros_x2, z$x2)
      zeros_y1 <- c(zeros_y1, z$y1)
      zeros_y2 <- c(zeros_y2, z$y2)
    }
    
    # make dataframe of all the info of these - then can rbind it to the bottom of posdat_all
    zeros_df <- data.frame(x = zeros_x, y = zeros_y, sequenceID = rep(NA, times = length(zeros_x)), distance = rep(NA, times = length(zeros_x)), detected = rep(FALSE, times = length(zeros_x)), speed = zeros_v, single = rep(FALSE, times = length(zeros_x)), zero = rep(TRUE, times = length(zeros_x)), zero_x1=zeros_x1, zero_x2=zeros_x2, zero_y1=zeros_y1, zero_y2=zeros_y2)
    
    # add zeros column to posdat_all so that all the columns are matching
    posdat_all["zero"] <- rep(FALSE, times = nrow(posdat_all))
    
    # also zeros_x1, x2, y1, & y2
    posdat_all["zero_x1"] <- rep(NA, times = nrow(posdat_all))
    posdat_all["zero_x2"] <- rep(NA, times = nrow(posdat_all))
    posdat_all["zero_y1"] <- rep(NA, times = nrow(posdat_all))
    posdat_all["zero_y2"] <- rep(NA, times = nrow(posdat_all))
    
    # combine both dataframes by row
    posdat_all <- rbind(posdat_all, zeros_df)
    
    ## ratio of detected/non-detected and singles&zeros/sequences with 2+ points
    
    detected_ratio <- (nrow(posdat_all[posdat_all$detected==TRUE,]))/nrow(posdat_all[posdat_all$zero==FALSE,])
    posdat_all["detected_percent"] <- rep(detected_ratio*100, times = nrow(posdat_all))
    
    s_ratio <- nrow(posdat_all[posdat_all$single==TRUE & posdat_all$detected==TRUE,])/nrow(posdat_all[posdat_all$zero==FALSE & posdat_all$detected==TRUE,]) # just compare to total no. detected
    posdat_all["single_percent"] <- rep(s_ratio*100, times = nrow(posdat_all))
    z_ratio <- nrow(posdat_all[posdat_all$zero==TRUE,])/nrow(posdat_all[posdat_all$zero==FALSE & posdat_all$detected==TRUE,]) # just compare to total no. detected
    posdat_all["zero_percent"] <- rep(z_ratio*100, times = nrow(posdat_all))
    
    sz_ratio <- (nrow(posdat_all[posdat_all$single==TRUE & posdat_all$detected==TRUE,]) + nrow(posdat_all[posdat_all$zero==TRUE,]))/nrow(posdat_all[posdat_all$zero==FALSE & posdat_all$detected==TRUE,])
    posdat_all["sz_percent"] <- rep(sz_ratio*100, times = nrow(posdat_all))
    
    
    # add column with body mass
    
    posdat_all["Mb"] <- rep(i, times= nrow(posdat_all))
    
    
    
    
    # also save each individual dataframe to the path folder
    save(posdat_all, file = paste0("../Mb_results/paths_30Jun22_1727/Mb", i, "/vis_plotting_variables_withzcoords_iter1.RData")) # add sp range and number of iters too to the name of the output file
    
    rm(list = c("seq_dats", "metadata_sim", "path"))
    
  }
}



## vis_plot
# make the visualisation plot using variables generated in make_vis_plotting_variables
# INPUTS
# Mb_range: range of body masses to plot for
# r: radius of detection zone
# th: angle of dz
# twoCTs: whether or not to use two CTs
# connectedCTs: whether or not the two CTs are set up in a connected way such that the detection zones are triangles side-by-side facing opposite ways (hence maximising their area of contact and making one large rectangular-ish shaped dz)
# OUTPUT
# visualisation plots outputted to the plots/vis_plots folder
vis_plot <- function(Mb_range, r, th, twoCTs=FALSE, connectedCTs=FALSE){
  
  ## concatenate the posdat_all dataframes from individual body masses into one big one
  
  # variables needed
  Mb_plot <- c() # body mass associated with the simulation run that the point is in
  x_plot <- c() # x coord of a position data point falling in the dz
  y_plot <- c() # y coord of that position data point
  v_plot <- c() # speed of the sequence that the point is in
  seqID_plot <- c() # seqID of the sequence the point is in
  dist_plot <- c() # distance between that point and the previous one
  detected_plot <- c() # whether that point got detected by the CT
  single_plot <- c() # whether that point was a single frame
  zero_plot <- c() # whether that point was a zero frame
  zero_x1_plot <- c() # coords of points either side of zero frames to see what's going on
  zero_x2_plot <- c()
  zero_y1_plot <- c()
  zero_y2_plot <- c()
  detected_percent_plot <- c() # ratio of detected to non-detected points to display on visualisation plot
  single_percent_plot <- c() # ratio of no. of singles vs no. of sequences with 2 or more points
  zero_percent_plot <- c() # ratio of no. of zeros vs no. of sequences with 2 or more points
  sz_percent_plot <- c() # ratio of no. of singles & zeros vs no. of sequences with 2 or more points
  
  for (i in Mb_range){
    
    ## load in the path and seq_dats for that simulation run #####################################################################################
    
    load(paste0("../Mb_results/paths_30Jun22_1727/Mb", i, "/vis_plotting_variables_iter1.RData"))
    
    # save all the variables to make one big vis_df outside this loop with info from multiple body masses
    Mb_plot <- c(Mb_plot, posdat_all$Mb)
    x_plot <- c(x_plot, posdat_all$x)
    y_plot <- c(y_plot, posdat_all$y)
    v_plot <- c(v_plot, posdat_all$speed)
    seqID_plot <- c(seqID_plot, posdat_all$sequenceID)
    dist_plot <- c(dist_plot, posdat_all$distance)
    detected_plot <- c(detected_plot, posdat_all$detected)
    single_plot <- c(single_plot, posdat_all$single)
    zero_plot <- c(zero_plot, posdat_all$zero)
    zero_x1_plot <- c(zero_x1_plot, posdat_all$zero_x1)
    zero_x2_plot <- c(zero_x2_plot, posdat_all$zero_x2)
    zero_y1_plot <- c(zero_y1_plot, posdat_all$zero_y1)
    zero_y2_plot <- c(zero_y2_plot, posdat_all$zero_y2)
    detected_percent_plot <- c(detected_percent_plot, posdat_all$detected_percent)
    single_percent_plot <- c(single_percent_plot, posdat_all$single_percent)
    zero_percent_plot <- c(zero_percent_plot, posdat_all$zero_percent)
    sz_percent_plot <- c(sz_percent_plot, posdat_all$sz_percent)
    
  }
  
  ## visualisation plot 
  
  vis_df <- data.frame(Mb=Mb_plot,x=x_plot, y=y_plot, speed=v_plot, seqID=seqID_plot, dist=dist_plot, detected=detected_plot, single=single_plot, zero=zero_plot)
  
  # need new x & y coords so that CT is at 0,0 and points are relative to that
  new_cols <- apply(vis_df, 1, vis_df_new_xy)
  x_new <- sapply(new_cols, "[[", 1)
  y_new <- sapply(new_cols, "[[", 2)
  vis_df["x_new"] <- x_new
  vis_df["y_new"] <- y_new
  
  vis_plot <- ggplot()+
    geom_point(data = vis_df[vis_df$detected==TRUE & vis_df$single==FALSE,], aes(x = x_new, y = y_new, colour = speed), shape = 1)+
    # facet_grid(vars(sp))+
    facet_wrap(~ Mb, ncol = 2)+
    # scale_colour_distiller(direction = 1)+
    # scale_colour_gradient(low = "#56B4E9", high = "dark blue", na.value = NA)+
    scale_colour_gradient(low = "#66CCFF", high = "dark blue", na.value = NA)+
    # scale_shape_manual(values=c(1, 16))+
    # scale_colour_manual(values = c("#0000FF", "#FF0000"))+ # generated using wheel("blue, 3)
    theme_minimal()+
    labs(x = "x", y = "y",
         title = "Locations of position data points for one run\nof a simulation of a given body mass")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          title = element_text(size = 13),
          panel.grid.minor = element_blank(),
          legend.title = element_text("Speed (m/s)"),
          plot.title = element_text(hjust = 0.5))+
    
    new_scale_colour()+
    geom_point(data = vis_df[vis_df$zero==TRUE,], aes(x = x_new, y = y_new, colour = "zero"), shape = 1)+
    scale_colour_manual(name = "",
                        breaks = c("zero"),
                        values = c("red"))+
    guides(colour = guide_legend(override.aes = list(size=5)))+
    theme(legend.text = element_text(size = 12))
  vis_plot
  
  png(file=paste0("../Mb_results/plots/vis_plots/Mb", Mb_range[1], "-", Mb_range[length(Mb_range)], "_detected_vs_zeros.png"),
      width=900, height=650)
  print(vis_plot)
  dev.off()
  
  
  # --> need to make changes to this that C suggested though
  
  
}


## vis_df_newcols
# to help make the visualisation plot - need to change x & y coords so that CT is at 0,0 and x and y coords are relative to that
# INPUT
# dataframe of points to which in which the column needs to be filled
# OUTPUT
# list containing two new vectors of values for the two new columns: x_new & y_new
vis_df_new_xy <- function(df){
  
  x_new <- as.numeric(df[[2]]) - 20 # so that the centre of the x scale is 0
  y_new <- as.numeric(df[[3]]) - 10 # so that the y scale starts at 0
  
  output <- list(x_new = x_new,
                 y_new = y_new)
  
  return(output)
}

