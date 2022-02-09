devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/CamtrapSimulation/master/CamtrapSimulation.R")

require(ggplot2)
require(ggpubr)
library(gridExtra)

# improvements to make to simulation --------------------------------------



## 1. simulations of movement:

# - larger animals have greater turn radii and can't turn as rapidly (Wilson et al. 2015)
# -- BUT: tortuosity is v similar for hedgehogs & foxes --> so my hypothesis may not be super supported - but would need to compare more species 

## 2. how we decide which chunks get detected by the camera:

# - include detection probabilities? - e.g. further = less likely to get detected (McIntyre et al. 2020)
# could make changes to the is_in_dz function
# but not sure how much to change this by..
# could add in a probability gradient - going from 1 to 0 at a certain gradient
# to figure out what this gradient could be - could look at the data? - see distance_prob.R script
# to leave for now and discuss with M

# - higher diff between ambient & animal surface temp = increased detection probability (McIntyre et al. 2020)
# -- but probably can't do much about

# - reaction time of the camera? - e.g. if 2 positions are close together in time then the camera might not realistically trigger for both
# --> is this potentially already taken into account though?

# animal body mass??
# -- could be one to link with tortuosity too 
# -- e.g. higher body mass = less tortuous and also more likely to get detected by a CT


# Example -----------------------------------------------------------------

# Create a correlated random walk movement path
path <- pathgen(5e3, kTurn=2, kCor=TRUE, pTurn=1, 
                logspeed=-2, speedSD=1, speedCor=0, 
                xlim=c(0,10), wrap=TRUE)

# Create a camera detection zone
dz <- data.frame(x=5, y=2, r=6, th=1, dir=0)

# Visualise
plot_wrap(path, lineargs = list(col="grey"))
plot_dzone(dz, border=2)

# Create position data for sequences falling within the detection zone
posdat <- sequence_data(path, dz)
points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)

# Create speed data summarised for each sequence
seqdat <- calc_speed(posdat)
























# biases ----------------------------------------------------

# 1. speed is potentially proportional to trap rate
# 2. very high speeds likely to be missed by cameras
# 3. tortuosity?
# 4. habitat type?
# 5. diff CTs/settings?



# very high speeds likely to be missed by CTs -----------------------------

# simulate animal movements with varying speeds

# high speeds: 
# hedgehogs: max ~ 1.7m/s
# foxes: max ~ 13.9m/s

# Create a correlated random walk movement path
path2 <- pathgen(5e3, kTurn=2, kCor=TRUE, pTurn=1, 
                logspeed=-1, speedSD=1, speedCor=0, 
                xlim=c(0,10), wrap=TRUE)

# Create a camera detection zone
dz2 <- data.frame(x=5, y=2, r=6, th=1, dir=0)

# Visualise
plot_wrap(path2, lineargs = list(col="grey"))
plot_dzone(dz2, border=2)

# Create position data for sequences falling within the detection zone
posdat2 <- sequence_data(path2, dz2)
points(posdat2$x, posdat2$y, col=2, pch=16, cex=0.5)

# Create speed data summarised for each sequence
seqdat2 <- calc_speed(posdat2)

# visualise how the measured speeds relate to what it's meant to be

ggplot()+
  geom_density(aes(x = seqdat2$speed))+
  geom_vline(xintercept = exp(-1), colour = 'blue', size = 1)+
  theme_minimal()+
  geom_text(aes(x=exp(-1), label="true speed", y=0.5), colour="blue", angle=90, vjust = 1.2, text=element_text(size=11))



# plot how this relationship changes as you up the speed

# make function to do everything with varying speed as an input:

speeds_plot <- function(speed) {
  path <- pathgen(5e3, kTurn=2, kCor=TRUE, pTurn=1, 
          logspeed=speed, speedSD=1, speedCor=0, 
          xlim=c(0,10), wrap=TRUE)
  
  dz <- data.frame(x=5, y=2, r=6, th=1, dir=0)

  plot_wrap(path, lineargs = list(col="grey"))
  plot_dzone(dz, border=2)
  
  posdat <- sequence_data(path, dz)
  points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
  
  seqdat <- calc_speed(posdat)
  
  # p <- ggplot()+
  #   geom_density(aes(x = seqdat$speed))+
  #   geom_vline(xintercept = exp(speed), colour = 'blue', size = 1)+
  #   theme_minimal()

  colours <- c("real" = "blue", "measured" = "red")
  p <- ggplot()+
    geom_density(aes(x = seqdat$speed, colour = 'measured'))+
    geom_density(aes(x = path$speed, colour = 'real'))+
    theme_minimal()+
    scale_color_manual(values = colours)+
    labs(x = "speed",
         colour = "speed")
  
  return(plot(p))
  
}


# plot how the relationship changes as you get higher speeds:

# make vector of some speeds to compare:
speeds <- seq(from = -3, to = 2, by = 0.25) # upper limit here is a bit under the max for foxes

# run the simulation on each speed
plots <- lapply(speeds, speeds_plot)

# arrange all the plots in one panel
m <- marrangeGrob(plots, nrow = 7, ncol = 3)

ggsave(filename = "high_speeds3.png", plot = m, path = "plots", width = 10, height = 18)

# there is definitely a threshold above which the cameras start underestimating speed - but also same for the low speeds

# maybe there's a bias in both directions

# but also this isn't really showing us that CTs miss high speeds - it's more just showing us how high speeds get underestimated and low speeds get overestimated


# would be good to look at the proportion of frame numbers equal to 1 - how those change with increasing speeds (expect them to just increase)

## plot how the number of frame numbers equal to 1 increases as speeds increase
# then could investigate factors that affect this 
# e.g. camera settings, tortuosity

single_frames <- function(speed) {
  path <- pathgen(5e3, kTurn=2, kCor=TRUE, pTurn=1, 
                  logspeed=speed, speedSD=1, speedCor=0, 
                  xlim=c(0,10), wrap=TRUE)
  
  dz <- data.frame(x=5, y=2, r=6, th=1, dir=0)
  
  plot_wrap(path, lineargs = list(col="grey"))
  plot_dzone(dz, border=2)
  
  posdat <- sequence_data(path, dz)
  points(posdat$x, posdat$y, col=2, pch=16, cex=0.5)
  
  seqdat <- calc_speed(posdat)
  
  seqdat2 <- cbind(seqdat, rep(speed, length(nrow(seqdat))))
  
  single_frames <- nrow(seqdat2[seqdat2$points==1,])
  
  return(c(speed, single_frames))
}

speeds2 <- seq(-3, 2, by = 0.01)

lapply(speeds2, single_frames) # takes absolutely ages!

# plot speed against single frame number


# try varying different things and see what happens to this relationship? - e.g. add inputs to the speeds_plot and single_frames functions for things like tortuosity

# also see what happens when alter camera settings




## could also be worth investigating what's going on with these low speeds 

# try varying tortuosity and seeing what happens?

