devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/CamtrapSimulation/master/CamtrapSimulation.R")

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




















### improvements to make:

## simulations of movement:

# - larger animals have greater turn radii and can't turn as rapidly (Wilson et al. 2015)


## how we decide which chunks get detected by the camera:

# - include detection probabilities? - e.g. further = less likely to get detected (McIntyre et al. 2020)
# could make changes to the is_in_dz function
# but not sure how much to change this by..
# could add in a probability gradient - going from 1 to 0 at a certain gradient
# to figure out what this gradient could be - could look at the data? - see distance_prob.R script



# - higher diff between ambient & animal surface temp = increased detection probability (McIntyre et al. 2020)
# -- but probably can't do much about

# - reaction time of the camera? - e.g. if 2 positions are close together in time then the camera might not realistically trigger for both
# --> is this potentially already taken into account though?