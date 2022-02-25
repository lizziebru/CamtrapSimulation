### RUNNING THE SIMULATION USING PIPELINE ###


## to do no: vary speeds to get an unbiased simulation


rm(list = ls())
dev.off()

# define the input average speed to simulate and how many times to run the simulation:

simulate_speed <- -1

n <- 100


source("~/Documents/Project/CamtrapSimulation/code/simulation/pipeline.R", echo=TRUE)


# OUTPUTS:

# plots:
# - n plots of simulation itself with the simulated path, dz, and points captured by the CT - need to figure out how to arrange these onto one multi-panel plot though
# - sbm_plots: n plots of the fitted SBM models: all three models for each simulation repeat - ditto
# - box: boxplot of speed distributions
# - box_minus_high_measured_speeds: boxplot of speed distributions with higher measured_speeds removed
# - averages_plot: plot of means and medians compared
##--> box, box_minus_high_measured_speeds, and averages_plot all get saved to the plots folder in this Rproj

# useful dataframes:
# - speeds_df: input speeds and averaged speeds (using each of the four methods) for each simulation rep
# - model_AICs: AICs for each model fitted for each simulation rep

