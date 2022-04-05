### SIMULATION ANALYSIS USING RESULTS FROM MULTIPLE DIFFERENT RUNS ##

require(ggplot2)


setwd("~/Documents/Project/CamtrapSimulation/code/simulation")

setwd("seq_dats/0.06-0.15_5e+05_1_(0,40)")

load("seq_dats.RData")

# work out things to use in plotting:
sim_length <- length(seq_dats)/5 # number of speeds inputted into the simulation
obs_real_error <- c() # mean error between observed and realised speeds for each simulated set of speeds
real_mean <- c() # mean realised speeds
singles <- c() # number of single frames for each simulation run
zeros <- c() # number of zero frames for each simulation run
singles_prop <- c() # number of single frames / total number of points recorded
zeros_prop <- c() # number of zero frames / total number of points recorded
obs_count <- c() # number of observed speeds captured
for (i in 1:sim_length){
  s <- seq_dats[,i]
  real_mean <- c(real_mean, mean(s$realised))
  singles <- c(singles, s$n_singles[1])
  zeros <- c(zeros, s$n_zeros[1])
  singles_prop <- c(singles_prop, s$n_singles[1]/s$n_points[1])
  zeros_prop <- c(zeros_prop, s$n_zeros[1]/s$n_points[1])
  obs_count <- c(obs_count, length(s$observed))
  o <- s$observed
  o <- o[!is.na(o)]
  r <- mean(seq_dats[,i]$realised)
  errors <- c()
  for (j in 1:length(o)){
    e <- o[j] - r
    errors <- c(errors, e)
    errors_mean <- mean(errors)
  }
  obs_real_error <- c(obs_real_error, errors_mean)
}

# pooled1 <- data.frame(obs_real_error = obs_real_error, ## for 0.06-0.15_50000_0_0,40
#                                 real_mean = real_mean,
#                                 n_singles = singles_prop,
#                                 n_zeros = zeros_prop)


# pooled2 <- data.frame(obs_real_error = obs_real_error, ## for 0.06-0.15_5e+05_0_0,40
#                                 real_mean = real_mean,
#                                 n_singles = singles_prop,
#                                 n_zeros = zeros_prop)


# pooled3 <- data.frame(obs_real_error = obs_real_error, ## for 0.1-0.19_5e+05_1_0,40
#                                 real_mean = real_mean,
#                                 n_singles = singles_prop,
#                                 n_zeros = zeros_prop)

# pooled5 <- data.frame(obs_real_error = obs_real_error, ## for 0.1-0.19_5e+05_1_0,50
#                                 real_mean = real_mean,
#                                 n_singles = singles_prop,
#                                 n_zeros = zeros_prop)

# pooled4 <- data.frame(obs_real_error = obs_real_error, ## for 0.1-0.19_50000_0_0,40
#                                 real_mean = real_mean,
#                                 n_singles = singles_prop,
#                                 n_zeros = zeros_prop)

pooled6 <- data.frame(obs_real_error = obs_real_error, ## for 0.06-0.15_5e+05_1_0,40
                                real_mean = real_mean,
                                n_singles = singles_prop,
                                n_zeros = zeros_prop)



# for the next ones:
pooled_df <- read.csv("seq_dats/pooled_results/pooled_df.csv")

pooled_df <- rbind(pooled_df, pooled6)

write.csv(pooled_df, file = "seq_dats/pooled_results/pooled_df.csv")


















# PLOTTING ----------------------------------------------------------------

real_obs_errors_plot <- ggplot(pooled_df, aes(obs_real_error))+
  geom_density(colour = "blue")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_minimal()+
  labs(x = "mean error between realised and observed speeds (m/s)")+
  theme(axis.title = element_text(size=21),
        axis.text = element_text(size = 18))
real_obs_errors_plot

png(file="seq_dats/pooled_results/real_obs_errors.png",
    width=900, height=600)
real_obs_errors_plot
dev.off()