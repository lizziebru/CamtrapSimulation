### compile & analyse data on multiple runs of the same simulation for multiple diff speeds ###

require(ggplot2)
#require(FNN) - made my own KL divergence function - but there's a built-in one here if needed at later stage

source("~/Documents/Project/CamtrapSimulation/code/simulation/CamtrapSimulation.R", echo=TRUE)

setwd("~/Documents/Project/CamtrapSimulation/code/simulation")

setwd("results2/size0_sd1_pTurn0.5_Cor0.9_kTurn2_kCorT_r9_th0.7") # set which one to analyse


n_speeds <- 16 # number of different speeds simulated


# set speed parameters
speedstart <- 0.05
speedstop <- 0.2
speedby <- 0.01

# need to do it this weird way to avoid issues with floating points (basically need to either round or use all.equal when this problem crops up)
speed_digits <- max(decimalplaces(speedstart), decimalplaces(speedstop), decimalplaces(speedby))
speed_options <- seq(speedstart, speedstop, speedby)
speed_parameters <- round(speed_options, digits=speed_digits+1)



# wrangle & compile data --------------------------------------------------

real <- c()
obs <- c()
speed_lengths <- c()
singles <- c()
zeros <- c()
points <- c()
#mean_obs_length <- c()
counter <- 0
for (i in speed_parameters){
  counter <- counter + 1
  for (j in 0:9){
    k <- counter + j*n_speeds # run number
    load(paste0("sp", i, "_run", k, ".RData"))
    real <- c(real, seq_dats$realised)
    obs <- c(obs, seq_dats$observed)
    speed_lengths <- c(speed_lengths, length(seq_dats$realised))
    singles <- c(singles, seq_dats$n_singles[1])
    zeros <- c(zeros, seq_dats$n_zeros[1])
    points <- c(points, seq_dats$n_points[1])
    #mean_obs_length <- c(mean_obs_length, seq_dats$mean_obs_length[1])
  }
}

speed <- c()
n_singles <- c()
n_zeros <- c()
n_points <- c()

for (i in 0:(n_speeds-1)){
  select <- c(1:10) + 10*i
  speed_param <- speed_parameters[i+1]
  newspeed <- rep(speed_param, times = sum(speed_lengths[select]))
  speed <- c(speed, newspeed)
  n_singles <- c(n_singles, sum(singles[select]))
  n_zeros <- c(n_zeros, sum(zeros[select]))
  n_points <- c(n_points, sum(points[select]))
}

singles_prop <- n_singles/n_points
zeros_prop <- n_zeros/n_points

df1 <- data.frame(speed = speed,
                  real = real,
                  obs = obs)

log_pdf_ratio <- c()
max_obs <- c() # for vertical lines in plot of distributions of observed and realised speeds
max_real <- c() # ditto
kl_divergence <- c() # divergence from realised distribution to observed distribution
for (i in speed_parameters){ # bugs for the second one when in a loop - make it into a function instead to see what happens
  d <- df1[speed==i,]
  o <- d$obs
  o <- o[is.finite(o)]
  d_real <- density(d$real)
  d_obs <- density(o)
  real_y <- d_real$y
  obs_y <- d_obs$y
  obs_y <- obs_y[is.finite(obs_y)]
  max_obs <- c(max_obs, d_obs$x[which.max(obs_y)])
  max_real <- c(max_real, d_real$x[which.max(real_y)])
  log_pdf_ratio <- c(log_pdf_ratio, log_pdf_calc(real_y=real_y, obs_y=obs_y))
  kl_divergence <- c(kl_divergence, kl_div_calc(real_y=real_y, obs_y=obs_y))
}

df2 <- data.frame(speed_param = speed_parameters,
                  log_pdf_ratio = log_pdf_ratio,
                  kl_divergence = kl_divergence,
                  n_singles = n_singles,
                  singles_prop = singles_prop,
                  n_zeros = n_zeros,
                  zeros_prop = zeros_prop,
                  #mean_obs_seq_length = mean_obs_length,
                  n_points = n_points)

df3 <- data.frame(speedparameter = rep(df1$speed, times = 2),
                  speed = c(real, obs),
                  obs_real = c(rep("realised", times = length(real)), rep("observed", times = length(obs))))


# plotting biases ----------------------------------------------------------

## DISTRIBUTIONS OF OBSERVED AND REALISED SPEEDS

obs_real <- ggplot(df3, aes(x = speed, colour = obs_real))+
  geom_density()+
  facet_wrap(~ speedparameter, ncol=4)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  scale_colour_manual(values = c("blue", "red"))+
  xlab("speed (m/s)")+
  labs(title = "Distributions of realised and observed speeds for different speed parameters (m/s)")+
  xlim(0,5)

for (i in 1:n_speeds){
  j <- speed_parameters[i]
  obs_real <- obs_real+
    geom_vline(data=df3[df3$speedparameter==j,], aes(xintercept=max_real[i]), colour="red")+
    geom_vline(data=df3[df3$speedparameter==j,], aes(xintercept=max_obs[i]), colour="blue")
}
obs_real

png(file="obs_real.png",
    width=700, height=600)
obs_real
dev.off()


## LOG PDF RATIO

log_pdf_ratio_plot <- ggplot(df2, aes(x = speed_param, y = log_pdf_ratio))+
  geom_point()+
  theme_minimal()+
  labs(x = "speed parameter (m/s)",
       y = "log mean error between observed and realised speeds\n(log(mean(realised PDF / observed PDF)))",
       title = "Log mean ratio between observed and realised speeds for different speed parameters")
log_pdf_ratio_plot

png(file="log_pdf_ratio.png",
    width=700, height=600)
log_pdf_ratio_plot
dev.off()


## DIVERGENCE SCORES

kl_div_plot <- ggplot(df2, aes(x = speed_param, y = kl_divergence))+
  geom_point()+
  theme_minimal()+
  labs(x = "speed parameter (m/s)",
       y = "KL divergence score from realised to observed speeds",
       title = "KL divergence scores from realised to observed speeds for different speed parameters")
kl_div_plot

png(file="kl_divergence.png",
    width=700, height=600)
kl_div_plot
dev.off()


## combined plot for log_pdf_ratio & KL divergence to show M & C:
# make new dataframe:
df4 <- data.frame(speed_param = rep(df2$speed_param, times = 2),
                  score = c(df2$log_pdf_ratio, df2$kl_divergence),
                  method = c(rep("log ratio", times = length(df2$log_pdf_ratio)), rep("KL divergence", times = length(df2$kl_divergence))))


combined <- ggplot(df4, aes(x = speed_param, y = score))+
  geom_point()+
  geom_line()+
  facet_grid(method ~ ., scales = "free")+
  theme_bw()+
  labs(x = "speed parameter (m/s)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        strip.text.y = element_text(size = 15))
combined

png(file="log_pdf_and_kl_div.png",
    width=700, height=600)
combined
dev.off()


# same combined plot but with positive instead of negative scores
df5 <- data.frame(speed_param = rep(df2$speed_param, times = 2),
                  score = c(-df2$log_pdf_ratio, -df2$kl_divergence),
                  method = c(rep("log ratio", times = length(df2$log_pdf_ratio)), rep("KL divergence", times = length(df2$kl_divergence))))


combined2 <- ggplot(df5, aes(x = speed_param, y = score))+
  geom_point()+
  geom_line()+
  facet_grid(method ~ ., scales = "free")+
  theme_bw()+
  labs(x = "speed parameter (m/s)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        strip.text.y = element_text(size = 15))
combined2

png(file="POSITIVE_log_pdf_and_kl_div.png",
    width=700, height=600)
combined2
dev.off()


# calculate estimated speeds - using hmean and SBMs - need to change though#####

harmonic <- (hmean(obs))[1] # harmonic mean estimate
obs_df <- data.frame(speed = obs)
mods <- sbm3(speed~1, obs_df) # fit all the models
lnorm <- predict.sbm(mods[[1]]$lnorm)[1,1] # lnorm speed estimate
gamma <- predict.sbm(mods[[1]]$gamma)[1,1] # gamma speed estimate
weibull <- predict.sbm(mods[[1]]$weibull)[1,1] # weibull speed estimate

# plot
real_df <- data.frame(real = real)
estimates_plot <- ggplot(real_df, aes(x = real))+
  geom_density()+
  theme_minimal()+
  labs(x = "realised speed (m/s)",
       title = "Estimates of mean realised speed overlaying the distribution of realised speeds")+
  theme(legend.position = "bottom")+
  geom_vline(aes(xintercept = density(real)$x[which.max(density(real)$y)], colour = "raw"))+
  geom_vline(aes(xintercept = harmonic, colour = "harmonic"), show.legend = TRUE)+
  geom_vline(aes(xintercept = lnorm, colour = "lnorm"), show.legend = TRUE)+
  geom_vline(aes(xintercept = gamma, colour = "gamma"))+
  geom_vline(aes(xintercept = weibull, colour = "weibull"))+
  scale_color_manual(name = "type of mean", values = c(raw = "black", harmonic = "orange", lnorm = "red", gamma = "blue", weibull = "green"))
estimates_plot

png(file="estimates.png",
    width=700, height=600)
estimates_plot
dev.off()

# errors between estimates and realised speeds
hmean_error <- harmonic - mean(real)
lnorm_error <- lnorm - mean(real)
gamma_error <- gamma - mean(real)
weibull_error <- weibull - mean(real)





