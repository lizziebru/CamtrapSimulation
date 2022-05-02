### compile & analyse data on multiple runs of the same simulation (for one speed at a time) ###

setwd("~/Documents/Project/CamtrapSimulation/code/simulation")

setwd("results2/size0_sd1_pTurn0.5_Cor0.9_kTurn2_kCorT_r9_th0.7") # set which one to analyse

## compile data
real <- c()
obs <- c()
obs_lengths <- c()
n_singles <- 0
n_zeros <- 0
n_points <- 0
for (i in c(1:10)){
  load(paste0("run", i, ".RData"))
  real <- c(real, seq_dats$realised)
  o <- seq_dats$observed
  o[!is.finite(o)] <- NA
  o <- o[!is.na(o)]
  obs <- c(obs, o)
  obs_lengths <- c(obs_lengths, seq_dats$obs_lengths)
  n_singles <- n_singles + seq_dats$n_singles[1]
  n_zeros <- n_zeros + seq_dats$n_zeros[1]
  n_points <- n_points + seq_dats$n_points[1]
}

singles_prop <- n_singles/n_points
zeros_prop <- n_zeros/n_points

obs_real_error <- c()
for (i in 1:length(obs)){
  e <- obs[i] - mean(real)
  obs_real_error <- c(obs_real_error, e)
}


# plotting biases ---------------------------------------------------------

## PLOT: obs_real.png
# comparisons of observed vs realised speeds
obs_real_df <- data.frame(speed = c(real, obs),
                          obs_real = c(rep("realised", length(real)), rep("observed", length(obs))))
obs_real_plot <- ggplot(obs_real_df, aes(x = speed, colour = obs_real))+
  geom_density()+
  theme_minimal()+
  theme(legend.title = element_blank(),
        legend.position = "bottom")+
  scale_colour_manual(values = c("blue", "red"))+
  xlab("speed (m/s)")+
  labs(title = "Distributions of realised and observed speeds")+
  geom_vline(xintercept = density(real)$x[which.max(density(real)$y)], colour = "red")+
  geom_vline(xintercept = density(obs)$x[which.max(density(obs)$y)], colour = "blue")
obs_real_plot

obs_real_error_df <- data.frame(error = obs_real_error)

obs_real_error_plot <- ggplot(obs_real_error_df, aes(x = error))+
  geom_density(size = 1)+
  theme_minimal()+
  labs(x = "error (m/s)",
       title = "Distribution of errors between observed and realised speeds")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(x = -1, y = 0.1, label = "obs > real", size = 3)+
  geom_text(x = 1, y = 0.1, label = "real > obs", size = 3)
obs_real_error_plot

png(file="obs_real.png",
    width=700, height=600)
obs_real_arranged <- ggarrange(obs_real_plot, obs_real_error_plot, nrow = 2)
annotate_figure(obs_real_arranged, top = text_grob(paste("mean realised speed = ", round_dp(mean(real), 3), "m/s, number of observed speeds = ", length(obs), "\nsingle frame prop = ", signif(singles_prop, 3), ", zero frame prop = ", signif(zeros_prop, 3), "\nmean observed sequence length = ", mean(obs_lengths), sep = ""), 
                                                   face = "bold", size = 14))
dev.off()


# calculate estimated speeds - using hmean and SBMs #####

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





