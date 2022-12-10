## MAKING PLOTS ##
# make summary plots using data generated in generate_plotting_variables function

require(ggplot2)
require(ggpubr)
require(colortools)
require(colorBlindness)


# body mass - comparing simulation results with and without body mass scaling of effective detection distance - NOT USED IN MANUSCRIPT---------------------------------------------------------------

# extract the variables you need: first 220 of each is for uni_scaling, second 220 for uni_noscaling
MRS <- c() # 20 reps for each 11 body masses in each of the 7 different types of movement behaviours (= 1540 total)
ar <- c() # ditto
hmean <- c()
lnorm <- c()
gamma <- c()
weibull <- c()
ar_sz <- c()
hmean_sz <- c()
lnorm_sz <- c()
gamma_sz <- c()
weibull_sz <- c()
mov_type <- c()
sz_spds <- c()
n_singles <- c()
n_zeros <- c()
n_detected <- c()

folderpaths <- c("uni_hz_scaling/plotting_variables/", "uni_hz_noscaling/plotting_variables/")

for (f in folderpaths){ # loop through each type movement type
  load(paste0("../results/final_results/", f, "wedge0/Mb_all_iters1-20.RData"))
  for (i in 1:11){ # loop through each body mass
    mb_list <- outputs_mb[[i]] # list of 20 iterations for a given body mass
    for (j in 1:20){ # loop through each iteration
      i_list <- mb_list[[j]] # list of 27 variables for a given iteration
      
      # for some iterations, the models couldn't be fitted: these lists don't contain any variables and just contain the error message
      # check if this is the case:
      test <- 0
      try(test <- i_list$aMRS, silent=TRUE) # try to extract something from the list and ignore any error messages and keep going
      if (test==0){ # if the iteration list contains just errors, test will still be zero because there was nothing to replace it with - in this case just assign everything as NA
        MRS <- c(MRS, NA)
        ar <- c(ar, NA)
        hmean <- c(hmean, NA)
        lnorm <- c(lnorm, NA)
        gamma <- c(gamma, NA)
        weibull <- c(weibull, NA)
        ar_sz <- c(ar_sz, NA)
        hmean_sz <- c(hmean_sz, NA)
        lnorm_sz <- c(lnorm_sz, NA)
        gamma_sz <- c(gamma_sz, NA)
        weibull_sz <- c(weibull_sz, NA)
        mov_type <- c(mov_type, NA)
        sz_spds <- c(sz_spds, NA)
        n_singles <- c(n_singles, NA)
        n_zeros <- c(n_zeros, NA)
        n_detected <- c(n_detected, NA)
      }
      else { # if the iteration list is ok, a value was assigned to test so it won't be zero anymore (aMRS can't physically be zero) - if this is the case then extract all the variables as normal
        MRS <- c(MRS, i_list[["aMRS"]])
        ar <- c(ar, i_list[["amMOS"]])
        hmean <- c(hmean, i_list[["hmean_m"]])
        lnorm <- c(lnorm, i_list[["lnorm_m"]])
        gamma <- c(gamma, i_list[["gamma_m"]])
        weibull <- c(weibull, i_list[["weibull_m"]])
        ar_sz <- c(ar_sz, i_list[["amMOS_sz"]])
        hmean_sz <- c(hmean_sz, i_list[["hmean_m_sz"]])
        lnorm_sz <- c(lnorm_sz, i_list[["lnorm_m_sz"]])
        gamma_sz <- c(gamma_sz, i_list[["gamma_m_sz"]])
        weibull_sz <- c(weibull_sz, i_list[["weibull_m_sz"]])
        sz_spds <- c(sz_spds, (i_list$singles_v_mean+i_list$zeros_v_mean)/2)
        n_singles <- c(n_singles, i_list$n_singles)
        n_zeros <- c(n_zeros, i_list$n_zeros)
        n_detected <- c(n_detected, i_list$n_detected)
        
        # if (f == "uni_hz_scaling/plotting_variables/"){
        #   mov_type <- c(mov_type, "Unimodal")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.1/"){
        #   mov_type <- c(mov_type, "Bimodal: 10% moving")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.25/"){
        #   mov_type <- c(mov_type, "Bimodal: 25% moving")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.4/"){
        #   mov_type <- c(mov_type, "Bimodal: 40% moving")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.6/"){
        #   mov_type <- c(mov_type, "Bimodal: 60% moving")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.75/"){
        #   mov_type <- c(mov_type, "Bimodal: 75% moving")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.9/"){
        #   mov_type <- c(mov_type, "Bimodal: 90% moving")
        # }
        
        
        if (f == "uni_hz_scaling/plotting_variables/"){
          mov_type <- c(mov_type, 0)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.1/"){
          mov_type <- c(mov_type, 0.1)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.25/"){
          mov_type <- c(mov_type, 0.25)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.4/"){
          mov_type <- c(mov_type, 0.4)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.6/"){
          mov_type <- c(mov_type, 0.6)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.75/"){
          mov_type <- c(mov_type, 0.7)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.9/"){
          mov_type <- c(mov_type, 0.9)
        }
      }
    }
  }
}


radial_dists <- c()
mb <- c()
sampled_no <- c()

folderpaths <- c("uni_hz_scaling/seq_dats/", "uni_hz_noscaling/seq_dats/")
mb_range <- c(1,5,10,15,20,25,30,35,40,45,50)

for (f in folderpaths){ # loop through each type movement type
  for (i in mb_range){ # loop through each body mass
    for (j in 1:20){ # loop through each iteration
      load(paste0("../results/final_results/", f, "Mb", i, "iter", j, ".RData"))
      posdat <- seq_dats$posdat
      x <- posdat$x
      y <- posdat$y
      spds <- na.omit(seq_dats$v$speed)
      sampled_no <- c(sampled_no, length(spds))
      mb <- c(mb, i)
      radial_dists <- c(radial_dists, mean(sqrt((x-20)^2+(y-10)^2)))
    }
  }
}



## new plot: to show how prop of singles & zeros stays constant across body masses bc just depends on dz dimensions
missed_df <- data.frame(mb=mb,
                        scaling=c(rep("Yes", times=220), rep("No", times=220)),
                        # mb = c(rep(c(rep(1,times=20), rep(5, times=20), rep(10,times=20), rep(15,times=20), rep(20,times=20), rep(25,times=20), rep(30,times=20), rep(35,times=20), rep(40,times=20), rep(45,times=20), rep(50,times=20)), times=7)),
                        dz = radial_dists,
                        mrs=MRS,
                        missed_prop = c(sampled_no/(n_singles+n_zeros+sampled_no)))
missed_df <- na.omit(missed_df)

df <- data.frame(mb=mb,
                 scaling=c(rep("Yes", times=220), rep("No", times=220)),
                 singles=n_singles,
                 zeros=n_zeros,
                 sampled_no=sampled_no)
df <- na.omit(df)
ggplot(df, aes(x=mb, y=zeros, colour=scaling))+geom_point()

mb_missed <- ggplot(missed_df, aes(x=mb, y=missed_prop))+
  geom_point(size=0.1, aes(colour=scaling))+
  geom_smooth(alpha = 0.2, se=F, aes(colour=scaling))+
  theme_bw()+
  # scale_colour_manual(values=c("#FF0099"))+
  labs(x = "Body mass (kg)",
       y = "Proportion sampled speeds")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        # legend.key = element_blank(),
        # legend.title = element_blank(),
        # legend.text = element_blank(),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))
guides(col=guide_legend("Estimation method"))
mb_missed

mb_speed <- ggplot(missed_df, aes(x=mb, y=mrs))+
  geom_point(size=0.1, aes(colour=scaling))+
  geom_smooth(alpha = 0.2, se=F, aes(colour=scaling))+
  theme_bw()+
  # scale_colour_manual(values=c("#FF0099"))+
  labs(x = "Body mass (kg)",
       y = "True mean travel speed (m/s)")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Scaled with body mass?"))
mb_speed

mb_dz <- ggplot(missed_df, aes(x=mb, y=dz))+
  geom_point(size=0.1, aes(colour=scaling))+
  geom_smooth(alpha = 0.2, se=F, aes(colour=scaling))+
  theme_bw()+
  # scale_colour_manual(values=c("#FF0099"))+
  labs(x = "Body mass (kg)",
       y = "Mean detection distance (m)")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Scaled with body mass?"))
mb_dz

fig_mb_missed <- ggarrange(mb_speed + rremove("xlab"), mb_dz + rremove("xlab"), mb_missed, nrow=3)

png(file=paste0("../results/final_results/plots/fig_mb_missed.png"), width = 7, height = 8, units = 'in', res = 300)
print(fig_mb_missed)
dev.off()

# extract radial distances and number of sampled speeds for each path ----------------------------------

radial_dists <- c()
mb <- c()
sampled_no <- c()

folderpaths <- c("uni_hz_scaling/seq_dats/", "bi_hz_scaling/seq_dats/mov0.1/", "bi_hz_scaling/seq_dats/mov0.25/", "bi_hz_scaling/seq_dats/mov0.4/", "bi_hz_scaling/seq_dats/mov0.6/", "bi_hz_scaling/seq_dats/mov0.75/", "bi_hz_scaling/seq_dats/mov0.9/")
mb_range <- c(1,5,10,15,20,25,30,35,40,45,50)

for (f in folderpaths){ # loop through each type movement type
  for (i in mb_range){ # loop through each body mass
    for (j in 1:20){ # loop through each iteration
      load(paste0("../results/final_results/", f, "Mb", i, "iter", j, ".RData"))
      posdat <- seq_dats$posdat
      x <- posdat$x
      y <- posdat$y
      spds <- na.omit(seq_dats$v$speed)
      sampled_no <- c(sampled_no, length(spds))
      mb <- c(mb, i)
      radial_dists <- c(radial_dists, mean(sqrt((x-20)^2+(y-10)^2)))
    }
  }
}
  






# FIGURES 1 & 2 -----------------------------------------

# extract the variables you need:
MRS <- c() # 20 reps for each 11 body masses in each of the 7 different types of movement behaviours (= 1540 total)
ar <- c() # ditto
hmean <- c()
lnorm <- c()
gamma <- c()
weibull <- c()
ar_sz <- c()
hmean_sz <- c()
lnorm_sz <- c()
gamma_sz <- c()
weibull_sz <- c()
mov_type <- c()
sz_spds <- c()
n_singles <- c()
n_zeros <- c()
n_detected <- c()

folderpaths <- c("uni_hz_scaling/plotting_variables/", "bi_hz_scaling/plotting_variables/mov0.1/", "bi_hz_scaling/plotting_variables/mov0.25/", "bi_hz_scaling/plotting_variables/mov0.4/", "bi_hz_scaling/plotting_variables/mov0.6/", "bi_hz_scaling/plotting_variables/mov0.75/", "bi_hz_scaling/plotting_variables/mov0.9/")

for (f in folderpaths){ # loop through each type movement type
  load(paste0("../results/final_results/", f, "wedge0/Mb_all_iters1-20.RData"))
  for (i in 1:11){ # loop through each body mass
    mb_list <- outputs_mb[[i]] # list of 20 iterations for a given body mass
    for (j in 1:20){ # loop through each iteration
      i_list <- mb_list[[j]] # list of 27 variables for a given iteration
      
      # for some iterations, the models couldn't be fitted: these lists don't contain any variables and just contain the error message
      # check if this is the case:
      test <- 0
      try(test <- i_list$aMRS, silent=TRUE) # try to extract something from the list and ignore any error messages and keep going
      if (test==0){ # if the iteration list contains just errors, test will still be zero because there was nothing to replace it with - in this case just assign everything as NA
        MRS <- c(MRS, NA)
        ar <- c(ar, NA)
        hmean <- c(hmean, NA)
        lnorm <- c(lnorm, NA)
        gamma <- c(gamma, NA)
        weibull <- c(weibull, NA)
        ar_sz <- c(ar_sz, NA)
        hmean_sz <- c(hmean_sz, NA)
        lnorm_sz <- c(lnorm_sz, NA)
        gamma_sz <- c(gamma_sz, NA)
        weibull_sz <- c(weibull_sz, NA)
        mov_type <- c(mov_type, NA)
        sz_spds <- c(sz_spds, NA)
        n_singles <- c(n_singles, NA)
        n_zeros <- c(n_zeros, NA)
        n_detected <- c(n_detected, NA)
      }
      else { # if the iteration list is ok, a value was assigned to test so it won't be zero anymore (aMRS can't physically be zero) - if this is the case then extract all the variables as normal
        MRS <- c(MRS, i_list[["aMRS"]])
        ar <- c(ar, i_list[["amMOS"]])
        hmean <- c(hmean, i_list[["hmean_m"]])
        lnorm <- c(lnorm, i_list[["lnorm_m"]])
        gamma <- c(gamma, i_list[["gamma_m"]])
        weibull <- c(weibull, i_list[["weibull_m"]])
        ar_sz <- c(ar_sz, i_list[["amMOS_sz"]])
        hmean_sz <- c(hmean_sz, i_list[["hmean_m_sz"]])
        lnorm_sz <- c(lnorm_sz, i_list[["lnorm_m_sz"]])
        gamma_sz <- c(gamma_sz, i_list[["gamma_m_sz"]])
        weibull_sz <- c(weibull_sz, i_list[["weibull_m_sz"]])
        sz_spds <- c(sz_spds, (i_list$singles_v_mean+i_list$zeros_v_mean)/2)
        n_singles <- c(n_singles, i_list$n_singles)
        n_zeros <- c(n_zeros, i_list$n_zeros)
        n_detected <- c(n_detected, i_list$n_detected)
        
        # if (f == "uni_hz_scaling/plotting_variables/"){
        #   mov_type <- c(mov_type, "Unimodal")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.1/"){
        #   mov_type <- c(mov_type, "Bimodal: 10% moving")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.25/"){
        #   mov_type <- c(mov_type, "Bimodal: 25% moving")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.4/"){
        #   mov_type <- c(mov_type, "Bimodal: 40% moving")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.6/"){
        #   mov_type <- c(mov_type, "Bimodal: 60% moving")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.75/"){
        #   mov_type <- c(mov_type, "Bimodal: 75% moving")
        # }
        # if (f == "bi_hz_scaling/plotting_variables/mov0.9/"){
        #   mov_type <- c(mov_type, "Bimodal: 90% moving")
        # }
        
        
        if (f == "uni_hz_scaling/plotting_variables/"){
          mov_type <- c(mov_type, 0)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.1/"){
          mov_type <- c(mov_type, 0.1)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.25/"){
          mov_type <- c(mov_type, 0.25)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.4/"){
          mov_type <- c(mov_type, 0.4)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.6/"){
          mov_type <- c(mov_type, 0.6)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.75/"){
          mov_type <- c(mov_type, 0.7)
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.9/"){
          mov_type <- c(mov_type, 0.9)
        }
      }
    }
  }
}

# before: -5
# after: -3
# improvement: after-before = -3--5=-3+5=+2

# another new plot: improvement of bias of lnorm & hmean when correct sampling bias: + also t-test!

improv_df <- data.frame(improv = c(((hmean_sz-MRS)/MRS-(hmean-MRS)/MRS), ((lnorm_sz-MRS)/MRS-(lnorm-MRS)/MRS), ((gamma_sz-MRS)/MRS-(gamma-MRS)/MRS), ((weibull_sz-MRS)/MRS-(weibull-MRS)/MRS)),
                        MRS=rep(MRS, times=4),
                        Method = factor(c(rep("Harmonic", times=1540), rep("Log-normal", times=1540), rep("Gamma", times=1540), rep("Weibull", times=1540)), levels=c("Harmonic", "Log-normal", "Gamma", "Weibull")))
improv_df <- na.omit(improv_df)

improv <- ggplot(improv_df, aes(x=MRS, y=improv))+
  geom_point(aes(colour=Method), size=0.1)+
  geom_smooth(aes(colour=Method), alpha = 0.2, se=F)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  labs(x = "True travel speed (m/s)",
       y = "Improvement [bias after - bias before correction]")+
  # ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Type of mean"))
improv

# combine into dataframe
improv2_df <- data.frame(bias = c((ar-MRS)/MRS, (hmean-MRS)/MRS, (lnorm-MRS)/MRS, (gamma-MRS)/MRS, (weibull-MRS)/MRS, (ar_sz-MRS)/MRS, (hmean_sz-MRS)/MRS, (lnorm_sz-MRS)/MRS, (gamma_sz-MRS)/MRS, (weibull_sz-MRS)/MRS),
                        MRS = c(rep(MRS, times=10)),
                        mov_type = c(rep(mov_type, times=10)),
                        type = factor(c(rep("Sampling bias not corrected", times=7700), rep("Sampling bias corrected", times=7700)), levels = c("Sampling bias not corrected", "Sampling bias corrected")),
                        Method = factor(c(rep(c(rep("Uncorrected", times=1540), rep("Harmonic", times=1540), rep("Log-normal", times=1540), rep("Gamma", times=1540), rep("Weibull", times=1540)), times=2)), levels = c("Uncorrected", "Harmonic", "Log-normal", "Gamma", "Weibull")))
improv2_df <- na.omit(improv2_df) # remove rows where bias and MRS are NA

improv2 <- ggplot(improv2_df, aes(x=MRS, y=bias))+
  geom_point(aes(colour=Method), size=0.1)+
  facet_grid(~ type)+
  geom_smooth(aes(colour=Method), alpha = 0.2, se=F)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "True mean travel speed (m/s)",
       y = "Bias [(estimated mean/true mean) - 1]")+
  ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Encountering bias\ncorrection"))
improv2

# do the same as improv2 but with labels A and B so need to do separately:
improv_nocorrection <- ggplot(improv2_df[improv2_df$type=="Sampling bias not corrected",], aes(x=MRS, y=bias))+
  geom_point(aes(colour=Method), size=0.1)+
  facet_grid(~ type)+
  geom_smooth(aes(colour=Method), alpha = 0.2, se=F)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "True travel speed (m/s)",
       y = "Bias [(estimated mean/true mean) - 1]")+
  ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=10),
        legend.text = element_text(size = 10),
        title = element_text(size = 13),
        strip.text = element_text(size = 12),
        # legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Encountering\nbias correction"))
improv_nocorrection

improv_correction <- ggplot(improv2_df[improv2_df$type=="Sampling bias corrected",], aes(x=MRS, y=bias))+
  geom_point(aes(colour=Method), size=0.1)+
  facet_grid(~ type)+
  geom_smooth(aes(colour=Method), alpha = 0.2, se=F)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_shape_manual(values = c(17, 16, 16, 16, 16))+ 
  scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "True travel speed (m/s)",
       y = "Bias [(estimated mean/true mean) - 1]")+
  ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=10),
        legend.text = element_text(size = 10),
        title = element_text(size = 13),
        strip.text = element_text(size = 12),
        # legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Encountering\nbias correction"))
improv_correction


require(grid)   # for the textGrob() function

improv_arranged <- ggarrange(improv_nocorrection + rremove("ylab") + rremove("xlab"), improv_correction + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
                    labels = c("A", "B"),
                    ncol = 2,
                    common.legend = TRUE, legend = "right")
                    # align = "hv", 
                    # font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

improv_annotated <- annotate_figure(improv_arranged, left = textGrob("Bias [(estimated mean/true travel speed) - 1]", rot = 90, vjust = 1, gp = gpar(cex = 1.2)),
                bottom = textGrob("True travel speed (m/s)", hjust=0.7, gp = gpar(cex = 1.2)))
improv_annotated

png(file=paste0("../results/final_results/plots/both_biases_correction.png"), width = 7, height = 4.8, units = 'in', res = 300)
print(improv_annotated)
dev.off()

png(file=paste0("../results/final_results/plots/improv.png"), width = 7, height = 5, units = 'in', res = 300)
print(improv)
dev.off()

png(file=paste0("../results/final_results/plots/improv2.png"), width = 7, height = 5, units = 'in', res = 300)
print(improv2)
dev.off()


# t-test for improvement:
hmean_improv <- na.omit((hmean_sz-MRS)/MRS-(hmean-MRS)/MRS)
t.test(hmean_improv, alternative="greater")
lnorm_improv <- na.omit((lnorm_sz-MRS)/MRS-(lnorm-MRS)/MRS)
t.test(lnorm_improv, alternative="greater")

gamma_improv <- na.omit((gamma_sz-MRS)/MRS-(gamma-MRS)/MRS)
t.test(gamma_improv, alternative="greater")
weibull_improv <- na.omit((weibull_sz-MRS)/MRS-(weibull-MRS)/MRS)
t.test(weibull_improv)



hmean_bias <- na.omit(hmean-MRS)
t.test(hmean_bias)

gamma_bias <- na.omit(gamma-MRS)
t.test(gamma_bias)

## new plot: to show how prop of singles & zeros stays constant across body masses bc just depends on dz dimensions
missed_df <- data.frame(mb=mb,
                        # mb = c(rep(c(rep(1,times=20), rep(5, times=20), rep(10,times=20), rep(15,times=20), rep(20,times=20), rep(25,times=20), rep(30,times=20), rep(35,times=20), rep(40,times=20), rep(45,times=20), rep(50,times=20)), times=7)),
                        dz = radial_dists,
                        mrs=MRS,
                        missed_prop = c(sampled_no/(n_singles+n_zeros+sampled_no)))

mb_missed <- ggplot(missed_df, aes(x=mb, y=missed_prop))+
  geom_point(size=0.1)+
  geom_smooth(alpha = 0.2, se=F, colour="black")+
  theme_bw()+
  # scale_colour_manual(values=c("#FF0099"))+
  labs(x = "Body mass (kg)",
       y = "Proportion sampled speeds")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        # legend.key = element_blank(),
        # legend.title = element_blank(),
        # legend.text = element_blank(),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))
  guides(col=guide_legend("Estimation method"))
mb_missed

mb_speed <- ggplot(missed_df, aes(x=mb, y=mrs))+
  geom_point(size=0.1)+
  geom_smooth(alpha = 0.2, se=F, colour="black")+
  theme_bw()+
  # scale_colour_manual(values=c("#FF0099"))+
  labs(x = "Body mass (kg)",
       y = "True travel speed (m/s)")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
mb_speed

mb_dz <- ggplot(missed_df, aes(x=mb, y=dz))+
  geom_point(size=0.1)+
  geom_smooth(alpha = 0.2, se=F, colour="black")+
  theme_bw()+
  # scale_colour_manual(values=c("#FF0099"))+
  labs(x = "Body mass (kg)",
       y = "Mean detection distance (m)")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
mb_dz

fig_mb_missed <- ggarrange(mb_speed + rremove("xlab"), mb_dz + rremove("xlab"), mb_missed, nrow=3)

png(file=paste0("../results/final_results/plots/fig_mb_missed.png"), width = 7, height = 8, units = 'in', res = 300)
print(fig_mb_missed)
dev.off()

# anova for body mass and prop of sampled frames:
mb_aov <- aov(missed_prop ~ mb, data = missed_df)
# Summary of the analysis
summary(mb_aov)




# top plot: x axis = true travel speed

# combine into dataframe
fig1_1_df <- data.frame(bias = c((ar-MRS)/MRS, (hmean-MRS)/MRS, (lnorm-MRS)/MRS, (gamma-MRS)/MRS, (weibull-MRS)/MRS, (ar_sz-MRS)/MRS, (hmean_sz-MRS)/MRS, (lnorm_sz-MRS)/MRS, (gamma_sz-MRS)/MRS, (weibull_sz-MRS)/MRS),
                      MRS = c(rep(MRS, times=10)),
                      mov_type = c(rep(mov_type, times=10)),
                      type = factor(c(rep("Sampling bias not corrected", times=7700), rep("Sampling bias corrected", times=7700)), levels = c("Sampling bias not corrected", "Sampling bias corrected")),
                      Method = factor(c(rep(c(rep("Uncorrected", times=1540), rep("Harmonic", times=1540), rep("Log-normal", times=1540), rep("Gamma", times=1540), rep("Weibull", times=1540)), times=2)), levels = c("Uncorrected", "Harmonic", "Log-normal", "Gamma", "Weibull")))
fig1_1_df <- na.omit(fig1_1_df) # remove rows where bias and MRS are NA

## glm:
glm_movtype <- glm(bias ~ mov_type + Method + type, data=fig1_1_df)
summary(glm_movtype)

fig1_1 <- ggplot(fig1_1_df, aes(x=mov_type, y=bias))+
  geom_point(aes(colour=Method), size=0.1)+
  facet_grid(~ type)+
  geom_smooth(aes(colour=Method), alpha = 0.2, se=F)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "True travel speed (m/s)",
       y = "Bias [(estimated mean/true mean) - 1]")+
  ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
fig1_1
cvdPlot(fig1_1) # to simulate colour-blindness - use this + wheel("red", 8) from colortools package to trial different sets of contrasting colours


# now with mov_type on x axis
fig1_3 <- ggplot(fig1_1_df, aes(x=mov_type, y=bias))+
  geom_point(aes(colour=Method), size=0.1)+
  geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5), aes(colour=Method), alpha=0.2)+
  facet_grid(~ type)+
  # geom_smooth(aes(colour=Method), alpha = 0.2)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "Proportion fast behaviour",
       y = "Bias [(estimated mean/true mean) - 1]")+
  ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
fig1_3



## now just missing bias not corrected (panels showing travel speeds vs diff movement types)
# make plot for just travel speed without missing bias corrected:
fig1_1 <- ggplot(fig1_1_df[fig1_1_df$type=="Sampling bias not corrected",], aes(x=MRS, y=bias))+
  geom_point(aes(colour=Method), size=0.1)+
  # facet_grid(~ type)+
  geom_smooth(aes(colour=Method), alpha = 0.2, se=F)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "True mean travel speed (m/s)",
       y = "Bias [(estimated mean/true mean) - 1]")+
  # ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Type of mean"))
fig1_1 #--> currently using for figure 1!!

png(file=paste0("../results/final_results/plots/fig1_new.png"), width = 7, height = 5, units = 'in', res = 300)
print(fig1_1)
dev.off()


fig1_2 <- ggplot(fig1_1_df[fig1_1_df$type=="Missing bias not corrected",], aes(x=mov_type, y=bias))+
  geom_point(aes(colour=Method), size=0.1)+
  # facet_grid(~ type)+
  geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5), aes(colour=Method), alpha=0.2, se=F)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "Proportion fast behaviour",
       y = "Bias [(estimated mean/true mean) - 1]")+
  # ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
fig1_2

fig1a <- ggarrange(fig1_1, fig1_2 + rremove("ylab"),
                    labels = c("A", "B"),
                    ncol = 2,
                    common.legend = TRUE, legend = "right",
                    align = "hv")
                    # font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

png(file=paste0("../results/final_results/plots/fig1_ts+prop.png"), width = 7, height = 3.9, units = 'in', res = 300)
print(fig1a)
dev.off()



## new plot to show missed & sampled speeds:
fig1_4_df <- data.frame(bias = c((ar-MRS)/MRS, (sz_spds-MRS)/MRS),
                        MRS = c(rep(MRS, times=2)),
                        type = factor(c(rep("Sampled", times=1540), rep("Unsampled", times=1540)), levels = c("Sampled", "Unsampled")))
                        
fig1_4_df <- na.omit(fig1_4_df) # remove rows where bias and MRS are NA

fig1_4 <- ggplot(fig1_4_df, aes(x=MRS, y=bias))+
  geom_point(aes(colour=type), size=0.1)+
  # facet_grid(~ type)+
  geom_smooth(aes(colour=type), alpha = 0.2, se=F)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("black", "#FF0099"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "True travel speed (m/s)",
       y = "Bias [(estimated mean/true mean) - 1]")+
  # ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        # legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.position = c(0.855, 0.884),
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
fig1_4

png(file=paste0("../results/final_results/plots/fig2_missed.png"), width = 5, height = 3.8, units = 'in', res = 300)
print(fig1_4)
dev.off()

## new plots to show the 2 component biases (new figure 2):
# sampled vs encountered:
samp_spds <- ar
enc_spds <- (ar+sz_spds)/2
samp_enc_df <- data.frame(bias=c((samp_spds-enc_spds)/enc_spds),
                          enc=enc_spds)
samp_enc_df <- na.omit(samp_enc_df) # remove rows where bias and MRS are NA

samp_enc <- ggplot(samp_enc_df, aes(x=enc, y=bias))+
  geom_point(size=0.1, aes(colour=c("#FF0099")))+
  geom_smooth(alpha = 0.2, se=F, aes(colour=c("#FF0099")))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("#FF0099"))+
  labs(x = "Mean encountered speed (m/s)",
       y = "Bias [(sampled mean/encountered mean) - 1]")+
  ylim(c(-0.5, 2.5))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        # legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
samp_enc


enc_mrs_df <- data.frame(bias=c((enc_spds-MRS)/MRS),
                          mrs=MRS)
enc_mrs_df <- na.omit(enc_mrs_df) # remove rows where bias and MRS are NA

enc_mrs <- ggplot(enc_mrs_df, aes(x=mrs, y=bias))+
  geom_point(size=0.1, aes(colour=c("#0000FF")))+
  geom_smooth(alpha = 0.2, se=F, aes(colour=c("#0000FF")))+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("#0000FF"))+
  labs(x = "True mean travel speed (m/s)",
       y = "Bias [(encountered mean/true mean) - 1]")+
  ylim(c(-0.5, 2.5))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        # legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
enc_mrs

ggarrange(samp_enc, enc_mrs)


## plot them together instead:
enc_samp_mrs_df <- data.frame(bias = c((enc_spds-MRS)/MRS, (samp_spds-MRS)/MRS),
                        MRS = c(rep(MRS, times=2)),
                        type = factor(c(rep("Encountered", times=1540), rep("Sampled", times=1540)), levels = c("Encountered", "Sampled")))

enc_samp_mrs_df<- na.omit(enc_samp_mrs_df) # remove rows where bias and MRS are NA

enc_samp_mrs<- ggplot(enc_samp_mrs_df, aes(x=MRS, y=bias))+
  geom_point(aes(colour=type), size=0.1)+
  # facet_grid(~ type)+
  geom_smooth(aes(colour=type), alpha = 0.2, se=F)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("#0000FF", "#FF0099"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "True travel speed (m/s)",
       y = "Bias [(mean/true travel speed) - 1]")+
  # ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        # legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.position = c(0.84, 0.88),
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
enc_samp_mrs

png(file=paste0("../results/final_results/plots/enc_samp_mrs.png"), width = 5, height = 3.8, units = 'in', res = 400)
print(enc_samp_mrs)
dev.off()





# to show that movement type wasn't important, just do separate lm & anova for each:
hmean_df <- data.frame(bias = c((hmean-MRS)/MRS),
                    mov_type=mov_type)
hmean_lm <- lm(bias~mov_type, data=hmean_df)
summary(hmean_lm)

ggplot(hmean_df, aes(x=mov_type, y=bias))+geom_point()
anova(hmean_lm)
#--> actually maybe just keep it simple and stick with plots







# bottom plot: x axis = behaviour

# work out one mean bias per estimation method for each of the 7 movement behaviours:
ar_meanbias <- c()
h_meanbias <- c()
l_meanbias <- c()
g_meanbias <- c()
w_meanbias <- c()
ar_sz_meanbias <- c()
h_sz_meanbias <- c()
l_sz_meanbias <- c()
g_sz_meanbias <- c()
w_sz_meanbias <- c()

for (i in 1:7){ # for each movement behaviour, select the relevant chunk, work out all the biases, and average them out
  selection <- (220*(i-1)+1):(220*i)
  mrs <- na.omit(MRS[selection])
  a <- na.omit(ar[selection])
  h <- na.omit(hmean[selection])
  l <- na.omit(lnorm[selection])
  g <- na.omit(gamma[selection])
  w <- na.omit(weibull[selection])
  a_sz <- na.omit(ar_sz[selection])
  h_sz <- na.omit(hmean_sz[selection])
  l_sz <- na.omit(lnorm_sz[selection])
  g_sz <- na.omit(gamma_sz[selection])
  w_sz <- na.omit(weibull_sz[selection])
  
  ar_meanbias <- c(ar_meanbias, mean((a-mrs)/mrs))
  h_meanbias <- c(h_meanbias, mean((h-mrs)/mrs))
  l_meanbias <- c(l_meanbias, mean((l-mrs)/mrs))
  g_meanbias <- c(g_meanbias, mean((g-mrs)/mrs))
  w_meanbias <- c(w_meanbias, mean((w-mrs)/mrs))
  ar_sz_meanbias <- c(ar_sz_meanbias, mean((a_sz-mrs)/mrs))
  h_sz_meanbias <- c(h_sz_meanbias, mean((h_sz-mrs)/mrs))
  l_sz_meanbias <- c(l_sz_meanbias, mean((l_sz-mrs)/mrs))
  g_sz_meanbias <- c(g_sz_meanbias, mean((g_sz-mrs)/mrs))
  w_sz_meanbias <- c(w_sz_meanbias, mean((w_sz-mrs)/mrs))
  
}


fig1_2_df <- data.frame(bias = c(ar_meanbias, h_meanbias, l_meanbias, g_meanbias, w_meanbias, 
                               ar_sz_meanbias, h_sz_meanbias, l_sz_meanbias, g_sz_meanbias, w_sz_meanbias),
                      mov_behav = rep(c(0,0.1,0.25,0.4,0.6,0.75,0.9), times=10),
                      type = factor(c(rep("Missing bias not corrected", times=35), rep("Missing bias corrected", times=35)), levels = c("Missing bias not corrected", "Missing bias corrected")),
                      Method = factor(c(rep("Arithmetic mean", times=7), rep("Harmonic mean", times=7), rep("Log-normal", times=7), rep("Gamma", times=7), rep("Weibull", times=7), rep("Arithmetic mean", times=7), rep("Harmonic mean", times=7), rep("Log-normal", times=7), rep("Gamma", times=7), rep("Weibull", times=7)), levels = c("Arithmetic mean", "Harmonic mean", "Log-normal", "Gamma", "Weibull")))

# make plot
fig1_2 <- ggplot(fig1_2_df, aes(x=mov_behav, y=bias))+
  geom_point(aes(colour=Method), size=2)+
  facet_grid(~ type)+
  geom_smooth(aes(colour=Method), alpha = 0.2)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "Proportion fast behaviour",
       y = "Mean bias")+
  # ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
fig1_2

fig1comb <- ggarrange(fig1_1, fig1_2, nrow = 2, common.legend = TRUE, legend = "right", labels = c("A", "B"))

# save:
png(file=paste0("../results/final_results/plots/fig1comb.png"), width = 7, height = 7.4, units = 'in', res = 280)
print(fig1comb)
dev.off()
# too big - separate into figures 1 and 2


png(file=paste0("../results/final_results/plots/fig1.png"), width = 7, height = 4.2, units = 'in', res = 280)
print(fig1_1)
dev.off()

png(file=paste0("../results/final_results/plots/fig2.png"), width = 7, height = 4.7, units = 'in', res = 280)
print(fig1_2)
dev.off()



# diff parts of the wedge: fig 3 ---------------------------------

# need:

MRS <- c() # 20 reps for each 11 body masses in each of the 7 different types of movement behaviours (= 1540 total)
ar <- c() # ditto
hmean <- c()
lnorm <- c()
gamma <- c()
weibull <- c()
ar_sz <- c()
hmean_sz <- c()
lnorm_sz <- c()
gamma_sz <- c()
weibull_sz <- c()
n_s <- c()
n_z <- c()
s_v_mean <- c()
z_v_mean <- c()
s_prop <- c() # no. of singles / no. of detected points
z_prop <- c() # no. of zeros / no. of detected points
wedge <- c()
n_seqs_det <- c()

wedges <- c(1,2,3)

for (w in wedges){
  load(paste0("../results/final_results/uni_hz_scaling/plotting_variables/wedge", w, "/Mb_all_iters1-20.RData"))
  for (i in 1:11){ # loop through each body mass
    mb_list <- outputs_mb[[i]] # list of 20 iterations for a given body mass
    for (j in 1:20){ # loop through each iteration
      i_list <- mb_list[[j]] # list of 27 variables for a given iteration
      
      # for some iterations, the models couldn't be fitted: these lists don't contain any variables and just contain the error message
      # check if this is the case:
      test <- 0
      try(test <- i_list$aMRS, silent=TRUE) # try to extract something from the list and ignore any error messages and keep going
      if (test==0){ # if the iteration list contains just errors, test will still be zero because there was nothing to replace it with - in this case just assign everything as NA
        MRS <- c(MRS, NA)
        ar <- c(ar, NA)
        hmean <- c(hmean, NA)
        lnorm <- c(lnorm, NA)
        gamma <- c(gamma, NA)
        weibull <- c(weibull, NA)
        ar_sz <- c(ar_sz, NA)
        hmean_sz <- c(hmean_sz, NA)
        lnorm_sz <- c(lnorm_sz, NA)
        gamma_sz <- c(gamma_sz, NA)
        weibull_sz <- c(weibull_sz, NA)
        s_v_mean <- c(s_v_mean, NA)
        z_v_mean <- c(z_v_mean, NA)
        n_s <- c(n_s, NA)
        n_z <- c(n_z, NA)
        wedge <- c(wedge, NA)
        n_seqs_det <- c(n_seqs_det, NA)
      }
      else { # if the iteration list is ok, a value was assigned to test so it won't be zero anymore (aMRS can't physically be zero) - if this is the case then extract all the variables as normal
        MRS <- c(MRS, i_list[["aMRS"]])
        ar <- c(ar, i_list[["amMOS"]])
        hmean <- c(hmean, i_list[["hmean_m"]])
        lnorm <- c(lnorm, i_list[["lnorm_m"]])
        gamma <- c(gamma, i_list[["gamma_m"]])
        weibull <- c(weibull, i_list[["weibull_m"]])
        ar_sz <- c(ar_sz, i_list[["amMOS_sz"]])
        hmean_sz <- c(hmean_sz, i_list[["hmean_m_sz"]])
        lnorm_sz <- c(lnorm_sz, i_list[["lnorm_m_sz"]])
        gamma_sz <- c(gamma_sz, i_list[["gamma_m_sz"]])
        weibull_sz <- c(weibull_sz, i_list[["weibull_m_sz"]])
        n_s <- c(n_s, i_list$n_singles)
        n_z <- c(n_z, i_list$n_zeros)
        n_seqs_det <- c(n_seqs_det, i_list$n_seqs_det)
        s_v_mean <- c(s_v_mean, i_list$singles_v_mean)
        z_v_mean <- c(z_v_mean, i_list$zeros_v_mean)
        
        s_n <- i_list[["n_singles"]] # no. of singles
        z_n <- i_list[["n_zeros"]] # no. of zeros
        n_det <- i_list[["n_detected"]] # no. of detected points
        
        s_prop <- c(s_prop, s_n/n_det)
        z_prop <- c(z_prop, z_n/n_det)
        
        wedge <- c(wedge, w)
      }
    }
  }
}

props_spds_wedge_df <- data.frame(wedge=factor(c(rep("Bottom", times=220), rep("Middle", times=220), rep("Top", times=220)), levels=c("Top", "Middle", "Bottom")),
                                  prop_sampled = c(n_seqs_det/(n_s+n_z+n_seqs_det)),
                                  mean_missed_spd = c((s_v_mean+z_v_mean)/2))
props_spds_wedge_df <- na.omit(props_spds_wedge_df)

wedge_spds <- ggplot(props_spds_wedge_df, aes(x=mean_missed_spd))+
  # geom_density(aes(colour = type))+
  stat_density(aes(colour=wedge),
               geom="line",position="identity", size=1.1)+
  theme_bw()+
  scale_colour_manual(name = "",
                      labels = c("Top", "Middle", "Bottom"),
                      values = c("#FF0099", "#0000FF", "black"))+
  labs(x = "Sampled speed (m/s)",
       y = "Density")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = c(0.86,0.83),
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))
wedge_spds

png(file=paste0("../results/final_results/plots/wedge_spds.png"), width = 5, height = 3.5, units = 'in', res = 300)
print(wedge_spds)
dev.off()

mean(props_spds_wedge_df[props_spds_wedge_df$wedge=="Bottom",]$prop_sampled)
mean(props_spds_wedge_df[props_spds_wedge_df$wedge=="Middle",]$prop_sampled)
mean(props_spds_wedge_df[props_spds_wedge_df$wedge=="Top",]$prop_sampled)


# plotting bias for each wedge:

# fig3_df <- data.frame(bias = c((ar-MRS)/MRS, (hmean-MRS)/MRS, (lnorm-MRS)/MRS, (gamma-MRS)/MRS, (weibull-MRS)/MRS),
#                       MRS = c(rep(MRS, times=5)),
#                       type = factor(c(rep("Bottom", times=220), rep("Middle", times=220), rep("Top", times=220)), levels = c("Bottom", "Middle", "Top")),
#                       Method = factor(c(rep("Arithmetic mean", times=660), rep("Harmonic mean", times=660), rep("Log-normal", times=660), rep("Gamma", times=660), rep("Weibull", times=660)), levels = c("Arithmetic mean", "Harmonic mean", "Log-normal", "Gamma", "Weibull")))
# fig3_df <- na.omit(fig3_df)

# fig3_df <- data.frame(bias = c((hmean-MRS)/MRS, (lnorm-MRS)/MRS),
#                       MRS = c(rep(MRS, times=2)),
#                       type = factor(c(rep(c(rep("Bottom", times=220), rep("Middle", times=220), rep("Top", times=220)), times=2)), levels = c("Bottom", "Middle", "Top")),
#                       Method = factor(c(rep("Harmonic mean", times=660), rep("Log-normal", times=660)), levels = c("Harmonic mean", "Log-normal")))
# fig3_df <- na.omit(fig3_df)
# 
# fig3 <- ggplot(fig3_df, aes(x=MRS, y=bias))+
#   geom_point(aes(colour=Method), size=0.1)+
#   facet_grid(~ type)+
#   geom_smooth(aes(colour=Method), alpha = 0.2)+
#   theme_bw()+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   scale_colour_manual(values = c("#FF0099", "#0000FF"))+
#   labs(x = "True travel speed (m/s)",
#        y = "Bias [(estimated travel speed/true travel speed) - 1]")+
#   ylim(c(-0.5, 1))+
#   theme(axis.title = element_text(size=13),
#         axis.text = element_text(size = 11),
#         legend.title = element_text(size=11),
#         legend.text = element_text(size = 11),
#         title = element_text(size = 13),
#         strip.text = element_text(size = 13),
#         # legend.position = "bottom",
#         strip.background = element_rect(fill = "white", color = "black", size = 1),
#         panel.border = element_rect(color = "black", size = 0.7),
#         panel.grid = element_blank(),
#         panel.background = element_rect(colour="black", size = 1.3))+
#   guides(col=guide_legend("Estimation method"))
# fig3
# # doesn't look great bc got lots of NAs at low MRSes for middle & top so they're not even distributions of travel speeds

# so just make one figure:
fig3_df <- data.frame(prop = c(rep(c(s_prop+z_prop), times=2)),
                      bias = c((hmean-MRS)/MRS, (lnorm-MRS)/MRS),
                      wedge = factor(c(rep(c(rep("Bottom", times=220), rep("Middle", times=220), rep("Top", times=220)), times=2)), levels = c("Top", "Middle", "Bottom")))
fig3_df <- na.omit(fig3_df)

fig3 <- ggplot(fig3_df, aes(x=prop, y=bias))+
  geom_point(aes(colour=wedge, shape=wedge))+
  geom_smooth(method="lm", se=F, colour="black")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(name = "Part of detection zone",
                      labels = c("Top", "Middle", "Bottom"),
                      values = c( "black", "#0000FF", "#FF0099")) +   
  scale_shape_manual(name = "Part of detection zone",
                      labels = c("Top", "Middle", "Bottom"),
                     values = c(3, 2, 1)) + 
  # scale_colour_manual(values = c("#FF0099", "#0000FF", "red"))+
  labs(x = "Proportion missed frames [number of missed frames/number of detected points]",
       y = "Bias [(estimated travel speed/true travel speed) - 1]")+
  # ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = c(0.78,0.87),
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(color = guide_legend(override.aes = list(size = 4)))
fig3

png(file=paste0("../results/final_results/plots/fig3.png"), width = 7, height = 5, units = 'in', res = 300)
print(fig3)
dev.off()


# testing significance of effect of prop missed frames on bias:
miss_lm <- lm(bias ~ prop, data=fig3_df)
miss_lm
summary(miss_lm)
# test significance of regression fit using ANOVA (decomposes total variation into variation due to relationship between x & y and variation due to random error)
# null hypothesis: slope = 0
miss_mod <- aov(miss_lm)
par(mfrow=c(2,2))
plot(miss_mod)
# fits the assumptions of ANOVA pretty well
anova(miss_lm)
# F1,1190 = 765.8, p < 0.05




# FIGURE 4: true vs missed vs obs distributions ---------------------------

# use just one simulation run - play around with diff ones and see which one looks best
load("../results/final_results/paths_uni/Mb1/iter1.RData") # gives you distr of true speeds
load("../results/final_results/uni_hz_scaling/seq_dats/Mb1iter1.RData") # gives you distr of obs speeds
load("../results/final_results/uni_hz_scaling/plotting_variables/wedge0/Mb_all_iters1-20.RData") # only gives mean of single & zero speeds
# # manually work out speeds of singles & zeros using gen_plot_var_eachiter function: gives you:
# sz_tosave <- list(singles_v=singles_v,
#                   zeros_v=zeros_v)
# save(sz_tosave, file = "../results/final_results/uni_hz_scaling/plotting_variables/wedge0/Mb1iter1_sz_spds.RData")
# just load back in if needed again:
load("../results/final_results/uni_hz_scaling/plotting_variables/wedge0/Mb1iter1_sz_spds.RData")

true_spds <- na.omit(path$speed)
obs_spds <- na.omit(seq_dats$v$speed)
missed_spds <- na.omit(c(sz_tosave$singles_v, sz_tosave$zeros_v))
encount_spds <- c(obs_spds, missed_spds)
mb1 <- outputs_mb[[1]]
hmean <- mb1[[1]]$hmean_m
lnorm <- mb1[[1]]$lnorm_m
hmean_sz <- mb1[[1]]$hmean_m_sz
lnorm_sz <- mb1[[1]]$lnorm_m_sz

fig4_df <- data.frame(speed = c(obs_spds, encount_spds, true_spds),
                      type = factor(c(rep("Sampled", times = length(obs_spds)), rep("Encountered", times = length(encount_spds)), rep("True", times=length(true_spds))), levels = c("True", "Encountered", "Sampled")))
fig4_df <- na.omit(fig4_df)

all <- ggplot(fig4_df, aes(x=log(speed)))+
  # geom_density(aes(colour = type))+
  stat_density(aes(colour=type),
               geom="line",position="identity", size=1.1)+
  theme_bw()+
  scale_colour_manual(name = "",
                      labels = c("True", "Encountered", "Sampled"),
                      values = c("black", "#0000FF", "#FF0099"))+
  labs(x = "Ln speed (m/s)",
       y = "Density")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = c(0.83,0.83),
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))
all

true <- ggplot(fig4_df[fig4_df$type=="True",], aes(x=log(speed)))+
  stat_density(aes(colour=type),
               geom="line",position="identity", size=1.1)+
  theme_bw()+
  scale_colour_manual(name = "",
                      labels = c("True"),
                      values = c("black"))+
  labs(x = "Ln speed (m/s)",
       y = "Density")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = c(0.83,0.86),
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))
true


true_enc <- ggplot(fig4_df[fig4_df$type%in%c("True", "Encountered"),], aes(x=log(speed)))+
  stat_density(aes(colour=type),
               geom="line",position="identity", size=1.1)+
  theme_bw()+
  scale_colour_manual(name = "",
                      labels = c("True", "Encountered"),
                      values = c("black", "#0000FF"))+
  labs(x = "Ln speed (m/s)",
       y = "Density")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = c(0.83,0.85),
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))
true_enc

png(file=paste0("../results/final_results/plots/dens_all.png"), width = 5, height = 3.5, units = 'in', res = 300)
print(all)
dev.off()

png(file=paste0("../results/final_results/plots/dens_true.png"), width = 5, height = 3.5, units = 'in', res = 300)
print(true)
dev.off()

png(file=paste0("../results/final_results/plots/dens_true_enc.png"), width = 5, height = 3.5, units = 'in', res = 300)
print(true_enc)
dev.off()



# new: make top & bottom plots:
# top plot: just sampled speeds and true speeds + means of each + hmean & lnorm estimates
# bottom plot: true, sampled & missed speeds + means of each + hmean & lnorm estimates
fig4_1_df <- data.frame(speed = c(obs_spds, true_spds),
                      type = factor(c(rep("Sampled", times = length(obs_spds)), rep("True", times=length(true_spds))), levels = c("True", "Sampled")))

fig4_1_vlines <- data.frame(xintercept = c(log(hmean), log(lnorm), log(mean(true_spds)), log(mean(obs_spds))),
                            lines = c("Harmonic mean", "Log-normal", "True mean", "Sampled arithmetic mean"))

fig4_1 <- ggplot(fig4_1_df, aes(x=log(speed)))+
  # geom_density(aes(colour = type))+
  stat_density(aes(colour=type),
               geom="line",position="identity", size=1.1)+
    geom_vline(aes(xintercept = xintercept, color = lines), fig4_1_vlines, size = 1) +
  theme_bw()+
  scale_colour_manual(name = "",
                      # labels = c("True", "Sampled", "Missed"),
                      values = c("black", "#0000FF", "#0000FF", "#FF0099", "red", "green"))+
  labs(x = "Ln speed (m/s)",
       y = "Density")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = c(0.84,0.6),
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))
fig4_1


# line.data <- data.frame(xintercept = c(2, 4), Lines = c("lower", "upper"),
#                         color = c("red", "blue"), stringsAsFactors = FALSE)
# 
# ggplot(BOD, aes( Time, demand ) ) + 
#   geom_point() + 
#   geom_vline(aes(xintercept = xintercept, color = Lines), line.data, size = 1) +
#   scale_colour_manual(values = line.data$color)




png(file=paste0("../results/final_results/plots/fig4.png"), width = 5, height = 2.5, units = 'in', res = 300)
print(fig4)
dev.off()

ggplot()+geom_density(aes(x=log(obs_spds)), colour="red")+geom_density(aes(x=log(missed_spds)))+geom_density(aes(x=log(true_spds)), colour="blue")







# true vs missed vs sampled:
true_missed_sampled_df <- data.frame(speed = c(obs_spds, missed_spds, true_spds),
                        type = factor(c(rep("Sampled", times = length(obs_spds)), rep("Missed", times = length(missed_spds)), rep("True", times=length(true_spds))), levels = c("True", "Sampled", "Missed")))
true_missed_sampled_df <- na.omit(true_missed_sampled_df)

true_missed_sampled <- ggplot(true_missed_sampled_df, aes(x=log(speed)))+
  # geom_density(aes(colour = type))+
  stat_density(aes(colour=type, linetype=type),
               geom="line",position="identity", size=1.1)+
  theme_bw()+
  scale_colour_manual(name = "",
                      labels = c("True", "Sampled", "Missed"),
                      values = c("black", "#FF0099", "#FF0099"))+
  scale_linetype_manual(name="",
                        labels=c("True", "Sampled", "Missed"),
                        values=c(1,1,4))+
  labs(x = "Ln speed (m/s)",
       y = "Density")+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = c(0.86,0.74),
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))
true_missed_sampled

png(file=paste0("../results/final_results/plots/true_missed_sampled.png"), width = 5, height = 2.7, units = 'in', res = 300)
print(true_missed_sampled)
dev.off()








# (prev)FIGURE 1: compare biases for diff estimation methods for uni-modal movement only -------------------------------------

# just need plotting variables for uni_hz_scaling: Mb_all_iters1-20.RData

load("../results/final_results/uni_hz_scaling/plotting_variables/wedge0/Mb_all_iters1-20.RData")

# list of 11 - one for each body mass
# for each body mass: list of 20 - one for each iteration
# for each iteration: list of 27 variables (each contains just one value): aMRS, amMOS etc

# need:
# true MRS across the whole path == aMRS
# estimated travel speeds:
    # arithmetic == amMOS
    # arithmetic + sz == amMOS_sz
    # hmean == hmean_m
    # hmean + sz == hmean_m_sz
    # lnorm == lnorm_m
    # lnorm + sz == lnorm_m_sz
    # gamma == gamma_m
    # gamma + sz == gamma_m_sz
    # weibull == weibull
    # weibull + sz == weibull_m_sz

# for the dataframe: need:
  # MRS (x) - 20 per body mass (=220)
  # bias (y) - 10 per MRS (each method + that method incl sz) (=2200)
  # type: raw vs sz (for 2 panels side by side)
  # method (for diff coloured lines)

# variables to extract:
MRS <- c() # order in which to fill: 20 MRSes for Mb1, 20MRSes for Mb5, etc for each body mass (length==220)
ar <- c() # 20 for Mb1, 20 for Mb5 etc for all body masses (length==220)
hmean <- c() # ditto
lnorm <- c()
gamma <- c()
weibull <- c()
ar_sz <- c()
hmean_sz <- c()
lnorm_sz <- c()
gamma_sz <- c()
weibull_sz <- c()

for (i in 1:11){ # loop through each body mass
  mb_list <- outputs_mb[[i]] # list of 20 iterations for a given body mass
  for (j in 1:20){ # loop through each iteration
    i_list <- mb_list[[j]] # list of 27 variables for a given iteration
    
    # for some iterations, the models couldn't be fitted: these lists don't contain any variables and just contain the error message
    # check if this is the case:
    test <- 0
    try(test <- i_list$aMRS, silent=TRUE) # try to extract something from the list and ignore any error messages and keep going
    if (test==0){ # if the iteration list contains just errors, test will still be zero because there was nothing to replace it with - in this case just assign everything as NA
      MRS <- c(MRS, NA)
      ar <- c(ar, NA)
      hmean <- c(hmean, NA)
      lnorm <- c(lnorm, NA)
      gamma <- c(gamma, NA)
      weibull <- c(weibull, NA)
      ar_sz <- c(ar_sz, NA)
      hmean_sz <- c(hmean_sz, NA)
      lnorm_sz <- c(lnorm_sz, NA)
      gamma_sz <- c(gamma_sz, NA)
      weibull_sz <- c(weibull_sz, NA)
    }
    else { # if the iteration list is ok, a value was assigned to test so it won't be zero anymore (aMRS can't physically be zero) - if this is the case then extract all the variables as normal
      MRS <- c(MRS, i_list[["aMRS"]])
      ar <- c(ar, i_list[["amMOS"]])
      hmean <- c(hmean, i_list[["hmean_m"]])
      lnorm <- c(lnorm, i_list[["lnorm_m"]])
      gamma <- c(gamma, i_list[["gamma_m"]])
      weibull <- c(weibull, i_list[["weibull_m"]])
      ar_sz <- c(ar_sz, i_list[["amMOS_sz"]])
      hmean_sz <- c(hmean_sz, i_list[["hmean_m_sz"]])
      lnorm_sz <- c(lnorm_sz, i_list[["lnorm_m_sz"]])
      gamma_sz <- c(gamma_sz, i_list[["gamma_m_sz"]])
      weibull_sz <- c(weibull_sz, i_list[["weibull_m_sz"]])
    }
  }
}

# combine into dataframe
fig1_df <- data.frame(bias = c((ar-MRS)/MRS, (hmean-MRS)/MRS, (lnorm-MRS)/MRS, (gamma-MRS)/MRS, (weibull-MRS)/MRS, (ar_sz-MRS)/MRS, (hmean_sz-MRS)/MRS, (lnorm_sz-MRS)/MRS, (gamma_sz-MRS)/MRS, (weibull_sz-MRS)/MRS),
                      MRS = c(rep(MRS, times=10)),
                      type = factor(c(rep("Missing bias not corrected", times=1100), rep("Missing bias corrected", times=1100)), levels = c("Missing bias not corrected", "Missing bias corrected")),
                      Method = factor(c(rep(c(rep("Arithmetic mean", times=220), rep("Harmonic mean", times=220), rep("Log-normal", times=220), rep("Gamma", times=220), rep("Weibull", times=220)), times=2)), levels = c("Arithmetic mean", "Harmonic mean", "Log-normal", "Gamma", "Weibull")))
fig1_df <- na.omit(fig1_df) # remove rows where bias and MRS are NA

# save df:
write.csv(fig1_df, file = "../results/final_results/plots/fig1_df.csv")

# make plot
fig1 <- ggplot(fig1_df, aes(x=MRS, y=bias))+
  geom_point(aes(colour=Method), size=0.6)+
  facet_grid(~ type)+
  geom_smooth(aes(colour=Method), alpha = 0.2)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "True travel speed (m/s)",
       y = "Bias [(estimated travel speed/true travel speed) - 1]")+
  ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
fig1
cvdPlot(fig1) # to simulate colour-blindness - use this + wheel("red", 8) from colortools package to trial different sets of contrasting colours

# save:
png(file=paste0("../results/final_results/plots/fig1.png"), width = 7, height = 4.2, units = 'in', res = 280)
print(fig1)
dev.off()

jpeg(filename=paste0("../results/final_results/plots/fig1.jpeg"), width = 7, height = 4.2, units = 'in', res = 280, quality=100)
print(fig1)
dev.off()


# (prev)FIGURE 2: comparing biases of hmean across movement behaviours - incorporates supp fig 1 -------------------

# extract the variables you need:
MRS <- c() # 20 reps for each 11 body masses in each of the 7 different types of movement behaviours (= 1540 total)
ar <- c() # ditto
hmean <- c()
lnorm <- c()
gamma <- c()
weibull <- c()
ar_sz <- c()
hmean_sz <- c()
lnorm_sz <- c()
gamma_sz <- c()
weibull_sz <- c()
mov_type <- c()

folderpaths <- c("uni_hz_scaling/plotting_variables/", "bi_hz_scaling/plotting_variables/mov0.1/", "bi_hz_scaling/plotting_variables/mov0.25/", "bi_hz_scaling/plotting_variables/mov0.4/", "bi_hz_scaling/plotting_variables/mov0.6/", "bi_hz_scaling/plotting_variables/mov0.75/", "bi_hz_scaling/plotting_variables/mov0.9/")

for (f in folderpaths){
  load(paste0("../results/final_results/", f, "wedge0/Mb_all_iters1-20.RData"))
  for (i in 1:11){ # loop through each body mass
    mb_list <- outputs_mb[[i]] # list of 20 iterations for a given body mass
    for (j in 1:20){ # loop through each iteration
      i_list <- mb_list[[j]] # list of 27 variables for a given iteration
      
      # for some iterations, the models couldn't be fitted: these lists don't contain any variables and just contain the error message
      # check if this is the case:
      test <- 0
      try(test <- i_list$aMRS, silent=TRUE) # try to extract something from the list and ignore any error messages and keep going
      if (test==0){ # if the iteration list contains just errors, test will still be zero because there was nothing to replace it with - in this case just assign everything as NA
        MRS <- c(MRS, NA)
        ar <- c(ar, NA)
        hmean <- c(hmean, NA)
        lnorm <- c(lnorm, NA)
        gamma <- c(gamma, NA)
        weibull <- c(weibull, NA)
        ar_sz <- c(ar_sz, NA)
        hmean_sz <- c(hmean_sz, NA)
        lnorm_sz <- c(lnorm_sz, NA)
        gamma_sz <- c(gamma_sz, NA)
        weibull_sz <- c(weibull_sz, NA)
        mov_type <- c(mov_type, NA)
      }
      else { # if the iteration list is ok, a value was assigned to test so it won't be zero anymore (aMRS can't physically be zero) - if this is the case then extract all the variables as normal
        MRS <- c(MRS, i_list[["aMRS"]])
        ar <- c(ar, i_list[["amMOS"]])
        hmean <- c(hmean, i_list[["hmean_m"]])
        lnorm <- c(lnorm, i_list[["lnorm_m"]])
        gamma <- c(gamma, i_list[["gamma_m"]])
        weibull <- c(weibull, i_list[["weibull_m"]])
        ar_sz <- c(ar_sz, i_list[["amMOS_sz"]])
        hmean_sz <- c(hmean_sz, i_list[["hmean_m_sz"]])
        lnorm_sz <- c(lnorm_sz, i_list[["lnorm_m_sz"]])
        gamma_sz <- c(gamma_sz, i_list[["gamma_m_sz"]])
        weibull_sz <- c(weibull_sz, i_list[["weibull_m_sz"]])
        
        if (f == "uni_hz_scaling/plotting_variables/"){
          mov_type <- c(mov_type, "Unimodal")
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.1/"){
          mov_type <- c(mov_type, "Bimodal: 10% moving")
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.25/"){
          mov_type <- c(mov_type, "Bimodal: 25% moving")
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.4/"){
          mov_type <- c(mov_type, "Bimodal: 40% moving")
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.6/"){
          mov_type <- c(mov_type, "Bimodal: 60% moving")
        }
        if (f == "bi_hz_scaling/plotting_variables/mov0.75/"){
          mov_type <- c(mov_type, "Bimodal: 75% moving")
        }
         if (f == "bi_hz_scaling/plotting_variables/mov0.9/"){
           mov_type <- c(mov_type, "Bimodal: 90% moving")
         }
      }
    }
  }
}

# work out one mean bias per estimation method for each of the 7 movement behaviours:
ar_meanbias <- c()
h_meanbias <- c()
l_meanbias <- c()
g_meanbias <- c()
w_meanbias <- c()
ar_sz_meanbias <- c()
h_sz_meanbias <- c()
l_sz_meanbias <- c()
g_sz_meanbias <- c()
w_sz_meanbias <- c()

for (i in 1:7){ # for each movement behaviour, select the relevant chunk, work out all the biases, and average them out
  selection <- (220*(i-1)+1):(220*i)
  mrs <- na.omit(MRS[selection])
  a <- na.omit(ar[selection])
  h <- na.omit(hmean[selection])
  l <- na.omit(lnorm[selection])
  g <- na.omit(gamma[selection])
  w <- na.omit(weibull[selection])
  a_sz <- na.omit(ar_sz[selection])
  h_sz <- na.omit(hmean_sz[selection])
  l_sz <- na.omit(lnorm_sz[selection])
  g_sz <- na.omit(gamma_sz[selection])
  w_sz <- na.omit(weibull_sz[selection])

  ar_meanbias <- c(ar_meanbias, mean((a-mrs)/mrs))
  h_meanbias <- c(h_meanbias, mean((h-mrs)/mrs))
  l_meanbias <- c(l_meanbias, mean((l-mrs)/mrs))
  g_meanbias <- c(g_meanbias, mean((g-mrs)/mrs))
  w_meanbias <- c(w_meanbias, mean((w-mrs)/mrs))
  ar_sz_meanbias <- c(ar_sz_meanbias, mean((a_sz-mrs)/mrs))
  h_sz_meanbias <- c(h_sz_meanbias, mean((h_sz-mrs)/mrs))
  l_sz_meanbias <- c(l_sz_meanbias, mean((l_sz-mrs)/mrs))
  g_sz_meanbias <- c(g_sz_meanbias, mean((g_sz-mrs)/mrs))
  w_sz_meanbias <- c(w_sz_meanbias, mean((w_sz-mrs)/mrs))
  
}


fig2_df <- data.frame(bias = c(ar_meanbias, h_meanbias, l_meanbias, g_meanbias, w_meanbias, 
                               ar_sz_meanbias, h_sz_meanbias, l_sz_meanbias, g_sz_meanbias, w_sz_meanbias),
                      mov_behav = rep(c(0,0.1,0.25,0.4,0.6,0.75,0.9), times=10),
                      type = factor(c(rep("Missing bias not corrected", times=35), rep("Missing bias corrected", times=35)), levels = c("Missing bias not corrected", "Missing bias corrected")),
                      Method = factor(c(rep("Arithmetic mean", times=7), rep("Harmonic mean", times=7), rep("Log-normal", times=7), rep("Gamma", times=7), rep("Weibull", times=7), rep("Arithmetic mean", times=7), rep("Harmonic mean", times=7), rep("Log-normal", times=7), rep("Gamma", times=7), rep("Weibull", times=7)), levels = c("Arithmetic mean", "Harmonic mean", "Log-normal", "Gamma", "Weibull")))

# make plot
fig2 <- ggplot(fig2_df, aes(x=mov_behav, y=bias))+
  geom_point(aes(colour=Method), size=2)+
  facet_grid(~ type)+
  geom_smooth(aes(colour=Method), alpha = 0.2)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "Proportion of fast behaviour (0 = unimodal)",
       y = "Mean bias [(estimated travel speed/true travel speed) - 1]")+
  # ylim(c(-0.5, 1))+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size = 11),
        legend.title = element_text(size=11),
        legend.text = element_text(size = 11),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        # legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))+
  guides(col=guide_legend("Estimation method"))
fig2
cvdPlot(fig1) # to simulate colour-blindness - use this + wheel("red", 8) from colortools package to trial different sets of contrasting colours

# save:
png(file=paste0("../results/final_results/plots/fig2.png"), width = 7, height = 4.2, units = 'in', res = 280)
print(fig2)
dev.off()



# prev version: just showing hmean and ar biases from diff movement behaviours (colour of lines) across diff MRSes (x axis) for panels with and without singles & zeros 
# # combine into dataframe
# fig2_df <- data.frame(bias = c((hmean-MRS)/MRS, (ar-MRS)/ar, (hmean_sz-MRS)/MRS, (ar_sz-MRS)/MRS),
#                       MRS = c(rep(MRS, times=4)),
#                       type = factor(c(rep("Missed speeds absent", times=3080), rep("Missed speeds included", times=3080)), levels = c("Missed speeds absent", "Missed speeds included")),
#                       Method = factor(c(rep("Harmonic mean", times=1540), rep("Arithmetic mean", times=1540), rep("Harmonic mean", times=1540), rep("Arithmetic mean", times=1540)), levels = c("Harmonic mean", "Arithmetic mean")),
#                       Behaviour = factor(c(rep(mov_type, times=4)), levels = c("Unimodal", "Bimodal: 10% moving", "Bimodal: 25% moving", "Bimodal: 40% moving", "Bimodal: 60% moving", "Bimodal: 75% moving")))
# fig2_df <- na.omit(fig2_df) # remove rows where bias and MRS are NA
# 
# # save df:
# write.csv(fig2_df, file = "../results/final_results/plots/fig2_df.csv")
# 
# # make plot
# fig2 <- ggplot(fig2_df, aes(x=MRS, y=bias))+
#   # geom_point(aes(colour=Method))+
#   facet_grid(~ type)+
#   geom_smooth(aes(colour=Behaviour, linetype=Method), alpha = 0.2)+
#   theme_bw()+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000", "green"))+
#   # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
#   labs(x = "True travel speed (m/s)",
#        y = "Bias [(estimated travel speed/true travel speed) - 1]")+
#   ylim(c(-0.5, 1.3))+
#   theme(axis.title = element_text(size=17),
#         axis.text = element_text(size = 15),
#         legend.title = element_text(size = 16, face = "bold"),
#         legend.text = element_text(size = 16),
#         title = element_text(size = 13),
#         strip.text = element_text(size = 15),
#         # legend.position = "bottom",
#         strip.background = element_rect(fill = "white", color = "black", size = 1),
#         panel.border = element_rect(color = "black", size = 0.7),
#         panel.grid = element_blank(),
#         panel.background = element_rect(colour="black", size = 1.3))
# fig2
# cvdPlot(fig2) # to simulate colour-blindness - use this + wheel("red", 8) from colortools package to trial different sets of contrasting colours
# 
# # save:
# png(file=paste0("../results/final_results/plots/fig1.png"),
#     width=900, height=500)
# print(fig1)
# dev.off()





# (prev)SUPP FIG 1: check that hmean is indeed consistently pretty good for different movement behaviours -------------------

# repeat what you did for figure 1 for the different movement behaviours: mov0.1, mov0.25, mov0.4, mov0.6, mov0.75, mov0.9

load("../results/final_results/uni_hz_scaling/plotting_variables/wedge2/Mb_all_iters1-20.RData")

# variables to extract:
MRS <- c() # order in which to fill: 20 MRSes for Mb1, 20MRSes for Mb5, etc for each body mass (length==220)
ar <- c() # 20 for Mb1, 20 for Mb5 etc for all body masses (length==220)
hmean <- c() # ditto
lnorm <- c()
gamma <- c()
weibull <- c()
ar_sz <- c()
hmean_sz <- c()
lnorm_sz <- c()
gamma_sz <- c()
weibull_sz <- c()

for (i in 1:11){ # loop through each body mass
  mb_list <- outputs_mb[[i]] # list of 20 iterations for a given body mass
  for (j in 1:20){ # loop through each iteration
    i_list <- mb_list[[j]] # list of 27 variables for a given iteration
    
    # for some iterations, the models couldn't be fitted: these lists don't contain any variables and just contain the error message
    # check if this is the case:
    test <- 0
    try(test <- i_list$aMRS, silent=TRUE) # try to extract something from the list and ignore any error messages and keep going
    if (test==0){ # if the iteration list contains just errors, test will still be zero because there was nothing to replace it with - in this case just assign everything as NA
      MRS <- c(MRS, NA)
      ar <- c(ar, NA)
      hmean <- c(hmean, NA)
      lnorm <- c(lnorm, NA)
      gamma <- c(gamma, NA)
      weibull <- c(weibull, NA)
      ar_sz <- c(ar_sz, NA)
      hmean_sz <- c(hmean_sz, NA)
      lnorm_sz <- c(lnorm_sz, NA)
      gamma_sz <- c(gamma_sz, NA)
      weibull_sz <- c(weibull_sz, NA)
    }
    else { # if the iteration list is ok, a value was assigned to test so it won't be zero anymore (aMRS can't physically be zero) - if this is the case then extract all the variables as normal
      MRS <- c(MRS, i_list[["aMRS"]])
      ar <- c(ar, i_list[["amMOS"]])
      hmean <- c(hmean, i_list[["hmean_m"]])
      lnorm <- c(lnorm, i_list[["lnorm_m"]])
      gamma <- c(gamma, i_list[["gamma_m"]])
      weibull <- c(weibull, i_list[["weibull_m"]])
      ar_sz <- c(ar_sz, i_list[["amMOS_sz"]])
      hmean_sz <- c(hmean_sz, i_list[["hmean_m_sz"]])
      lnorm_sz <- c(lnorm_sz, i_list[["lnorm_m_sz"]])
      gamma_sz <- c(gamma_sz, i_list[["gamma_m_sz"]])
      weibull_sz <- c(weibull_sz, i_list[["weibull_m_sz"]])
    }
  }
}

# combine into dataframe
fig1_df <- data.frame(bias = c((ar-MRS)/MRS, (hmean-MRS)/MRS, (lnorm-MRS)/MRS, (gamma-MRS)/MRS, (weibull-MRS)/MRS, (ar_sz-MRS)/MRS, (hmean_sz-MRS)/MRS, (lnorm_sz-MRS)/MRS, (gamma_sz-MRS)/MRS, (weibull_sz-MRS)/MRS),
                      MRS = c(rep(MRS, times=10)),
                      type = factor(c(rep("Missed speeds absent", times=1100), rep("Missed speeds included", times=1100)), levels = c("Missed speeds absent", "Missed speeds included")),
                      Method = factor(c(rep(c(rep("Arithmetic mean", times=220), rep("Harmonic mean", times=220), rep("Log-normal", times=220), rep("Gamma", times=220), rep("Weibull", times=220)), times=2)), levels = c("Arithmetic mean", "Harmonic mean", "Log-normal", "Gamma", "Weibull")))
fig1_df <- na.omit(fig1_df) # remove rows where bias and MRS are NA

# make plot
fig1 <- ggplot(fig1_df, aes(x=MRS, y=bias))+
  # geom_point(aes(colour=Method))+
  facet_grid(~ type)+
  geom_smooth(aes(colour=Method), alpha = 0.2)+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values = c("black", "#FF0099", "#0000FF", "#FFBF00", "#FF0000"))+
  # scale_colour_manual(values = c("#FF0000", "#FF00FF", "#56B4E9", "#00FF00", "#0000FF"))+
  labs(x = "True travel speed (m/s)",
       y = "Bias [(estimated travel speed/true travel speed) - 1]")+
  theme(axis.title = element_text(size=17),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        title = element_text(size = 13),
        strip.text = element_text(size = 15),
        # legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 0.7),
        panel.grid = element_blank(),
        panel.background = element_rect(colour="black", size = 1.3))
fig1

# save this one too:
png(file=paste0("../results/final_results/plots/uni_wedge2_scal.png"),
    width=900, height=500)
print(fig1)
dev.off()

# make some kind of combined plot with all of these








# (prev)extract variables to plot biases to compare across uni/bimodal - using the whole wedge + full distance det prob scaling  ---------



parentfolder <- paste0("../results/final_results/")

# set which speed parameters to analyse
Mb_range <- c(1,5,10,15,20,25,30,35,40,45,50)



  aMRS <- c()
  Mb <- c()
  aMOS_MRS_err <- c() # aMOS/aMRS using mean of means
  aMOS_MRS_err_sz <- c() # aMOS/aMRS using mean of means + incl sz
  hmean_MRS_err <- c() # hmean/aMRS using mean of means
  hmean_MRS_err_sz <- c() # hmean/aMRS using mean of means + incl sz
  lnorm_MRS_err <- c() # same for lnorm
  lnorm_MRS_err_sz <- c() # same for lnorm
  gamma_MRS_err <- c() 
  gamma_MRS_err_sz <- c() 
  weibull_MRS_err <- c()
  weibull_MRS_err_sz <- c()
  uni_bi <- c() # whether it's unimodal or bimodal (and if so which one - mov0.1 etc)
  singles_percent <- c() # percentage of detected points which are singles
  
  # fill these variables
  # should have 4 reps (some of them didn't fit the models for all 5) for each 11 body masses for each of uni, bi0.9, and bi0.9 --i.e. 132 of each
  uni_bi <- c(rep("uni", times =44), rep("bi0.9", times=44), rep("bi0.9", times=44))
  Mb <- c(rep(c(1,5,10,15,20,25,30,35,40,45,50), times=3))
  
  
  # for unimodal:
  aMRS_uni <- c()
  aMOS_MRS_err_uni <- c()
  aMOS_MRS_err_uni_sz <- c()
  hmean_MRS_err_uni <- c()
  hmean_MRS_err_uni_sz <- c()
  lnorm_MRS_err_uni <- c()
  lnorm_MRS_err_uni_sz <- c()
  gamma_MRS_err_uni <- c()
  gamma_MRS_err_uni_sz <- c()
  weibull_MRS_err_uni <- c()
  weibull_MRS_err_uni_sz <- c()
  singles_percent_uni <- c()
  for (i in Mb_range){
    load(paste0(parentfolder, "uni_hz_scaling/plotting_variables/wedge0/Mb", i, "_iters1-5.RData"))
    
    mos_errors <- output$amMOS - output$aMRS
    mos_errors_sz <- output$amMOS_sz - output$aMRS
    
    hmean_errors <- output$hmean_m - output$aMRS
    hmean_errors_sz <- output$hmean_m_sz - output$aMRS
    
    lnorm_errors <- output$lnorm_m - output$aMRS
    lnorm_errors_sz <- output$lnorm_m_sz - output$aMRS
    
    gamma_errors <- output$gamma_m - output$aMRS
    gamma_errors_sz <- output$gamma_m_sz - output$aMRS
    
    weibull_errors <- output$weibull_m - output$aMRS
    weibull_errors_sz <- output$weibull_m_sz - output$aMRS
    
    s_perc <- (output$n_singles/output$n_detected)*100
    
    if (length(output$aMRS)==5){ # if there were 5 fitted models, just use the first 4
      
      aMRS_uni <- c(aMRS_uni, output$aMRS[-5])
      
      aMOS_MRS_err_uni <- c(aMOS_MRS_err_uni, mos_errors[-5])
      aMOS_MRS_err_uni_sz <- c(aMOS_MRS_err_uni_sz, mos_errors_sz[-5])
      
      hmean_MRS_err_uni <- c(hmean_MRS_err_uni, hmean_errors[-5])
      hmean_MRS_err_uni_sz <- c(hmean_MRS_err_uni_sz, hmean_errors_sz[-5])
      
      lnorm_MRS_err_uni <- c(lnorm_MRS_err_uni, lnorm_errors[-5])
      lnorm_MRS_err_uni_sz <- c(lnorm_MRS_err_uni_sz, lnorm_errors_sz[-5])
      
      gamma_MRS_err_uni <- c(gamma_MRS_err_uni, gamma_errors[-5])
      gamma_MRS_err_uni_sz <- c(gamma_MRS_err_uni_sz, gamma_errors[-5])
      
      weibull_MRS_err_uni <- c(weibull_MRS_err_uni, weibull_errors[-5])
      weibull_MRS_err_uni_sz <- c(weibull_MRS_err_uni_sz, weibull_errors_sz[-5])
      
      singles_percent_uni <- c(singles_percent_uni, s_perc[-5])
    }
    else {
      aMRS_uni <- c(aMRS_uni, output$aMRS)
      
      aMOS_MRS_err_uni <- c(aMOS_MRS_err_uni, mos_errors)
      aMOS_MRS_err_uni_sz <- c(aMOS_MRS_err_uni_sz, mos_errors_sz)
      
      hmean_MRS_err_uni <- c(hmean_MRS_err_uni, hmean_errors)
      hmean_MRS_err_uni_sz <- c(hmean_MRS_err_uni_sz, hmean_errors_sz)
      
      lnorm_MRS_err_uni <- c(lnorm_MRS_err_uni, lnorm_errors)
      lnorm_MRS_err_uni_sz <- c(lnorm_MRS_err_uni_sz, lnorm_errors_sz)
      
      gamma_MRS_err_uni <- c(gamma_MRS_err_uni, gamma_errors)
      gamma_MRS_err_uni_sz <- c(gamma_MRS_err_uni_sz, gamma_errors)
      
      weibull_MRS_err_uni <- c(weibull_MRS_err_uni, weibull_errors)
      weibull_MRS_err_uni_sz <- c(weibull_MRS_err_uni_sz, weibull_errors_sz)
      
      singles_percent_uni <- c(singles_percent_uni, s_perc)
    }
    rm(list = c("output"))
  }
  
  # for bimodal0.9:
  aMRS_bi0.9 <- c()
  aMOS_MRS_err_bi0.9 <- c()
  aMOS_MRS_err_bi0.9_sz <- c()
  hmean_MRS_err_bi0.9 <- c()
  hmean_MRS_err_bi0.9_sz <- c()
  lnorm_MRS_err_bi0.9 <- c()
  lnorm_MRS_err_bi0.9_sz <- c()
  gamma_MRS_err_bi0.9 <- c()
  gamma_MRS_err_bi0.9_sz <- c()
  weibull_MRS_err_bi0.9 <- c()
  weibull_MRS_err_bi0.9_sz <- c()
  singles_percent_bi0.9 <- c()
  for (i in Mb_range){
    load(paste0(parentfolder, "bi_hz_scaling/plotting_variables/mov0.9/wedge0/Mb", i, "_iters1-5.RData"))
    
    mos_errors <- output$amMOS - output$aMRS
    mos_errors_sz <- output$amMOS_sz - output$aMRS
    
    hmean_errors <- output$hmean_m - output$aMRS
    hmean_errors_sz <- output$hmean_m_sz - output$aMRS
    
    lnorm_errors <- output$lnorm_m - output$aMRS
    lnorm_errors_sz <- output$lnorm_m_sz - output$aMRS
    
    gamma_errors <- output$gamma_m - output$aMRS
    gamma_errors_sz <- output$gamma_m_sz - output$aMRS
    
    weibull_errors <- output$weibull_m - output$aMRS
    weibull_errors_sz <- output$weibull_m_sz - output$aMRS
    
    s_perc <- (output$n_singles/output$n_detected)*100
    
    if (length(output$aMRS)==5){ # if there were 5 fitted models, just use the first 4
      
      aMRS_bi0.9 <- c(aMRS_bi0.9, output$aMRS[-5])
      
      aMOS_MRS_err_bi0.9 <- c(aMOS_MRS_err_bi0.9, mos_errors[-5])
      aMOS_MRS_err_bi0.9_sz <- c(aMOS_MRS_err_bi0.9_sz, mos_errors_sz[-5])
      
      hmean_MRS_err_bi0.9 <- c(hmean_MRS_err_bi0.9, hmean_errors[-5])
      hmean_MRS_err_bi0.9_sz <- c(hmean_MRS_err_bi0.9_sz, hmean_errors_sz[-5])
      
      lnorm_MRS_err_bi0.9 <- c(lnorm_MRS_err_bi0.9, lnorm_errors[-5])
      lnorm_MRS_err_bi0.9_sz <- c(lnorm_MRS_err_bi0.9_sz, lnorm_errors_sz[-5])
      
      gamma_MRS_err_bi0.9 <- c(gamma_MRS_err_bi0.9, gamma_errors[-5])
      gamma_MRS_err_bi0.9_sz <- c(gamma_MRS_err_bi0.9_sz, gamma_errors[-5])
      
      weibull_MRS_err_bi0.9 <- c(weibull_MRS_err_bi0.9, weibull_errors[-5])
      weibull_MRS_err_bi0.9_sz <- c(weibull_MRS_err_bi0.9_sz, weibull_errors_sz[-5])
      
      singles_percent_bi0.9 <- c(singles_percent_bi0.9, s_perc[-5])
    }
    else {
      aMRS_bi0.9 <- c(aMRS_bi0.9, output$aMRS)
      
      aMOS_MRS_err_bi0.9 <- c(aMOS_MRS_err_bi0.9, mos_errors)
      aMOS_MRS_err_bi0.9_sz <- c(aMOS_MRS_err_bi0.9_sz, mos_errors_sz)
      
      hmean_MRS_err_bi0.9 <- c(hmean_MRS_err_bi0.9, hmean_errors)
      hmean_MRS_err_bi0.9_sz <- c(hmean_MRS_err_bi0.9_sz, hmean_errors_sz)
      
      lnorm_MRS_err_bi0.9 <- c(lnorm_MRS_err_bi0.9, lnorm_errors)
      lnorm_MRS_err_bi0.9_sz <- c(lnorm_MRS_err_bi0.9_sz, lnorm_errors_sz)
      
      gamma_MRS_err_bi0.9 <- c(gamma_MRS_err_bi0.9, gamma_errors)
      gamma_MRS_err_bi0.9_sz <- c(gamma_MRS_err_bi0.9_sz, gamma_errors)
      
      weibull_MRS_err_bi0.9 <- c(weibull_MRS_err_bi0.9, weibull_errors)
      weibull_MRS_err_bi0.9_sz <- c(weibull_MRS_err_bi0.9_sz, weibull_errors_sz)
      
      singles_percent_bi0.9 <- c(singles_percent_bi0.9, s_perc)
    }
    rm(list = c("output"))
  }
  
  # for bimodal0.9
  aMRS_bi0.9 <- c()
  aMOS_MRS_err_bi0.9 <- c()
  aMOS_MRS_err_bi0.9_sz <- c()
  hmean_MRS_err_bi0.9 <- c()
  hmean_MRS_err_bi0.9_sz <- c()
  lnorm_MRS_err_bi0.9 <- c()
  lnorm_MRS_err_bi0.9_sz <- c()
  gamma_MRS_err_bi0.9 <- c()
  gamma_MRS_err_bi0.9_sz <- c()
  weibull_MRS_err_bi0.9 <- c()
  weibull_MRS_err_bi0.9_sz <- c()
  singles_percent_bi0.9 <- c()
  for (i in Mb_range){
    load(paste0(parentfolder, "bi_hz_scaling/plotting_variables/mov0.9/wedge0/Mb", i, "_iters1-5.RData"))
    
    mos_errors <- output$amMOS - output$aMRS
    mos_errors_sz <- output$amMOS_sz - output$aMRS
    
    hmean_errors <- output$hmean_m - output$aMRS
    hmean_errors_sz <- output$hmean_m_sz - output$aMRS
    
    lnorm_errors <- output$lnorm_m - output$aMRS
    lnorm_errors_sz <- output$lnorm_m_sz - output$aMRS
    
    gamma_errors <- output$gamma_m - output$aMRS
    gamma_errors_sz <- output$gamma_m_sz - output$aMRS
    
    weibull_errors <- output$weibull_m - output$aMRS
    weibull_errors_sz <- output$weibull_m_sz - output$aMRS
    
    s_perc <- (output$n_singles/output$n_detected)*100
    
    if (length(output$aMRS)==5){ # if there were 5 fitted models, just use the first 4
      
      aMRS_bi0.9 <- c(aMRS_bi0.9, output$aMRS[-5])
      
      aMOS_MRS_err_bi0.9 <- c(aMOS_MRS_err_bi0.9, mos_errors[-5])
      aMOS_MRS_err_bi0.9_sz <- c(aMOS_MRS_err_bi0.9_sz, mos_errors_sz[-5])
      
      hmean_MRS_err_bi0.9 <- c(hmean_MRS_err_bi0.9, hmean_errors[-5])
      hmean_MRS_err_bi0.9_sz <- c(hmean_MRS_err_bi0.9_sz, hmean_errors_sz[-5])
      
      lnorm_MRS_err_bi0.9 <- c(lnorm_MRS_err_bi0.9, lnorm_errors[-5])
      lnorm_MRS_err_bi0.9_sz <- c(lnorm_MRS_err_bi0.9_sz, lnorm_errors_sz[-5])
      
      gamma_MRS_err_bi0.9 <- c(gamma_MRS_err_bi0.9, gamma_errors[-5])
      gamma_MRS_err_bi0.9_sz <- c(gamma_MRS_err_bi0.9_sz, gamma_errors[-5])
      
      weibull_MRS_err_bi0.9 <- c(weibull_MRS_err_bi0.9, weibull_errors[-5])
      weibull_MRS_err_bi0.9_sz <- c(weibull_MRS_err_bi0.9_sz, weibull_errors_sz[-5])
      
      singles_percent_bi0.9 <- c(singles_percent_bi0.9, s_perc[-5])
    }
    else {
      aMRS_bi0.9 <- c(aMRS_bi0.9, output$aMRS)
      
      aMOS_MRS_err_bi0.9 <- c(aMOS_MRS_err_bi0.9, mos_errors)
      aMOS_MRS_err_bi0.9_sz <- c(aMOS_MRS_err_bi0.9_sz, mos_errors_sz)
      
      hmean_MRS_err_bi0.9 <- c(hmean_MRS_err_bi0.9, hmean_errors)
      hmean_MRS_err_bi0.9_sz <- c(hmean_MRS_err_bi0.9_sz, hmean_errors_sz)
      
      lnorm_MRS_err_bi0.9 <- c(lnorm_MRS_err_bi0.9, lnorm_errors)
      lnorm_MRS_err_bi0.9_sz <- c(lnorm_MRS_err_bi0.9_sz, lnorm_errors_sz)
      
      gamma_MRS_err_bi0.9 <- c(gamma_MRS_err_bi0.9, gamma_errors)
      gamma_MRS_err_bi0.9_sz <- c(gamma_MRS_err_bi0.9_sz, gamma_errors)
      
      weibull_MRS_err_bi0.9 <- c(weibull_MRS_err_bi0.9, weibull_errors)
      weibull_MRS_err_bi0.9_sz <- c(weibull_MRS_err_bi0.9_sz, weibull_errors_sz)
      
      singles_percent_bi0.9 <- c(singles_percent_bi0.9, s_perc)
    }
    rm(list = c("output"))
  }
  
  
  # combine everything together:
  aMRS <- c(aMRS_uni, aMRS_bi0.9, aMRS_bi0.9)
  aMOS_MRS_err <- c(aMOS_MRS_err_uni, aMOS_MRS_err_bi0.9, aMOS_MRS_err_bi0.9) 
  aMOS_MRS_err_sz <- c(aMOS_MRS_err_uni_sz, aMOS_MRS_err_bi0.9_sz, aMOS_MRS_err_bi0.9_sz) 
  hmean_MRS_err <- c(hmean_MRS_err_uni, hmean_MRS_err_bi0.9, hmean_MRS_err_bi0.9) 
  hmean_MRS_err_sz <- c(hmean_MRS_err_uni_sz, hmean_MRS_err_bi0.9_sz, hmean_MRS_err_bi0.9_sz) 
  lnorm_MRS_err <- c(lnorm_MRS_err_uni, lnorm_MRS_err_bi0.9, lnorm_MRS_err_bi0.9) 
  lnorm_MRS_err_sz <- c(lnorm_MRS_err_uni_sz, lnorm_MRS_err_bi0.9_sz, lnorm_MRS_err_bi0.9_sz) 
  gamma_MRS_err <- c(gamma_MRS_err_uni, gamma_MRS_err_bi0.9, gamma_MRS_err_bi0.9) 
  gamma_MRS_err_sz <- c(gamma_MRS_err_uni_sz, gamma_MRS_err_bi0.9_sz, gamma_MRS_err_bi0.9_sz) 
  weibull_MRS_err <- c(weibull_MRS_err_uni, weibull_MRS_err_bi0.9, weibull_MRS_err_bi0.9)
  weibull_MRS_err_sz <- c(weibull_MRS_err_uni_sz, weibull_MRS_err_bi0.9_sz, weibull_MRS_err_bi0.9_sz)
  singles_percent <- c(singles_percent_uni, singles_percent_bi0.9, singles_percent_bi0.9) 

  


# unimodal plots ----------------------------------------------------------


## plot for uni modal with hz_scaling detection distance - raw biases
uni_raw_df <- data.frame(aMRS=rep(aMRS_uni, times=5),
                           error=c(aMOS_MRS_err_uni/aMRS_uni, hmean_MRS_err_uni/aMRS_uni, lnorm_MRS_err_uni/aMRS_uni, gamma_MRS_err_uni/aMRS_uni, weibull_MRS_err_uni/aMRS_uni),
                           method= c(rep("arithmetic", times=44), rep("hmean", times=44), rep("lnorm", times=44), rep("gamma", times=44), rep("weibull",times=44)))
  
uni_raw_plot <- ggplot(uni_raw_df, aes(x = aMRS, y = error, colour = method))+
    geom_point()+
    # facet_grid(~ uni_bi, scales="free")+
    geom_smooth(alpha = 0.2, se=F)+
    theme_minimal()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
    labs(x = "Mean realised speed (m/s)",
         y = "Proportional error",
         title = "UNIMODAL SPEEDS\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          title = element_text(size = 13),
          strip.text = element_text(size = 13),
          legend.position = "bottom")
uni_raw_plot

# with singles added back in:
uni_singles_df <- data.frame(aMRS=rep(aMRS_uni, times=5),
                         error=c(aMOS_MRS_err_uni_sz/aMRS_uni, hmean_MRS_err_uni_sz/aMRS_uni, lnorm_MRS_err_uni_sz/aMRS_uni, gamma_MRS_err_uni_sz/aMRS_uni, weibull_MRS_err_uni_sz/aMRS_uni),
                         method= c(rep("arithmetic", times=44), rep("hmean", times=44), rep("lnorm", times=44), rep("gamma", times=44), rep("weibull",times=44)))

uni_singles_plot <- ggplot(uni_singles_df, aes(x = aMRS, y = error, colour = method))+
  geom_point()+
  # facet_grid(~ uni_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F)+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Proportional error",
       title = "UNIMODAL SPEEDS\nwith singles added back in\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")
uni_singles_plot

# percentage singles (to explain the change when add singles back in):
uni_s_percent_df <- data.frame(aMRS=aMRS_uni,
                             singles_percent=singles_percent_uni)

uni_s_percent_plot <- ggplot(uni_s_percent_df, aes(x = aMRS, y = singles_percent))+
  geom_point()+
  # facet_grid(~ uni_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F)+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Percentage (%)",
       title = "UNIMODAL SPEEDS\nPercentage of detected points which are single frames")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")
uni_s_percent_plot




# comparing raw vs singles for just hmean & lnorm so that can put them on the same plot:
uni_raw_singles_lnorm_hmean_df <- data.frame(aMRS=c(aMRS_uni, aMRS_uni, aMRS_uni, aMRS_uni),
                                         error=c(hmean_MRS_err_uni/aMRS_uni, lnorm_MRS_err_uni/aMRS_uni, hmean_MRS_err_uni_sz/aMRS_uni, lnorm_MRS_err_uni_sz/aMRS_uni),
                                         method=c(rep("hmean", times=44), rep("lnorm", times=44), rep("hmean", times=44), rep("lnorm", times=44)),
                                         scaling=c(rep("raw", times=88), rep("with singles", times=88)))

uni_raw_singles_lnorm_hmean_plot <- ggplot(uni_raw_singles_lnorm_hmean_df, aes(x = aMRS, y = error, colour=method))+
  geom_point()+
  # facet_grid(~ uni_noscal_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F, aes(linetype=scaling))+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Proportional error",
       title = "UNIMODAL SPEEDS\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")



# bimodal plots - 25% moving ----------------------------------------------------
  
# bimodal with 25% of time spent moving - raw
bi0.25_raw_df <- data.frame(aMRS=rep(aMRS_bi0.25, times=5),
                         error=c(aMOS_MRS_err_bi0.25/aMRS_bi0.25, hmean_MRS_err_bi0.25/aMRS_bi0.25, lnorm_MRS_err_bi0.25/aMRS_bi0.25, gamma_MRS_err_bi0.25/aMRS_bi0.25, weibull_MRS_err_bi0.25/aMRS_bi0.25),
                         method= c(rep("arithmetic", times=44), rep("hmean", times=44), rep("lnorm", times=44), rep("gamma", times=44), rep("weibull",times=44)))

bi0.25_raw_plot <- ggplot(bi0.25_raw_df, aes(x = aMRS, y = error, colour = method))+
  geom_point()+
  # facet_grid(~ bi0.25_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F)+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Proportional error",
       title = "BIMODAL SPEEDS - 25% fast + low tortuosity\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "botom")
bi0.25_raw_plot

# with singles added back in:
bi0.25_singles_df <- data.frame(aMRS=rep(aMRS_bi0.25, times=5),
                             error=c(aMOS_MRS_err_bi0.25_sz/aMRS_bi0.25, hmean_MRS_err_bi0.25_sz/aMRS_bi0.25, lnorm_MRS_err_bi0.25_sz/aMRS_bi0.25, gamma_MRS_err_bi0.25_sz/aMRS_bi0.25, weibull_MRS_err_bi0.25_sz/aMRS_bi0.25),
                             method= c(rep("arithmetic", times=44), rep("hmean", times=44), rep("lnorm", times=44), rep("gamma", times=44), rep("weibull",times=44)))

bi0.25_singles_plot <- ggplot(bi0.25_singles_df, aes(x = aMRS, y = error, colour = method))+
  geom_point()+
  # facet_grid(~ bi0.25_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F)+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Proportional error",
       title = "bi0.25MODAL SPEEDS\nwith singles added back in\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")
bi0.25_singles_plot


# percentage singles
bi0.25_s_percent_df <- data.frame(aMRS=aMRS_bi0.25,
                               singles_percent=singles_percent_bi0.25)

bi0.25_s_percent_plot <- ggplot(bi0.25_s_percent_df, aes(x = aMRS, y = singles_percent))+
  geom_point()+
  # facet_grid(~ bi0.25_bi, scales="free")+
  # geom_smooth(alpha = 0.2, se=F)+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Percentage (%)",
       title = "BIMODAL SPEEDS - 25% fast + low tortuosity\nPercentage of detected points which are single frames")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")
bi0.25_s_percent_plot



# comparing raw vs singles for just hmean & lnorm so that can put them on the same plot:
bi0.25_raw_singles_lnorm_hmean_df <- data.frame(aMRS=c(aMRS_bi0.25, aMRS_bi0.25, aMRS_bi0.25, aMRS_bi0.25),
                                             error=c(hmean_MRS_err_bi0.25/aMRS_bi0.25, lnorm_MRS_err_bi0.25/aMRS_bi0.25, hmean_MRS_err_bi0.25_sz/aMRS_bi0.25, lnorm_MRS_err_bi0.25_sz/aMRS_bi0.25),
                                             method=c(rep("hmean", times=44), rep("lnorm", times=44), rep("hmean", times=44), rep("lnorm", times=44)),
                                             scaling=c(rep("raw", times=88), rep("with singles", times=88)))

bi0.25_raw_singles_lnorm_hmean_plot <- ggplot(bi0.25_raw_singles_lnorm_hmean_df, aes(x = aMRS, y = error, colour=method))+
  geom_point()+
  # facet_grid(~ bi0.25_noscal_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F, aes(linetype=scaling))+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Proportional error",
       title = "BIMODAL SPEEDS - 25% fast + low tortuosity\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")






# bimodal plots - 90% of time spent moving -----------------------------------

# bimodal with 90% of time spent moving  (with full body mass scaling of detection probability) - raw
bi0.9_raw_df <- data.frame(aMRS=rep(aMRS_bi0.9, times=5),
                            error=c(aMOS_MRS_err_bi0.9/aMRS_bi0.9, hmean_MRS_err_bi0.9/aMRS_bi0.9, lnorm_MRS_err_bi0.9/aMRS_bi0.9, gamma_MRS_err_bi0.9/aMRS_bi0.9, weibull_MRS_err_bi0.9/aMRS_bi0.9),
                            method= c(rep("arithmetic", times=44), rep("hmean", times=44), rep("lnorm", times=44), rep("gamma", times=44), rep("weibull",times=44)))

bi0.9_raw_plot <- ggplot(bi0.9_raw_df, aes(x = aMRS, y = error, colour = method))+
  geom_point()+
  # facet_grid(~ bi0.9_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F)+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Proportional error",
       title = "BIMODAL SPEEDS - 90% fast + low tortuosity\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")
bi0.9_raw_plot  

# with singles added back in:
bi0.9_singles_df <- data.frame(aMRS=rep(aMRS_bi0.9, times=5),
                             error=c(aMOS_MRS_err_bi0.9_sz/aMRS_bi0.9, hmean_MRS_err_bi0.9_sz/aMRS_bi0.9, lnorm_MRS_err_bi0.9_sz/aMRS_bi0.9, gamma_MRS_err_bi0.9_sz/aMRS_bi0.9, weibull_MRS_err_bi0.9_sz/aMRS_bi0.9),
                             method= c(rep("arithmetic", times=44), rep("hmean", times=44), rep("lnorm", times=44), rep("gamma", times=44), rep("weibull",times=44)))

bi0.9_singles_plot <- ggplot(bi0.9_singles_df, aes(x = aMRS, y = error, colour = method))+
  geom_point()+
  # facet_grid(~ bi0.9_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F)+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Proportional error",
       title = "BIMODAL SPEEDS - 90% fast + low tortuosity\nwith singles added back in\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")
bi0.9_singles_plot


# percentage singles
bi0.9_s_percent_df <- data.frame(aMRS=aMRS_bi0.9,
                                  singles_percent=singles_percent_bi0.9)

bi0.9_s_percent_plot <- ggplot(bi0.9_s_percent_df, aes(x = aMRS, y = singles_percent))+
  geom_point()+
  # facet_grid(~ bi0.9_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F)+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Percentage (%)",
       title = "BIMODAL SPEEDS - 90% fast + low tortuosity\nPercentage of detected points which are single frames")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")
bi0.9_s_percent_plot

# comparing raw vs singles for just hmean & lnorm so that can put them on the same plot:
bi0.9_raw_singles_lnorm_hmean_df <- data.frame(aMRS=c(aMRS_bi0.9, aMRS_bi0.9, aMRS_bi0.9, aMRS_bi0.9),
                                                error=c(hmean_MRS_err_bi0.9/aMRS_bi0.9, lnorm_MRS_err_bi0.9/aMRS_bi0.9, hmean_MRS_err_bi0.9_sz/aMRS_bi0.9, lnorm_MRS_err_bi0.9_sz/aMRS_bi0.9),
                                                method=c(rep("hmean", times=44), rep("lnorm", times=44), rep("hmean", times=44), rep("lnorm", times=44)),
                                                scaling=c(rep("raw", times=88), rep("with singles", times=88)))

bi0.9_raw_singles_lnorm_hmean_plot <- ggplot(bi0.9_raw_singles_lnorm_hmean_df, aes(x = aMRS, y = error, colour=method))+
  geom_point()+
  # facet_grid(~ bi0.9_noscal_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F, aes(linetype=scaling))+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Proportional error",
       title = "BIMODAL SPEEDS - 90% fast + low tortuosity\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")





# arrangements comparing uni vs bi and raw vs singles  --------------------

uni_raw_plot
uni_singles_plot
# to better see what happens when you add the singles back in: combine into one plot using just hmean & lnorm:
uni_raw_singles_lnorm_hmean_plot
ggarrange(uni_raw_plot, uni_singles_plot, ncol=2) # arranged next to each other for all the models
uni_s_percent_plot # percentage singles

bi0.25_raw_plot
bi0.25_singles_plot
bi0.25_raw_singles_lnorm_hmean_plot
ggarrange(bi0.25_raw_plot, bi0.25_singles_plot, ncol=2)
bi0.25_s_percent_plot

bi0.9_raw_plot
bi0.9_singles_plot
bi0.9_raw_singles_lnorm_hmean_plot
ggarrange(bi0.9_raw_plot, bi0.9_singles_plot, ncol=2)
bi0.9_s_percent_plot

# percentage singles:
uni_s_percent_plot
bi0.25_s_percent_plot
bi0.9_s_percent_plot


## to compare uni vs bi:
uni_bi0.25_bi0.9_raw_scaling <- ggarrange(uni_raw_plot, bi0.25_raw_plot, bi0.9_raw_plot, ncol=3)
# save so that can view better:
png(file=paste0(parentfolder, "plots/uni_bi0.25_bi0.9_raw_scaling.png"),
    width=1300, height=400)
print(uni_bi0.25_bi0.9_raw_scaling)
dev.off()





# extract variables to compare scaling vs no scaling of det prob - for unimodal only --------

# without scaling:
aMRS_uni_noscal <- c()
aMOS_MRS_err_uni_noscal <- c()
aMOS_MRS_err_uni_noscal_sz <- c()
hmean_MRS_err_uni_noscal <- c()
hmean_MRS_err_uni_noscal_sz <- c()
lnorm_MRS_err_uni_noscal <- c()
lnorm_MRS_err_uni_noscal_sz <- c()
gamma_MRS_err_uni_noscal <- c()
gamma_MRS_err_uni_noscal_sz <- c()
weibull_MRS_err_uni_noscal <- c()
weibull_MRS_err_uni_noscal_sz <- c()
singles_percent_uni_noscal <- c()
for (i in Mb_range){
  load(paste0(parentfolder, "uni_hz_noscaling/plotting_variables/wedge0/Mb", i, "_iters1-5.RData"))
  
  mos_errors <- output$amMOS - output$aMRS
  mos_errors_sz <- output$amMOS_sz - output$aMRS
  
  hmean_errors <- output$hmean_m - output$aMRS
  hmean_errors_sz <- output$hmean_m_sz - output$aMRS
  
  lnorm_errors <- output$lnorm_m - output$aMRS
  lnorm_errors_sz <- output$lnorm_m_sz - output$aMRS
  
  gamma_errors <- output$gamma_m - output$aMRS
  gamma_errors_sz <- output$gamma_m_sz - output$aMRS
  
  weibull_errors <- output$weibull_m - output$aMRS
  weibull_errors_sz <- output$weibull_m_sz - output$aMRS
  
  s_perc <- (output$n_singles/output$n_detected)*100
  
  if (length(output$aMRS)==5){ # if there were 5 fitted models, just use the first 4
    
    aMRS_uni_noscal <- c(aMRS_uni_noscal, output$aMRS[-5])
    
    aMOS_MRS_err_uni_noscal <- c(aMOS_MRS_err_uni_noscal, mos_errors[-5])
    aMOS_MRS_err_uni_noscal_sz <- c(aMOS_MRS_err_uni_noscal_sz, mos_errors_sz[-5])
    
    hmean_MRS_err_uni_noscal <- c(hmean_MRS_err_uni_noscal, hmean_errors[-5])
    hmean_MRS_err_uni_noscal_sz <- c(hmean_MRS_err_uni_noscal_sz, hmean_errors_sz[-5])
    
    lnorm_MRS_err_uni_noscal <- c(lnorm_MRS_err_uni_noscal, lnorm_errors[-5])
    lnorm_MRS_err_uni_noscal_sz <- c(lnorm_MRS_err_uni_noscal_sz, lnorm_errors_sz[-5])
    
    gamma_MRS_err_uni_noscal <- c(gamma_MRS_err_uni_noscal, gamma_errors[-5])
    gamma_MRS_err_uni_noscal_sz <- c(gamma_MRS_err_uni_noscal_sz, gamma_errors[-5])
    
    weibull_MRS_err_uni_noscal <- c(weibull_MRS_err_uni_noscal, weibull_errors[-5])
    weibull_MRS_err_uni_noscal_sz <- c(weibull_MRS_err_uni_noscal_sz, weibull_errors_sz[-5])
    
    singles_percent_uni_noscal <- c(singles_percent_uni_noscal, s_perc[-5])
  }
  else {
    aMRS_uni_noscal <- c(aMRS_uni_noscal, output$aMRS)
    
    aMOS_MRS_err_uni_noscal <- c(aMOS_MRS_err_uni_noscal, mos_errors)
    aMOS_MRS_err_uni_noscal_sz <- c(aMOS_MRS_err_uni_noscal_sz, mos_errors_sz)
    
    hmean_MRS_err_uni_noscal <- c(hmean_MRS_err_uni_noscal, hmean_errors)
    hmean_MRS_err_uni_noscal_sz <- c(hmean_MRS_err_uni_noscal_sz, hmean_errors_sz)
    
    lnorm_MRS_err_uni_noscal <- c(lnorm_MRS_err_uni_noscal, lnorm_errors)
    lnorm_MRS_err_uni_noscal_sz <- c(lnorm_MRS_err_uni_noscal_sz, lnorm_errors_sz)
    
    gamma_MRS_err_uni_noscal <- c(gamma_MRS_err_uni_noscal, gamma_errors)
    gamma_MRS_err_uni_noscal_sz <- c(gamma_MRS_err_uni_noscal_sz, gamma_errors)
    
    weibull_MRS_err_uni_noscal <- c(weibull_MRS_err_uni_noscal, weibull_errors)
    weibull_MRS_err_uni_noscal_sz <- c(weibull_MRS_err_uni_noscal_sz, weibull_errors_sz)
    
    singles_percent_uni_noscal <- c(singles_percent_uni_noscal, s_perc)
  }
  rm(list = c("output"))
}



# make plots for uni without scaling --------------------------------------

## plot for uni_noscal modal with hz_scaling detection distance - raw biases
uni_noscal_raw_df <- data.frame(aMRS=rep(aMRS_uni_noscal, times=5),
                         error=c(aMOS_MRS_err_uni_noscal/aMRS_uni_noscal, hmean_MRS_err_uni_noscal/aMRS_uni_noscal, lnorm_MRS_err_uni_noscal/aMRS_uni_noscal, gamma_MRS_err_uni_noscal/aMRS_uni_noscal, weibull_MRS_err_uni_noscal/aMRS_uni_noscal),
                         method= c(rep("arithmetic", times=44), rep("hmean", times=44), rep("lnorm", times=44), rep("gamma", times=44), rep("weibull",times=44)))

uni_noscal_raw_plot <- ggplot(uni_noscal_raw_df, aes(x = aMRS, y = error, colour = method))+
  geom_point()+
  # facet_grid(~ uni_noscal_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F)+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Proportional error",
       title = "UNIMODAL SPEEDS\nno body mass scaling of detection probability\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")
uni_noscal_raw_plot

# with singles added back in:
uni_noscal_singles_df <- data.frame(aMRS=rep(aMRS_uni_noscal, times=5),
                             error=c(aMOS_MRS_err_uni_noscal_sz/aMRS_uni_noscal, hmean_MRS_err_uni_noscal_sz/aMRS_uni_noscal, lnorm_MRS_err_uni_noscal_sz/aMRS_uni_noscal, gamma_MRS_err_uni_noscal_sz/aMRS_uni_noscal, weibull_MRS_err_uni_noscal_sz/aMRS_uni_noscal),
                             method= c(rep("arithmetic", times=44), rep("hmean", times=44), rep("lnorm", times=44), rep("gamma", times=44), rep("weibull",times=44)))

uni_noscal_singles_plot <- ggplot(uni_noscal_singles_df, aes(x = aMRS, y = error, colour = method))+
  geom_point()+
  # facet_grid(~ uni_noscal_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F)+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Proportional error",
       title = "UNIMODAL SPEEDS\nno body mass scaling of detection probability\nwith singles added back in\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")
uni_noscal_singles_plot

# percentage singles (to explain the change when add singles back in):
uni_noscal_s_percent_df <- data.frame(aMRS=aMRS_uni_noscal,
                               singles_percent=singles_percent_uni_noscal)

uni_noscal_s_percent_plot <- ggplot(uni_noscal_s_percent_df, aes(x = aMRS, y = singles_percent))+
  geom_point()+
  # facet_grid(~ uni_noscal_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F)+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Percentage (%)",
       title = "UNIMODAL SPEEDS\nno body mass scaling of detection probability\nPercentage of detected points which are single frames")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")
uni_noscal_s_percent_plot



# comparing scaling vs no scaling for just hmean & lnorm so that can put them on the same plot:
scal_noscal_lnorm_hmean_df <- data.frame(aMRS=c(aMRS_uni_scal, aMRS_uni_scal, aMRS_uni_noscal, aMRS_uni_noscal),
                                         error=c(hmean_MRS_err_uni/aMRS_uni_scal, lnorm_MRS_err_uni/aMRS_uni_scal, hmean_MRS_err_uni_noscal/aMRS_uni_scal, lnorm_MRS_err_uni_noscal/aMRS_uni_scal),
                                         method=c(rep("hmean", times=44), rep("lnorm", times=44), rep("hmean", times=44), rep("lnorm", times=44)),
                                         scaling=c(rep("scaling", times=88), rep("no scaling", times=88)))

scal_noscal_lnorm_hmean_plot <- ggplot(scal_noscal_lnorm_hmean_df, aes(x = aMRS, y = error, colour=method))+
  geom_point()+
  # facet_grid(~ uni_noscal_bi, scales="free")+
  geom_smooth(alpha = 0.2, se=F, aes(linetype=scaling))+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
  labs(x = "Mean realised speed (m/s)",
       y = "Proportional error",
       title = "UNIMODAL SPEEDS\nErrors between MRS and estimated speeds\n(+ve: est>MRS)")+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.position = "bottom")


# compare between uni scaling vs no scaling -------------------------------

# with scaling:
uni_raw_plot
uni_singles_plot
uni_s_percent_plot

# without scaling:
uni_noscal_raw_plot
uni_noscal_singles_plot
uni_noscal_s_percent_plot

# compare raws:
ggarrange(uni_raw_plot, uni_noscal_raw_plot, ncol=2)
# to better see this comparison: just use lnorm and hmean and put them on the same plot:
scal_noscal_lnorm_hmean_plot 
