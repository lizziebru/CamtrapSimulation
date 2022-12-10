## PLOTS FOR THE MANUSCRIPT ONLY ##
# make summary plots using data generated in generate_plotting_variables function & do statistical tests

require(ggplot2)
require(ggpubr)
require(colortools)
require(colorBlindness)
require(grid)   # for the textGrob() function

#################### FIGURES made in this script ##############################################

# both_biases_correction (first figure in results)
# improv (second figure in results)
# bodymass (third figure in results)


## OTHER figures in manuscript that aren't made in this script:
# flowchart (figure at end of intro) - made using MS Visio - see OneDrive plots folder for .vsdx file


# extract variables for plotting ----------------------------------

## extract radial distances and number of sampled speeds for each simulation run

radial_dists <- c()
mb <- c()
sampled_no <- c()

folderpaths <- c("unimodal_seqdats/seq_dats/", "bimodal_seqdats/seq_dats/mov0.1/", "bimodal_seqdats/seq_dats/mov0.25/", "bimodal_seqdats/seq_dats/mov0.4/", "bimodal_seqdats/seq_dats/mov0.6/", "bimodal_seqdats/seq_dats/mov0.75/", "bimodal_seqdats/seq_dats/mov0.9/")
mb_range <- c(1,5,10,15,20,25,30,35,40,45,50)

for (f in folderpaths){ # loop through each type movement type
  for (i in mb_range){ # loop through each body mass
    for (j in 1:20){ # loop through each iteration
      load(paste0("../seqdats/", f, "Mb", i, "iter", j, ".RData"))
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


## extract all other required variables:

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

folderpaths <- c("unimodal_seqdats/plotting_variables/", "bimodal_seqdats/plotting_variables/mov0.1/", "bimodal_seqdats/plotting_variables/mov0.25/", "bimodal_seqdats/plotting_variables/mov0.4/", "bimodal_seqdats/plotting_variables/mov0.6/", "bimodal_seqdats/plotting_variables/mov0.75/", "bimodal_seqdats/plotting_variables/mov0.9/")

for (f in folderpaths){ # loop through each type movement type
  load(paste0("../seqdats/", f, "Mb_all_iters1-20.RData"))
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
        
        
        if (f == "unimodal_seqdats/plotting_variables/"){
          mov_type <- c(mov_type, 0)
        }
        if (f == "bimodal_seqdats/plotting_variables/mov0.1/"){
          mov_type <- c(mov_type, 0.1)
        }
        if (f == "bimodal_seqdats/plotting_variables/mov0.25/"){
          mov_type <- c(mov_type, 0.25)
        }
        if (f == "bimodal_seqdats/plotting_variables/mov0.4/"){
          mov_type <- c(mov_type, 0.4)
        }
        if (f == "bimodal_seqdats/plotting_variables/mov0.6/"){
          mov_type <- c(mov_type, 0.6)
        }
        if (f == "bimodal_seqdats/plotting_variables/mov0.75/"){
          mov_type <- c(mov_type, 0.7)
        }
        if (f == "bimodal_seqdats/plotting_variables/mov0.9/"){
          mov_type <- c(mov_type, 0.9)
        }
      }
    }
  }
}






# improv figure & t-tests -----------------------------------------

# to remind myself which way round it goes (ignore this if you have a normal brain)
# before: -5
# after: -3
# improvement: after-before = -3--5 = -3 + 5 = +2

# improv plot: what happens to the harmonic mean & SBM estimates when you correct the sampling bias 

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

# save
png(file=paste0("../plots/improv.png"), width = 7, height = 5, units = 'in', res = 300)
print(improv)
dev.off()

# t-test for improvement for hmean & lnorm:
hmean_improv <- na.omit((hmean_sz-MRS)/MRS-(hmean-MRS)/MRS)
t.test(hmean_improv, alternative="greater")
lnorm_improv <- na.omit((lnorm_sz-MRS)/MRS-(lnorm-MRS)/MRS)
t.test(lnorm_improv, alternative="greater")




# both_biases_correction figure ----------------------------------------------------

# make dataframe
improv2_df <- data.frame(bias = c((ar-MRS)/MRS, (hmean-MRS)/MRS, (lnorm-MRS)/MRS, (gamma-MRS)/MRS, (weibull-MRS)/MRS, (ar_sz-MRS)/MRS, (hmean_sz-MRS)/MRS, (lnorm_sz-MRS)/MRS, (gamma_sz-MRS)/MRS, (weibull_sz-MRS)/MRS),
                         MRS = c(rep(MRS, times=10)),
                         mov_type = c(rep(mov_type, times=10)),
                         type = factor(c(rep("Sampling bias not corrected", times=7700), rep("Sampling bias corrected", times=7700)), levels = c("Sampling bias not corrected", "Sampling bias corrected")),
                         Method = factor(c(rep(c(rep("Uncorrected", times=1540), rep("Harmonic", times=1540), rep("Log-normal", times=1540), rep("Gamma", times=1540), rep("Weibull", times=1540)), times=2)), levels = c("Uncorrected", "Harmonic", "Log-normal", "Gamma", "Weibull")))
improv2_df <- na.omit(improv2_df) # remove rows where bias and MRS are NA

# 2 separate plots
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

improv_arranged <- ggarrange(improv_nocorrection + rremove("ylab") + rremove("xlab"), improv_correction + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
                             labels = c("A", "B"),
                             ncol = 2,
                             common.legend = TRUE, legend = "right")
# align = "hv", 
# font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

improv_annotated <- annotate_figure(improv_arranged, left = textGrob("Bias [(estimated mean/true travel speed) - 1]", rot = 90, vjust = 1, gp = gpar(cex = 1.2)),
                                    bottom = textGrob("True travel speed (m/s)", hjust=0.7, gp = gpar(cex = 1.2)))
improv_annotated

# save
png(file=paste0("../plots/both_biases_correction.png"), width = 7, height = 4.8, units = 'in', res = 300)
print(improv_annotated)
dev.off()



# bodymass figure ---------------------------------------------------------

## show how prop of singles & zeros stays constant across body masses bc of effects of body mass on dz dimensions
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
# guides(col=guide_legend("Estimation method"))
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
fig_mb_missed

# save
png(file=paste0("../plots/bodymass.png"), width = 7, height = 8, units = 'in', res = 300)
print(fig_mb_missed)
dev.off()


# GLM for effect of movement type on bias ---------------------------------

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

