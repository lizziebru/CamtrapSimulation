## MAKING PLOTS ##
# make summary plots using data generated in generate_plotting_variables function

require(ggplot2)
require(ggpubr)
require(colortools)
require(colorBlindness)


# FIGURE 1: compare biases for diff estimation methods for uni-modal movement only -------------------------------------

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

# maybe add the points back in, and make some of the lines dashed (make the darkest lines dashed)
# probs change font too
# get rid of legend title
# make panel titles clearer maybe


# SUPP FIG 1: check that hmean is indeed consistently pretty good for different movement behaviours -------------------

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



# FIGURE 2: comparing biases of hmean across movement behaviours -------------------

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

for (i in 1:7){
  selection <- (220*(i-1)+1):(220*i)
  # select the relevant chunk, work out all the biases, and average them out

}

fig2_df <- data.frame()






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




# FIG 3: mean of means vs point-to-point?  -------------------------------------------------------------------




# FIG 4: using diff parts of the wedge? -----------------------------------








# extract variables to plot biases to compare across uni/bimodal - using the whole wedge + full distance det prob scaling  ---------



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
