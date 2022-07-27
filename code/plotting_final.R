## MAKING PLOTS ##
# make summary plots using data generated in generate_plotting_variables function

require(ggplot2)
require(ggpubr)

parentfolder <- paste0("../results/final_results/")

# set which speed parameters to analyse
Mb_range <- c(1,5,10,15,20,25,30,35,40,45,50)


# extract variables to plot biases to compare across uni/bimodal - using the whole wedge + full distance det prob scaling  ---------

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
