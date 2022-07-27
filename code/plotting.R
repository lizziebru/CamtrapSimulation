## MAKING PLOTS ##

# make summary plots using data generated in generate_plotting_variables function


  
  ## new set of plots:
  
  # plots comparing biases across uni/bimodal scenarios - incl raw data errors, sz data errors, and percentage singles
  
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
  # should have 4 reps (some of them didn't fit the models for all 5) for each 11 body masses for each of uni, bi0.25, and bi0.9 --i.e. 132 of each
  uni_bi <- c(rep("uni", times =44), rep("bi0.25", times=44), rep("bi0.9", times=44))
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
  
  # for bimodal0.25:
  aMRS_bi0.25 <- c()
  aMOS_MRS_err_bi0.25 <- c()
  aMOS_MRS_err_bi0.25_sz <- c()
  hmean_MRS_err_bi0.25 <- c()
  hmean_MRS_err_bi0.25_sz <- c()
  lnorm_MRS_err_bi0.25 <- c()
  lnorm_MRS_err_bi0.25_sz <- c()
  gamma_MRS_err_bi0.25 <- c()
  gamma_MRS_err_bi0.25_sz <- c()
  weibull_MRS_err_bi0.25 <- c()
  weibull_MRS_err_bi0.25_sz <- c()
  singles_percent_bi0.25 <- c()
  for (i in Mb_range){
    load(paste0(parentfolder, "bi_hz_scaling/plotting_variables/mov0.25/wedge0/Mb", i, "_iters1-5.RData"))
    
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
      
      aMRS_bi0.25 <- c(aMRS_bi0.25, output$aMRS[-5])
      
      aMOS_MRS_err_bi0.25 <- c(aMOS_MRS_err_bi0.25, mos_errors[-5])
      aMOS_MRS_err_bi0.25_sz <- c(aMOS_MRS_err_bi0.25_sz, mos_errors_sz[-5])
      
      hmean_MRS_err_bi0.25 <- c(hmean_MRS_err_bi0.25, hmean_errors[-5])
      hmean_MRS_err_bi0.25_sz <- c(hmean_MRS_err_bi0.25_sz, hmean_errors_sz[-5])
      
      lnorm_MRS_err_bi0.25 <- c(lnorm_MRS_err_bi0.25, lnorm_errors[-5])
      lnorm_MRS_err_bi0.25_sz <- c(lnorm_MRS_err_bi0.25_sz, lnorm_errors_sz[-5])
      
      gamma_MRS_err_bi0.25 <- c(gamma_MRS_err_bi0.25, gamma_errors[-5])
      gamma_MRS_err_bi0.25_sz <- c(gamma_MRS_err_bi0.25_sz, gamma_errors[-5])
      
      weibull_MRS_err_bi0.25 <- c(weibull_MRS_err_bi0.25, weibull_errors[-5])
      weibull_MRS_err_bi0.25_sz <- c(weibull_MRS_err_bi0.25_sz, weibull_errors_sz[-5])
      
      singles_percent_bi0.25 <- c(singles_percent_bi0.25, s_perc[-5])
    }
    else {
      aMRS_bi0.25 <- c(aMRS_bi0.25, output$aMRS)
      
      aMOS_MRS_err_bi0.25 <- c(aMOS_MRS_err_bi0.25, mos_errors)
      aMOS_MRS_err_bi0.25_sz <- c(aMOS_MRS_err_bi0.25_sz, mos_errors_sz)
      
      hmean_MRS_err_bi0.25 <- c(hmean_MRS_err_bi0.25, hmean_errors)
      hmean_MRS_err_bi0.25_sz <- c(hmean_MRS_err_bi0.25_sz, hmean_errors_sz)
      
      lnorm_MRS_err_bi0.25 <- c(lnorm_MRS_err_bi0.25, lnorm_errors)
      lnorm_MRS_err_bi0.25_sz <- c(lnorm_MRS_err_bi0.25_sz, lnorm_errors_sz)
      
      gamma_MRS_err_bi0.25 <- c(gamma_MRS_err_bi0.25, gamma_errors)
      gamma_MRS_err_bi0.25_sz <- c(gamma_MRS_err_bi0.25_sz, gamma_errors)
      
      weibull_MRS_err_bi0.25 <- c(weibull_MRS_err_bi0.25, weibull_errors)
      weibull_MRS_err_bi0.25_sz <- c(weibull_MRS_err_bi0.25_sz, weibull_errors_sz)
      
      singles_percent_bi0.25 <- c(singles_percent_bi0.25, s_perc)
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
  aMRS <- c(aMRS_uni, aMRS_bi0.25, aMRS_bi0.9)
  aMOS_MRS_err <- c(aMOS_MRS_err_uni, aMOS_MRS_err_bi0.25, aMOS_MRS_err_bi0.9) 
  aMOS_MRS_err_sz <- c(aMOS_MRS_err_uni_sz, aMOS_MRS_err_bi0.25_sz, aMOS_MRS_err_bi0.9_sz) 
  hmean_MRS_err <- c(hmean_MRS_err_uni, hmean_MRS_err_bi0.25, hmean_MRS_err_bi0.9) 
  hmean_MRS_err_sz <- c(hmean_MRS_err_uni_sz, hmean_MRS_err_bi0.25_sz, hmean_MRS_err_bi0.9_sz) 
  lnorm_MRS_err <- c(lnorm_MRS_err_uni, lnorm_MRS_err_bi0.25, lnorm_MRS_err_bi0.9) 
  lnorm_MRS_err_sz <- c(lnorm_MRS_err_uni_sz, lnorm_MRS_err_bi0.25_sz, lnorm_MRS_err_bi0.9_sz) 
  gamma_MRS_err <- c(gamma_MRS_err_uni, gamma_MRS_err_bi0.25, gamma_MRS_err_bi0.9) 
  gamma_MRS_err_sz <- c(gamma_MRS_err_uni_sz, gamma_MRS_err_bi0.25_sz, gamma_MRS_err_bi0.9_sz) 
  weibull_MRS_err <- c(weibull_MRS_err_uni, weibull_MRS_err_bi0.25, weibull_MRS_err_bi0.9)
  weibull_MRS_err_sz <- c(weibull_MRS_err_uni_sz, weibull_MRS_err_bi0.25_sz, weibull_MRS_err_bi0.9_sz)
  singles_percent <- c(singles_percent_uni, singles_percent_bi0.25, singles_percent_bi0.9) 
  
  biases_uni_bi0.250.9_4reps_sonly_df_plots1_2 <- data.frame(aMRS=rep(aMRS, times=10), Mb= rep(Mb, times=10), uni_bi = rep(uni_bi, times=10),
                                                             error = c(aMOS_MRS_err, aMOS_MRS_err_sz, hmean_MRS_err, hmean_MRS_err_sz, lnorm_MRS_err, lnorm_MRS_err_sz, gamma_MRS_err, gamma_MRS_err_sz, weibull_MRS_err, weibull_MRS_err_sz),
                                                             method = c(rep("arithmetic", times=8), rep("hmean", times=8), rep("lnorm", times=8), rep("gamma", times=8), rep("weibull", times=8)),
                                                             type = c(rep(c("raw", "raw", "raw", "raw","with_sz","with_sz","with_sz","with_sz"),times=5)))
  
  write.csv(biases_uni_bi0.250.9_4reps_sonly_df_plots1_2, file = "../results/final_results/biases_uni_bi0.250.9_4reps_sonly_df_plots1_2")
  
  # plot with just raw frames
  biases_uni_bi0.250.9_4reps_sonly_rawplot <- ggplot(biases_uni_bi0.250.9_4reps_sonly_df_plots1_2[biases_uni_bi0.250.9_4reps_sonly_df_plots1_2$type=="raw",], aes(x = aMRS, y = error, colour = method))+
    geom_point()+
    facet_grid(~ uni_bi, scales="free")+
    # geom_smooth(alpha = 0.2, se=F)+
    theme_minimal()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
    labs(x = "Mean realised speed (m/s)",
         y = "Error (m/s)",
         title = "Errors between MRS and estimated speeds\n(+ve: est>MRS)")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          title = element_text(size = 13),
          strip.text = element_text(size = 13))
  biases_uni_bi0.250.9_4reps_sonly_rawplot
  
  #-- not very clear -- do each 3 separately
  ## something's clearly messed up in larger df so just do things separately for now:
  
  ## plot for uni modal with hz_scaling detection distance - raw biases
  uni_raw_df <- data.frame(aMRS=rep(aMRS_uni, times=5),
                           error=c(aMOS_MRS_err_uni, hmean_MRS_err_uni, lnorm_MRS_err_uni, gamma_MRS_err_uni, weibull_MRS_err_uni),
                           method= c(rep("arithmetic", times=44), rep("hmean", times=44), rep("lnorm", times=44), rep("gamma", times=44), rep("weibull",times=44)))
  
  uni_raw_plot <- ggplot(biases_uni_bi0.250.9_4reps_sonly_df_plots1_2[uni_raw_df, aes(x = aMRS, y = error, colour = method))+
    geom_point()+
    # facet_grid(~ uni_bi, scales="free")+
    geom_smooth(alpha = 0.2, se=F)+
    theme_minimal()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
    labs(x = "Mean realised speed (m/s)",
         y = "Error (m/s)",
         title = "Errors between MRS and estimated speeds\n(+ve: est>MRS)")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          title = element_text(size = 13),
          strip.text = element_text(size = 13))
  uni_raw_plot
  
  
  
  # plot with singles too
  
  
  # plot of percentage singles
  biases_uni_bi0.250.9_4reps_sonly_singles_df <- data.frame(uni_bi =uni_bi,
                                                            singles_percent=singles_percent,
                                                            aMRS=aMRS)
  biases_uni_bi0.250.9_4reps_sonly_singles_plot <- ggplot(biases_uni_bi0.250.9_4reps_sonly_singles_df, aes(x = aMRS, y = singles_percent, colour = uni_bi))+
    geom_point()+
    # geom_smooth(alpha = 0.2, se=F)+
    theme_minimal()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    # scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
    labs(x = "Mean realised speed (m/s)",
         y = "Percentage (%)",
         title = "Percentage of detected points that are single frames")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          title = element_text(size = 13),
          strip.text = element_text(size = 13))
  biases_uni_bi0.250.9_4reps_sonly_singles_plot
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # initialise vectors to fill massive dataframe
  Mb <- c()
  iter <- c()
  aMRS <- c()
  amMOS <- c()
  amMOS_sz <- c()
  apMOS <- c()
  apMOS_sz <- c()
  hmean_m <- c() 
  hmean_m_sz <- c()
  hmean_p <- c()
  hmean_p_sz <- c()
  lnorm_m <- c()
  lnorm_m_sz <- c()
  lnorm_p <- c() 
  lnorm_p_sz <- c()
  gamma_m <- c()
  gamma_m_sz <- c() 
  gamma_p <- c()
  gamma_p_sz <- c()
  weibull_m <- c()
  weibull_m_sz <- c()
  weibull_p <- c()
  weibull_p_sz <- c()
  n_zeros <- c()
  n_singles <- c()
  singles_v_mean <- c()
  zeros_v_mean <- c()
  n_points <- c()
  n_detected <- c()
  
  for (i in Mb_range){
    
    # load in data generated in generate_plotting_variables function
    load(paste0(parentfolder, "plotting_data/wedge", part_of_wedge, "/Mb", i, "_iters1-", Mb_iters$iter[1], ".RData"))
    
    # fill vectors
    Mb <- c(Mb, rep(i, times = Mb_iters$iter[1]))
    iter <- c(iter, seq(from=1, to=Mb_iters$iter[1], by=1))
    aMRS <- c(aMRS, output$aMRS)
    amMOS <- c(amMOS, output$amMOS)
    amMOS_sz <- c(amMOS_sz, output$amMOS_sz)
    apMOS <- c(apMOS, output$apMOS)
    apMOS_sz <- c(apMOS_sz, output$apMOS_sz)
    hmean_m <- c(hmean_m, output$hmean_m) 
    hmean_m_sz <- c(hmean_m_sz, output$hmean_m_sz)
    hmean_p <- c(hmean_p, output$hmean_p)
    hmean_p_sz <- c(hmean_p_sz, output$hmean_p_sz)
    lnorm_m <- c(lnorm_m, output$lnorm_m)
    lnorm_m_sz <- c(lnorm_m_sz, output$lnorm_m_sz)
    lnorm_p <- c(lnorm_p, output$lnorm_p) 
    lnorm_p_sz <- c(lnorm_p_sz, output$lnorm_p_sz)
    gamma_m <- c(gamma_m, output$gamma_m)
    gamma_m_sz <- c(gamma_m_sz, output$gamma_m_sz) 
    gamma_p <- c(gamma_p, output$gamma_p)
    gamma_p_sz <- c(gamma_p_sz, output$gamma_p_sz)
    weibull_m <- c(weibull_m, output$weibull_m)
    weibull_m_sz <- c(weibull_m_sz, output$weibull_m_sz)
    weibull_p <- c(weibull_p, output$weibull_p)
    weibull_p_sz <- c(weibull_p_sz, output$weibull_p_sz)
    n_zeros <- c(n_zeros, output$n_zeros)
    n_singles <- c(n_singles, output$n_singles)
    singles_v_mean <- c(singles_v_mean, output$singles_v_mean)
    zeros_v_mean <- c(zeros_v_mean, output$zeros_v_mean)
    n_points <- c(n_points, output$n_points)
    n_detected <- c(n_detected, output$n_detected)
  }
  
  if (part_of_wedge==0){
    wedgename2 <- "whole"
  }
  if (part_of_wedge==1){
    wedgename2 <- "bottom"
  }
  if (part_of_wedge==2){
    wedgename2 <- "middle"
  }
  if (part_of_wedge==3){
    wedgename2 <- "top"
  }
  
  main_df <- data.frame(wedge=rep(wedgename2, times=length(Mb)) , Mb=Mb, iter=iter, aMRS=aMRS, amMOS=amMOS, amMOS_sz=amMOS_sz, apMOS=apMOS, apMOS_sz=apMOS_sz, hmean_m=hmean_m,
                        hmean_m_sz=hmean_m_sz, hmean_p=hmean_p, hmean_p_sz=hmean_p_sz, lnorm_m=lnorm_m, lnorm_m_sz=lnorm_m_sz,
                        lnorm_p=lnorm_p, lnorm_p_sz=lnorm_p_sz, gamma_m=gamma_m, gamma_m_sz=gamma_m_sz, gamma_p=gamma_p,
                        gamma_p_sz=gamma_p_sz, weibull_m=weibull_m, weibull_m_sz=weibull_m_sz, weibull_p=weibull_p, weibull_p_sz,
                        n_zeros=n_zeros, n_singles=n_singles, singles_v_mean=singles_v_mean, zeros_v_mean=zeros_v_mean, n_points=n_points, n_detected=n_detected)                        
  # write.csv(main_df, paste0("../Mb_results/08Jul22_1602/plotting_data/wedge", part_of_wedge, "/main_df_wedge", part_of_wedge, ".csv"))
  
  
  
  
  
  
  
  
  
  
  ## plots:
  
  # read back in CSVs - in each wedge folder
  
  # 4 plots ggarranged for errors between MRS-MOS and MRS-EST for each of m and p method
  
  ## mean of means
  
  # MRS-MOS using m method - with both raw and sz
  mrs_mos_m_df <- data.frame(aMRS=rep(aMRS, times=2), error=c((amMOS-aMRS), (amMOS_sz-aMRS)), type=c(rep("raw", times=length(aMRS)), rep("with_sz", times=length(aMRS))))
  
  mrs_mos_m_plot <- ggplot(mrs_mos_m_df, aes(x = aMRS, y = error, colour = type))+
    geom_point()+
    geom_smooth(alpha=0.3, se=F)+
    labs(x = "Mean realised speed (m/s)",
         y = "error (m/s)",
         title = "Errors between MRS and MOS\n(+ve: obs > MRS, -ve: MRS > obs)")+
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme_minimal()+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          title = element_text(size = 13),
          legend.title = element_blank(),
          legend.text = element_text(size = 15))
  mrs_mos_m_plot
  
  # MRS-est using m method - with just raw
  mrs_est_m_df <- data.frame(aMRS = c(rep(aMRS, times = 8)),
                             error = -c((hmean_m-aMRS), (lnorm_m-aMRS), (gamma_m-aMRS), (weibull_m-aMRS), (hmean_m_sz-aMRS), (lnorm_m_sz-aMRS), (gamma_m_sz-aMRS), (weibull_m_sz-aMRS)),
                             method = c(rep("hmean", times=length(aMRS)), rep("lnorm", times=length(aMRS)), rep("gamma", times=length(aMRS)), rep("weibull", times=length(aMRS)), rep("hmean", times=length(aMRS)), rep("lnorm", times=length(aMRS)), rep("gamma", times=length(aMRS)), rep("weibull", times=length(aMRS))),
                             type = c(rep("raw", times=length(aMRS)*4), rep("with_sz", times=length(aMRS)*4)))
  
  mrs_est_m_plot <- ggplot(mrs_est_m_df, aes(x = aMRS, y = error, colour = method))+
    geom_point()+
    facet_grid(type ~ .)+
    geom_smooth(alpha = 0.2, se=F)+
    theme_minimal()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
    labs(x = "Mean realised speed (m/s)",
         y = "Error (m/s)",
         title = "Errors between MRS and estimated speeds\n(+ve: MRS > est, -ve: est > MRS)")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          title = element_text(size = 13),
          strip.text = element_text(size = 13))
  mrs_est_m_plot
  
  
  ## point-to-point
  
  # MRS-MOS using p method - with both raw and sz
  mrs_mos_p_df <- data.frame(aMRS=rep(aMRS, times=2), error=c((apMOS-aMRS), (apMOS_sz-aMRS)), type=c(rep("raw", times=length(aMRS)), rep("with_sz", times=length(aMRS))))
  
  mrs_mos_p_plot <- ggplot(mrs_mos_p_df, aes(x = aMRS, y = error, colour = type))+
    geom_point()+
    geom_smooth(alpha=0.3, se=F)+
    labs(x = "Mean realised speed (m/s)",
         y = "error (m/s)",
         title = "Errors between MRS and MOS\n(+ve: obs > MRS, -ve: MRS > obs)")+
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme_minimal()+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          title = element_text(size = 13),
          legend.title = element_blank(),
          legend.text = element_text(size = 15))
  mrs_mos_p_plot
  
  # MRS-est using p method - with just raw
  mrs_est_p_df <- data.frame(aMRS = c(rep(aMRS, times = 8)),
                             error = -c((hmean_p-aMRS), (lnorm_p-aMRS), (gamma_p-aMRS), (weibull_p-aMRS), (hmean_p_sz-aMRS), (lnorm_p_sz-aMRS), (gamma_p_sz-aMRS), (weibull_p_sz-aMRS)),
                             method = c(rep("hmean", times=length(aMRS)), rep("lnorm", times=length(aMRS)), rep("gamma", times=length(aMRS)), rep("weibull", times=length(aMRS)), rep("hmean", times=length(aMRS)), rep("lnorm", times=length(aMRS)), rep("gamma", times=length(aMRS)), rep("weibull", times=length(aMRS))),
                             type = c(rep("raw", times=length(aMRS)*4), rep("with_sz", times=length(aMRS)*4)))
  
  mrs_est_p_plot <- ggplot(mrs_est_p_df, aes(x = aMRS, y = error, colour = method))+
    geom_point()+
    facet_grid(type ~ .)+
    geom_smooth(alpha = 0.2, se=F)+
    theme_minimal()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    scale_colour_manual(values = c("#FF0000", "#00FF66", "#0066FF", "#CC00FF"))+
    labs(x = "Mean realised speed (m/s)",
         y = "Error (m/s)",
         title = "Errors between MRS and estimated speeds\n(+ve: MRS > est, -ve: est > MRS)")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          title = element_text(size = 13),
          strip.text = element_text(size = 13))
  mrs_est_p_plot
  
  
  
  
  ## new plot: combined:
  
  ## mean of means:
  
  m_of_m_df <- data.frame(aMRS = c(rep(aMRS, times = 10)),
                          error = c((hmean_m-aMRS), (lnorm_m-aMRS), (gamma_m-aMRS), (weibull_m-aMRS), (amMOS-aMRS), (hmean_m_sz-aMRS), (lnorm_m_sz-aMRS), (gamma_m_sz-aMRS), (weibull_m_sz-aMRS), (amMOS_sz-aMRS)),
                          method = c(rep("hmean", times=length(aMRS)), rep("lnorm", times=length(aMRS)), rep("gamma", times=length(aMRS)), rep("weibull", times=length(aMRS)), rep("arithmetic", times=length(aMRS)), rep("hmean", times=length(aMRS)), rep("lnorm", times=length(aMRS)), rep("gamma", times=length(aMRS)), rep("weibull", times=length(aMRS)), rep("arithmetic", times=length(aMRS))),
                          type = c(rep("raw", times=length(aMRS)*5), rep("with_sz", times=length(aMRS)*5)))
  
  m_of_m_plot <- ggplot(m_of_m_df, aes(x = aMRS, y = error, colour = method))+
    geom_point()+
    facet_grid(type ~ .)+
    geom_smooth(alpha = 0.2, se=F)+
    theme_minimal()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    scale_colour_manual(values = c("#FF0000", "#00FF00", "#00FFFF", "#FF00FF", "#0000FF"))+
    labs(x = "Mean realised speed (m/s)",
         y = "Error (m/s)",
         title = "Errors between MRS and estimated speeds\n(+ve: est > MRS)")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          title = element_text(size = 13),
          strip.text = element_text(size = 13),
          legend.position = "bottom")
  m_of_m_plot
  
  
  ## point-to-point:
  
  p_to_p_df <- data.frame(aMRS = c(rep(aMRS, times = 10)),
                          error = c((hmean_p-aMRS), (lnorm_p-aMRS), (gamma_p-aMRS), (weibull_p-aMRS), (apMOS-aMRS), (hmean_p_sz-aMRS), (lnorm_p_sz-aMRS), (gamma_p_sz-aMRS), (weibull_p_sz-aMRS), (apMOS_sz-aMRS)),
                          method = c(rep("hmean", times=length(aMRS)), rep("lnorm", times=length(aMRS)), rep("gamma", times=length(aMRS)), rep("weibull", times=length(aMRS)), rep("arithmetic", times=length(aMRS)), rep("hmean", times=length(aMRS)), rep("lnorm", times=length(aMRS)), rep("gamma", times=length(aMRS)), rep("weibull", times=length(aMRS)), rep("arithmetic", times=length(aMRS))),
                          type = c(rep("raw", times=length(aMRS)*5), rep("with_sz", times=length(aMRS)*5)))
  
  p_to_p_plot <- ggplot(p_to_p_df, aes(x = aMRS, y = error, colour = method))+
    geom_point()+
    facet_grid(type ~ .)+
    geom_smooth(alpha = 0.2, se=F)+
    theme_minimal()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    scale_colour_manual(values = c("#FF0000", "#00FF00", "#00FFFF", "#FF00FF", "#0000FF"))+
    labs(x = "Mean realised speed (m/s)",
         y = "Error (m/s)",
         title = "Errors between MRS and estimated speeds\n(+ve: est > MRS)")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          title = element_text(size = 13),
          strip.text = element_text(size = 13),
          legend.position = "bottom")
  p_to_p_plot
  
  
  
  # save together
  
  if (part_of_wedge==0){
    wedgename <- "whole"
  }
  if (part_of_wedge==1){
    wedgename <- "BOTTOM THIRD"
  }
  if (part_of_wedge==2){
    wedgename <- "MIDDLE THIRD"
  }
  if (part_of_wedge==3){
    wedgename <- "TOP THIRD"
  }
  
  mp_arranged <- ggarrange(mrs_mos_m_plot, mrs_mos_p_plot, mrs_est_m_plot, mrs_est_p_plot, nrow = 2, ncol = 2)
  mp_annotated <- annotate_figure(mp_arranged, top = text_grob(paste0("PART OF WEDGE = ", wedgename, "\nLEFT: mean of means; RIGHT: point-to-point"),
                                                               color = "red", face = "bold", size = 14))
  
  png(file=paste0(parentfolder, "plots/wedge", part_of_wedge, "/mp_Mb", Mb_range[1], "-", Mb_range[length(Mb_range)], "_iters1-", Mb_iters$iter[1], ".png"),
      width=1000, height=1000)
  print(mp_annotated)
  dev.off()
  
  
  
  
  ## save new combined plots:
  
  comb_mp_arranged <- ggarrange(m_of_m_plot, p_to_p_plot, nrow = 1, ncol = 2)
  comb_mp_annotated <- annotate_figure(comb_mp_arranged, top = text_grob(paste0("PART OF WEDGE = ", wedgename, "\nLEFT: mean of means; RIGHT: point-to-point"),
                                                                         color = "red", face = "bold", size = 14))
  
  png(file=paste0(parentfolder, "plots/wedge", part_of_wedge, "/comb_mp_Mb", Mb_range[1], "-", Mb_range[length(Mb_range)], "_iters1-", Mb_iters$iter[1], ".png"),
      width=1000, height=800)
  print(comb_mp_annotated)
  dev.off()
  
  
  # SINGLES, ZEROS & DETECTED POINTS
  
  # for the whole wedge
  
  
  
  
  # for different chunks of the wedge
  
  # MAKING MAIN_DF_SEPARATE_WEDGES --> WILL NEED TO RE-DO THIS WHEN GENERATE MORE PLOTTING VARIABLES FOR MORE ITERATIONS
  # wedge1_df <- read.csv("../Mb_results/08Jul22_1602/plotting_data/wedge1/main_df_wedge1.csv")
  # wedge2_df <- read.csv("../Mb_results/08Jul22_1602/plotting_data/wedge2/main_df_wedge2.csv")
  # wedge3_df <- read.csv("../Mb_results/08Jul22_1602/plotting_data/wedge3/main_df_wedge3.csv")
  # 
  # main_df_separate_wedges <- rbind(wedge1_df, wedge2_df, wedge3_df)
  # main_df_separate_wedges["percent_det"] <- ((main_df_separate_wedges$n_detected/main_df_separate_wedges$n_points)*100) # % of all points falling into the dz that get detected
  # main_df_separate_wedges["percent_singles"] <- ((main_df_separate_wedges$n_singles/main_df_separate_wedges$n_detected)*100) # % of detected points that are singles
  # main_df_separate_wedges["ratio_zeros"] <- main_df_separate_wedges$n_zeros/main_df_separate_wedges$n_detected # ratio of number of zeros to number of detected points
  # 
  # write.csv(main_df_separate_wedges, "../Mb_results/08Jul22_1602/plotting_data/main_df_separate_wedges.csv")
  
  
  # read in main_df_separate_wedges made so far
  main_df_separate_wedges <- read.csv("../Mb_results/08Jul22_1602/plotting_data/main_df_separate_wedges.csv")
  
  # % of points that get detected - for separate wedges
  det_percent_plot_separate_wedges <- ggplot(main_df_separate_wedges, aes(x=aMRS, y=percent_det, colour = wedge))+
    geom_point()+
    geom_smooth(alpha=0.3, se=F)+
    labs(x = "Mean realised speed (m/s)",
         y = "%",
         title = "% of all points falling in\nthe dz that get detected")+
    theme_minimal()+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          title = element_text(size = 22),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.position="bottom")
  det_percent_plot_separate_wedges
  
  # % of detected points that are singles - for separate wedges
  singles_percent_plot_separate_wedges <- ggplot(main_df_separate_wedges, aes(x=aMRS, y=percent_singles, colour = wedge))+
    geom_point()+
    geom_smooth(alpha=0.3, se=F)+
    labs(x = "Mean realised speed (m/s)",
         y = "%",
         title = "% of detected points\nthat are single frames")+
    theme_minimal()+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          title = element_text(size = 22),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.position="bottom")
  singles_percent_plot_separate_wedges
  
  
  # ratio of zeros to detected points - for separate wedges
  zeros_ratio_plot_separate_wedges <- ggplot(main_df_separate_wedges, aes(x=aMRS, y=ratio_zeros, colour = wedge))+
    geom_point()+
    geom_smooth(alpha=0.3, se=F)+
    labs(x = "Mean realised speed (m/s)",
         y = "ratio",
         title = "Ratio of zero frames\nto detected points")+
    theme_minimal()+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size = 15),
          title = element_text(size = 22),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.position = "bottom")
  zeros_ratio_plot_separate_wedges
  
  
  # save each one separately
  png(file=paste0(parentfolder, "plots/detected_separate_wedges_Mb", Mb_range[1], "-", Mb_range[length(Mb_range)], "_iters1-", Mb_iters$iter[1], ".png"),
      width=550, height=550)
  print(det_percent_plot_separate_wedges)
  dev.off()
  
  png(file=paste0(parentfolder, "plots/singles_separate_wedges_Mb", Mb_range[1], "-", Mb_range[length(Mb_range)], "_iters1-", Mb_iters$iter[1], ".png"),
      width=550, height=550)
  print(singles_percent_plot_separate_wedges)
  dev.off()
  
  png(file=paste0(parentfolder, "plots/zeros_separate_wedges_Mb", Mb_range[1], "-", Mb_range[length(Mb_range)], "_iters1-", Mb_iters$iter[1], ".png"),
      width=550, height=550)
  print(zeros_ratio_plot_separate_wedges)
  dev.off()
  
  
  
  ### TO DO: MAYBE MAKE THE SAME 3 PLOTS BUT WITH DATA FROM JUST THE WHOLE WEDGE? - NEED TO RE-RUN TO GET THIS THOUGH BC GENERATED THOSE RESULTS BEFORE CHANGING THE CODE TO SAVE NO. OF POINTS & DETECTED POINTS
  