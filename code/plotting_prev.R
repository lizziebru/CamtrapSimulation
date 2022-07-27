# PLOTTING FROM PREVIOUS RESULTS ##
# results that are now in prev_results folder
# keeping this for now bc useful for generating final set of plots



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
