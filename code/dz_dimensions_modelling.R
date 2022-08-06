## DISTANCE FROM CT PROBABILITY DENSITY OF BEING DETECTED ##

# TO DO:
# go through all the data csvs and check which ones you actually need vs not
# go through this script and the path modelling one to tidy up and add things to discussion section if needed, and also any packages that need to go in README



# all the modelling done on detection probability stuff

require(stats4)
require(minpack.lm)
require(ggplot2)

# combined dataset:
data_all_cats <- read.csv("../data/data_all_cats.csv")

# panama-only dataset with speeds, distances & avg duration
panama <- read.csv("../data/panama/panama_full.csv")

# fitting hz rate only with and without scaling of effective detection distance  -------------------------------------

## fit hz only using all body masses:
# function:
# 1 - exp(-(a/dist)^g)
# where a (alpha) describes width and g (gamma) describes shape
l_r_dens_approxfun <- approxfun(density(panama$distance)) 
l_r_density <- l_r_dens_approxfun(panama$distance) # == y values
l_r <- panama$distance
# fit the model and plot the fit
hz_l_r <- nlsLM(l_r_density ~ (1 - exp(-(a/l_r)^g)), start = list(a = 1, g = 1))
hz_l_r_plot <- ggplot()+ 
  geom_point(aes(x=panama$distance, y=l_r_density))+
  geom_point(aes(x=panama$distance, y=predict(hz_l_r)), colour = "red")+
  theme_minimal()+
  labs(title = "hazard rate - all body sizes",
       x = "radius (m)",
       y = "density")+
  theme(axis.title = element_text(size = 18),
        title = element_text(size = 20),
        axis.text = element_text(size = 15))
hz_l_r_plot
coef(hz_l_r)
# coefficients:
# a = 0.4255583
# g = 0.7369158
# to use in pathgen: 1 - exp(-(0.4255583/radius)^0.7369158)


# fit for diff body masses to get scaling relationship for each coefficient:
Mb <- c()
a_coeffs <- c()
g_coeffs <- c()
for (i in unique(panama$Body_mass_g)){ # loop through each body mass
  Mb <- c(Mb, i/1000) # store body mass in kg
  l_r_dens_approxfun <- approxfun(density(panama[panama$Body_mass_g==i,]$distance)) 
  l_r_density <- l_r_dens_approxfun(panama[panama$Body_mass_g==i,]$distance) # == y values
  l_r <- panama[panama$Body_mass_g==i,]$distance
  # fit the model and plot the fit
  hz_l_r <- nlsLM(l_r_density ~ (1 - exp(-(a/l_r)^g)), start = list(a = 1, g = 1))
  a_coeffs <- c(a_coeffs, coef(hz_l_r)[[1]])
  g_coeffs <- c(g_coeffs, coef(hz_l_r)[[2]])
}

coeffs_scaling_df <- data.frame(Mb = Mb, a = a_coeffs, g = g_coeffs)

# get scaling relationships for each coefficient:

# from path_modelling:
# get relationship between body mass (kg) and mean_v (m/s) for each behaviour:

# need this on a log scale:
Mb_a_scaling <- ggplot(coeffs_scaling_df, aes(x = log(Mb), y = log(a)))+
  geom_point()
Mb_a_scaling
# very strong -ve relationship -- drops to v low from around 5kg

Mb_g_scaling <- ggplot(coeffs_scaling_df, aes(x = log(Mb), y = log(g)))+
  geom_point()
Mb_g_scaling
# similar as for a

# fit regression model:
Mb_a_lm <- lm(log(a) ~ log(Mb), data = coeffs_scaling_df)
summary(Mb_a_lm)
cfs_a <- coef(Mb_a_lm)
exp(cfs_a[1]) # intercept for a = 0.339065 
exp(cfs_a[2]) # slope for a = 0.5214121

Mb_g_lm <- lm(log(g) ~ log(Mb), data = coeffs_scaling_df)
summary(Mb_g_lm)
cfs_g <- coef(Mb_g_lm)
exp(cfs_g[1]) # intercept for g = 1.505105 
exp(cfs_g[2]) # slope for g = 0.5784485 


# c = 10^intercept
# b = slope

# power law eqns to use:
# a:
# a = 2.183057*(Mb^0.5214121)
# g:
# g = 31.99669*(Mb^0.5784485)





# fitting half normal with scaling of eff det angle -----------------------

p_angles <- read.csv("../data/panama/panama_with_angles.csv")

# from Rowcliffe et al. 2011: half-normal model with 2 cosine expansion terms fitted best, and suggested consistent across species - half normal worked fine too though so just to keep things simple use half normal
# half-normal model:
# a(y) = exp((-(y^2))/2*a^2)
# a = alpha = width of function

# fit half-normal model for diff body masses to get scaling relationship for each coefficient:
Mb <- c()
a_coeffs <- c()
p_angles2 <- p_angles[!p_angles$species=="tamandua",] # struggling to fit model for tamandua - just skip that one
for (i in unique(p_angles2$Body_mass_g)){ # loop through each body mass
  Mb <- c(Mb, i/1000) # store body mass in kg
  l_r_dens_approxfun <- approxfun(density(p_angles2[p_angles2$Body_mass_g==i,]$angles_rad)) 
  l_r_density <- l_r_dens_approxfun(p_angles2[p_angles2$Body_mass_g==i,]$angles_rad) # == y values
  l_r <- p_angles2[p_angles2$Body_mass_g==i,]$angles_rad
  # fit the model
  model_fit <- nlsLM(l_r_density ~ exp((-(l_r^2))/2*a^2), start = list(a = 10))
  a_coeffs <- c(a_coeffs, coef(model_fit)[[1]])
}
coeffs_scaling_df <- data.frame(Mb = Mb, a = a_coeffs)



# plot the fit for one of them: rat: 
l_r_dens_approxfun <- approxfun(density(p_angles2[p_angles2$Body_mass_g==282.17,]$angles_rad)) 
l_r_density <- l_r_dens_approxfun(p_angles2[p_angles2$Body_mass_g==282.17,]$angles_rad) # == y values
l_r <- p_angles2[p_angles2$Body_mass_g==282.17,]$angles_rad
# fit the model and plot the fit
hz_l_r <- nlsLM(l_r_density ~ exp((-(l_r^2))/2*a^2), start = list(a = 10))
hz_l_r_plot <- ggplot()+ 
  geom_point(aes(x=p_angles2[p_angles2$Body_mass_g==282.17,]$angles_rad, y=l_r_density))+
  geom_point(aes(x=p_angles2[p_angles2$Body_mass_g==282.17,]$angles_rad, y=predict(hz_l_r)), colour = "red")+
  theme_minimal()+
  labs(title = "half normal for angle for rats",
       x = "angle (rad)",
       y = "density")+
  theme(axis.title = element_text(size = 18),
        title = element_text(size = 20),
        axis.text = element_text(size = 15))
hz_l_r_plot
coef(hz_l_r)
# not a very good fit - maybe best not to do it this way then

# instead: just scale the angle of the fixed wedge-shaped detection zone with body mass 
# justify bc there wasn't that much of a marked decrease in detection with angle - esp given more recent data that M was talking about
# but makes sense that there is just a small amount of change in angle depending on body mass 
# so just simplify this to setting a fixed angle for a given body mass

# use the effective detection angles from Rowcliffe et al. 2011:
mb_effdetang <- data.frame(species = c("mouse", "rat", "squirrel", "agouti", "coati", "paca", "ocelot", "brocket", "peccary"),
                           angle = c(15.9, 16.5, 15.6, 17.6, 18.9, 17.5, 16, 22.2, 25.8),
                           Mb = c(19.3, 282.17, 545.4, 2309.12, 3775.5, 8172.55, 11880, 20546.86, 21133.69)/1000)


# get scaling relationships for effective detection angle:

# need this on a log scale:
Mb_a_scaling <- ggplot(mb_effdetang, aes(x = log(Mb), y = log(angle)))+
  geom_point()
Mb_a_scaling

# fit regression model:
Mb_a_lm <- lm(log(angle) ~ log(Mb), data = mb_effdetang)
summary(Mb_a_lm)
cfs_a <- coef(Mb_a_lm)
exp(cfs_a[1]) # intercept = 12.69519 
exp(cfs_a[2]) # slope = 1.047932  


# c = 10^intercept
# b = slope
# power law eqn: y = cx^b

# power law eqn to use:
# a = 4.95667e+12*(Mb^1.047932)

# --> looks really off for some reason - ran out of time so don't bother

# can justify this bc body mass doesn't affect angle that much - but could just be nice to add it in later 




# panama angle data wrangling - to make panama_with_angles.csv ---------------------------------------------

# with_angles <- read.csv("../data/panama/with_angles_raw.csv")
# 
# p_angles <- data.frame(species=as.character(with_angles$SPECIES_NAME_COMMON),
#                        angle=with_angles$ANIMAL_MEASURE_ANGLE_FIRST_DETECTION,
#                        dist= with_angles$ANIMAL_MEASURE_DISTANCE_FIRST_DETECTION)
# 
# # select only the same species for which used distance data
# p_angles <- p_angles[p_angles$species %in% panama$species,]
# 
# # add body masses:
# body_masses <- c()
# for (i in 1:nrow(p_angles)){
#   p <- p_angles[i,]
#   if (p$species=="agouti"){
#     body_masses <- c(body_masses, 2309.12)
#   }
#   if (p$species=="rat"){
#     body_masses <- c(body_masses, 282.17)
#   }
#   if (p$species=="paca"){
#     body_masses <- c(body_masses, 8172.55)
#   }
#   if (p$species=="brocket"){
#     body_masses <- c(body_masses, 20546.86)
#   }
#   if (p$species=="coati"){
#     body_masses <- c(body_masses, 3775.50)
#   }
#   if (p$species=="squirrel"){
#     body_masses <- c(body_masses, 545.40)
#   }
#   if (p$species=="ocelot"){
#     body_masses <- c(body_masses, 11880.00)
#   }
#   if (p$species=="tamandua"){
#     body_masses <- c(body_masses, 4800.00)
#   }
#   if (p$species=="peccary"){
#     body_masses <- c(body_masses, 21133.69)
#   }
#   if (p$species=="mouse"){
#     body_masses <- c(body_masses, 19.30)
#   }
#   if (p$species=="armadillo"){
#     body_masses <- c(body_masses, 3949.01)
#   }
#   if (p$species=="opossum"){
#     body_masses <- c(body_masses, 1134.75)
#   }
# }
# 
# p_angles["Body_mass_g"] <- body_masses
# 
# # remove rows containing NAs:
# p_angles <- na.omit(p_angles)
# 
# # make negative angles positive (bc the sign just indicates direction) and put angles in radians
# new_angles <- c()
# for (i in 1:nrow(p_angles)){
#   p <- p_angles[i,]
#   angle <- p$angle
#   if (angle < 0){ # make any negatives positive
#     angle <- -angle
#   }
#   angle <- angle*(pi/180) # to convert to radians, multiply by pi/180
#   new_angles <- c(new_angles, angle)
# }
# p_angles["angles_rad"] <- new_angles
# 
# # discard any rows where angle is greater than 45 degrees - anomalous
# p_angles <- p_angles[p_angles$angle < 45,]
# p_angles <- p_angles[p_angles$angle > -45,]
# 
# write.csv(p_angles, "../data/panama/panama_with_angles.csv")



# fitting hazard rate (+ log mix for small spp) functions for large vs small categories --------

# different parameters for large vs small, and added logistic mix for small
# large: over 4kg
# small: under 4kg

# use hazard rate bc Rowcliffe et al. 2011 found its underlying form seems consistent across diff species & body masses
# they also found adding logistic mix is good for species below 4kg

# so need to parameterize hazard rate function using data now:

# use only Panama bc shouldn't combine data collected using diff methodologies - bc dz is also affected by habitat etc

# only do radius (not angle bc Rowcliffe et al. 2011 found it doesn't vary significantly with body mass)

# would be overkill to scale each of the inidividual parameters of these functions with body mass, especially since Rowcliffe et al. 2011 showed the general shape is pretty consistent across species

# so just need to get set of coefficients for large vs small 


## large: hazard rate:

# function:
# 1 - exp(-(a/dist)^g)
# where a (alpha) describes width and g (gamma) describes shape

l_r_dens_approxfun <- approxfun(density(panama[panama$size=="large",]$distance)) 
l_r_density <- l_r_dens_approxfun(panama[panama$size=="large",]$distance) # == y values
l_r <- panama[panama$size=="large",]$distance
# fit the model and plot the fit
hz_l_r <- nlsLM(l_r_density ~ (1 - exp(-(a/l_r)^g)), start = list(a = 1, g = 1))
hz_l_r_plot <- ggplot()+ 
  geom_point(aes(x=panama[panama$size=="large",]$distance, y=l_r_density))+
  geom_point(aes(x=panama[panama$size=="large",]$distance, y=predict(hz_l_r)), colour = "red")+
  theme_minimal()+
  labs(title = "Large - distance - hazard rate",
       x = "radius (m)",
       y = "density")+
  theme(axis.title = element_text(size = 18),
        title = element_text(size = 20),
        axis.text = element_text(size = 15))
hz_l_r_plot
coef(hz_l_r)
# coefficients:
# a = 0.1219941
# g = 0.4477139


## small: hazard rate + logistic mix:
# (1 - exp(-(a/dist)^g))/(1 + exp(b*(e - dist)))
# where b (beta) describes rate of increase and e (epsilon) describes inflection point

s_r_dens_approxfun <- approxfun(density(panama[panama$size=="small",]$distance)) 
s_r_density <- s_r_dens_approxfun(panama[panama$size=="small",]$distance) # == y
s_r <- panama[panama$size=="small",]$distance
# fit the model and plot the fit
hzlog_s_r <- nlsLM(s_r_density ~ (1 - exp(-(a/s_r)^g))/(1 + exp(b*(e - s_r))), start = list(a = 1, g = 1, b = 1, e = 1))
hzlog_s_r_plot <- ggplot()+ 
  geom_point(aes(x=panama[panama$size=="small",]$distance, y=s_r_density))+
  geom_point(aes(x=panama[panama$size=="small",]$distance, y=predict(hzlog_s_r)), colour = "red")+
  theme_minimal()+
  labs(title = "Small - radius - logistic mix hazard rate",
       x = "radius (m)",
       y = "density")+
  theme(axis.title = element_text(size = 18),
        title = element_text(size = 20),
        axis.text = element_text(size = 15))
hzlog_s_r_plot
coef(hzlog_s_r)
# coefficients:
# a = 1.13331139
# g = 2.61961545
# b = -0.03641968
# e = 26.76458146 

# to use in simulation:
# small:
# (1 - exp(-(1.13331139/radius)^2.61961545))/(1 + exp(-0.03641968*(26.76458146 - radius))
# large:
# 1 - exp(-(0.1219941/radius)^0.4477139)


## need to scale detection probability density so that max probability = 1

# currently max detection probability = max density in the density curves from distribution of radii in real datas

# find the max and divide 1 by it to work out how much need to scale everything by

# model for large species' radius: hazard rate with logistic mix
large_radius <- function(radius){
  (1 - exp(-(0.1219941/radius)^0.4477139))
}

small_radius <- function(radius){
  (1 - exp(-(1.13331139/radius)^2.61961545))/(1 + exp(-0.03641968*(26.76458146 - radius)))
}

x <- seq(0,10, length = 1000)
y_large <- sapply(x, hz_radius, Mb=5, scaling=TRUE)
y_small <- sapply(x, hz_radius, Mb=1, scaling=TRUE)

ggplot()+
  geom_smooth(aes(x = x, y = y_large, colour = "large"))+
  geom_smooth(aes(x = x, y = y_small, colour = "small"))

max(y_large) # max for large == 1 so don't need to multiply by anything
max(y_small) # max for small == 0.7260668 --> so need to multiply everything by 1.377284















# PDFs from regents' park data only ------------------------------------------------------


# PDF worked out using fox & hedgehog data combined:

dist_prob <- function(x){ # == the probability density function estimated from fox & hedgehog data combined
  dnorm(x, mean = 0.9976297, sd = 0.5452, log = F) # log mean & sd from both species
}

# just foxes:
dist_prob_f <- function(x){ # == the probability density function estimated from fox & hedgehog data combined
  dnorm(x, mean = 1.029835, sd = 0.5263222, log = F) # log mean & sd from foxes
}

# just hedgehogs:
dist_prob_h <- function(x){ # == the probability density function estimated from fox & hedgehog data combined
  dnorm(x, mean = 0.8983482, sd = 0.5891332, log = F) # log mean & sd from foxes
}




# PDFs using Panama data only ---------------------------------------------

panama_06 <- read.table("data/rse217-sup-0006-speeddatapublished.txt")

# add a speed column: 

# speed = d / (i*t)

panama_06$speed <- panama_06$distance / (panama_06$intervals * panama_06$avgduration)





# Using methods from Rowcliffe et al. 2011 --------------------------------

# they've done pretty much exactly what I did - need to change my methods to be more like theirs'

# they fit 4 models:
# - half normal
# - hazard rate
# - half normal with logistic mix
# - hazard rate with logistic mix

## for the time being just fit the hazard rate model (models a slightly longer flat section at the start before tailing off vs the half normal starts decreasing immediately)
# -- then could see if half normal is any better - but probs not for now bc it's a bit trivial and their paper did show hazard rate had a generally slightly better fit
# -- when simulate diff sized spp etc could then consider switching to logistic mix


library(bbmle)	#for mle2
library(MASS)	#for mvrnorm (multivariate random normal numbers, for EDD conf int)

######################################
# Standard detection model functions #
######################################

#Cosine expansion terms for detection function
#y: data vector
#cprm: coefficients
expansion <- function(y, cprm, maxy)
{	y <- y/maxy
n <- length(y)
expn <- length(cprm)
m <- cos(pi*rep(1:expn,each=n)*rep(y,expn))
m <- matrix(m*rep(cprm,each=n), nrow=n)
apply(m,1,sum)
}

keyfunc <- function(y, prm, shape)
{	switch(shape, 
         exp = exp(-y/prm[1]),
         nor = exp(-y^2/(2*prm[1]^2)),
         haz = 1-exp(-(y/prm[1])^-prm[2]),
         tno = ifelse(y<prm[2], 1, exp(-(y-prm[2])^2/(2*prm[1]^2))) )
}

logitfunc <- function(y, prm){
  1/(1 + exp(prm[2]*(prm[1]-y)))
}


#Detection function
#y: data vector
#prm: vector of parameters. Must contain at least the first of these components, in this order:
#	1. s = function width parameter, either normal or hazard
#	2. b = hazard slope parameter
#	3. s0 = position of near-camera detection threshold
#	4. b0 = slope of near-camera detection increase
#	Vector length determines which model is used.
#type: form of detection function, either point or line
DF <- function(y,prm,type,monotonic,shape,maxy=max(y))
{	if(!type %in% c("point","line")) stop(paste("Model type",type,"not recognised"))
  if(!monotonic %in% c("mon","inc")) stop(paste("Model monotonic flag",monotonic,"not recognised"))
  if(!shape %in% c("nor","exp","haz","tno")) stop(paste("Model shape",shape,"not recognised"))
  
  nprm <- length(prm)
  nkeyprm <- switch(shape,exp=1,nor=1,haz=2,tno=2) + switch(monotonic,mon=0,inc=2)
  nexprm <- nprm-nkeyprm
  prm[1:nkeyprm] <- exp(ifelse(prm[1:nkeyprm]>100, 100, prm[1:nkeyprm]))
  key <- keyfunc(y,prm,shape)
  if(nexprm>0)
  {	key <- key * (1+expansion(y,prm[nprm-(nexprm:1)+1],maxy))
  key[key<0] <- 0
  key <- key / (keyfunc(0,prm,shape) * (1+expansion(0,prm[nprm-(nexprm:1)+1],maxy)))
  }
  if(monotonic=="inc") key <- key * logitfunc(y,prm[((nprm-1):nprm)-nexprm])
  if(type=="point") key <- key*y
  key
}

#Finds the peak probability of detection for fitted model object mod
DFPeak <- function(mod)
{	coefs <- mod$model@coef
maxy <- max(mod$model@data$y)
optimise(DF, c(0,maxy), maximum=T, prm=coefs, maxy=maxy,
         "line", monotonic=mod$monotonic, shape=mod$shape)$objective
}


#Probability density function
#log: defines whether to return log lik
PDF <- function(y,prm,type,monotonic,shape,maxy=max(y),log=TRUE)
{	intgrl <- integrate(DF,0,maxy,prm=prm,type=type,monotonic=monotonic,shape=shape,
                      maxy, rel.tol=1e-07, stop.on.error=F)$value
res <- DF(y,prm,type,monotonic,shape,maxy) / intgrl
if(log)
{	res <- log(res)
res[is.infinite(res)] <- -999
}
res
}

#Neg log-likelihood functions
PtMoExNLL <- function(prm,y) -sum(PDF(y,prm,"point","mon","exp"))
PtMoNoNLL <- function(prm,y) -sum(PDF(y,prm,"point","mon","nor"))
PtMoHaNLL <- function(prm,y) -sum(PDF(y,prm,"point","mon","haz"))
PtMoTnNLL <- function(prm,y) -sum(PDF(y,prm,"point","mon","tno"))

#PtIn functions include penalty term constraining s0 close to min(y)
PtInExNLL <- function(prm,y) -sum(PDF(y,prm,"point","inc","exp")) + 1e8*abs(log(min(y)/exp(prm[2])))^10
PtInNoNLL <- function(prm,y) -sum(PDF(y,prm,"point","inc","nor")) + 1e8*abs(log(min(y)/exp(prm[2])))^10
PtInHaNLL <- function(prm,y) -sum(PDF(y,prm,"point","inc","haz")) + 1e8*abs(log(min(y)/exp(prm[3])))^10
PtInTnNLL <- function(prm,y) -sum(PDF(y,prm,"point","inc","tno")) + 1e8*abs(log(min(y)/exp(prm[3])))^10

LnMoExNLL <- function(prm,y) -sum(PDF(y,prm,"line","mon","exp"))
LnMoNoNLL <- function(prm,y) -sum(PDF(y,prm,"line","mon","nor"))
LnMoHaNLL <- function(prm,y) -sum(PDF(y,prm,"line","mon","haz"))
LnMoTnNLL <- function(prm,y) -sum(PDF(y,prm,"line","mon","tno"))

LnInExNLL <- function(prm,y) -sum(PDF(y,prm,"line","inc","exp"))
LnInNoNLL <- function(prm,y) -sum(PDF(y,prm,"line","inc","nor"))
LnInHaNLL <- function(prm,y) -sum(PDF(y,prm,"line","inc","haz"))
LnInTnNLL <- function(prm,y) -sum(PDF(y,prm,"line","inc","tno"))

#Penalised Neg log-likelihood function
#For non-monotonic hazard model with constrained peak detection probability 
PNLL <- function(prm,y) 
{	PkHt <- optimise(DF, c(0,max(y)), maximum=T, prm=prm[1:4], "line","inc","haz")$objective
pen <- 10000*abs(prm[5]-PkHt)^2
if(prm[2]<0) pen <- pen + 1000*abs(prm[2])^5
pen <- pen + 1e8*abs(log(min(y)/exp(prm[3])))^10
if(prm[4]<0) pen <- pen + 1000*abs(prm[4])^2
-sum(PDF(y,prm[1:4],"point","inc","haz")) + pen
}

#Fit detection function to data y, given named vector of starting parameters strt
fitDF <- function(y, type, shape, monotonic, expn=0, plotres=FALSE, title=NULL, fixPk=NULL, fixed=NULL, strt=NULL)
{	if(!type %in% c("point","line")) stop(paste("Model type",type,"not recognised"))
  if(!monotonic %in% c("mon","inc")) stop(paste("Model monotonic flag",monotonic,"not recognised"))
  if(!shape %in% c("nor","exp","haz","tno")) stop(paste("Model shape",shape,"not recognised"))
  
  if(is.null(strt))
  {	strt <- c(s=log(switch(type, line=max(y), point=mean(y))))
  if(shape=="haz") strt <- c(strt, b=1)
  if(monotonic=="inc") strt <- c(strt, s0=log(min(y)), b0=log(10))
  if(expn)
  {	if(expn==1) strt <- c(strt, 0) else strt <- c(strt, -(-1)^(1:expn))
  i <- length(strt)-(expn:1)+1
  nms <- paste(rep("c",expn),1:expn,sep="")
  names(strt)[i] <- nms
  }
  }
  if(type=="line")
  {	if(monotonic=="mon")
  {	if(shape=="exp")
  {	parnames(LnMoExNLL) <- names(strt)
  res <- mle2(LnMoExNLL, start=strt, data=list(y=y))
  } else
    if(shape=="nor")
    {	parnames(LnMoNoNLL) <- names(strt)
    res <- mle2(LnMoNoNLL, start=strt, data=list(y=y))
    } else
      if(shape=="haz")
      {	parnames(LnMoHaNLL) <- names(strt)
      res <- mle2(LnMoHaNLL, start=strt, data=list(y=y))
      } else
        if(shape=="tno")
        {	parnames(LnMoTnNLL) <- names(strt)
        res <- mle2(LnMoTnNLL, start=strt, data=list(y=y))
        } else stop("Shape not recognised")
  } else
    if(monotonic=="inc")
    {	if(shape=="exp")
    {	parnames(LnInExNLL) <- names(strt)
    res <- mle2(LnInExNLL, start=strt, data=list(y=y))
    } else
      if(shape=="nor")
      {	parnames(LnInNoNLL) <- names(strt)
      res <- mle2(LnInNoNLL, start=strt, data=list(y=y))
      } else
        if(shape=="haz")
        {	parnames(LnInHaNLL) <- names(strt)
        res <- mle2(LnInHaNLL, start=strt, data=list(y=y))
        } else
          if(shape=="tno")
          {	parnames(LnInTnNLL) <- names(strt)
          res <- mle2(LnInTnNLL, start=strt, data=list(y=y))
          } else stop("Shape no recognised")
    } else stop("Monotonic flag not recognised")
  } else 
    if(type=="point")
    {	if(is.null(fixPk))
    {	if(monotonic=="mon")
    {	if(shape=="exp")
    {	parnames(PtMoExNLL) <- names(strt)
    res <- mle2(PtMoExNLL, start=strt, data=list(y=y))
    } else
      if(shape=="nor")
      {	parnames(PtMoNoNLL) <- names(strt)
      res <- mle2(PtMoNoNLL, start=strt, data=list(y=y))
      } else
        if(shape=="haz")
        {	parnames(PtMoHaNLL) <- names(strt)
        res <- mle2(PtMoHaNLL, start=strt, data=list(y=y))
        } else
          if(shape=="tno")
          {	parnames(PtMoTnNLL) <- names(strt)
          res <- mle2(PtMoTnNLL, start=strt, data=list(y=y))
          } else stop("Shape no recognised")
    } else
      if(monotonic=="inc")
      {	if(shape=="exp")
      {	parnames(PtInExNLL) <- names(strt)
      res <- mle2(PtInExNLL, start=strt, data=list(y=y))
      } else
        if(shape=="nor")
        {	parnames(PtInNoNLL) <- names(strt)
        res <- mle2(PtInNoNLL, start=strt, data=list(y=y))
        } else
          if(shape=="haz")
          {	parnames(PtInHaNLL) <- names(strt)
          if(!is.null(fixed)) 
            strt <- strt[-which(names(strt)==names(fixed))]
          res <- mle2(PtInHaNLL, start=strt, fixed=fixed, data=list(y=y))
          } else
            if(shape=="tno")
            {	parnames(PtInTnNLL) <- names(strt)
            if(!is.null(fixed)) 
              strt <- strt[-which(names(strt)==names(fixed))]
            res <- mle2(PtInTnNLL, start=strt, fixed=fixed, data=list(y=y))
            } else stop("Shape not recognised")
      } else stop("Monotonic flag not recognised")
    } else
    {	parnames(PNLL) <- c(names(strt), "PH")
    res <- mle2(PNLL, start=strt, fixed=c(PH=fixPk), data=list(y=y)) 
    }
    } else stop("Type not recognised")
  
  res <- list(model=res, type=type, monotonic=monotonic, shape=shape)
  if(plotres) plotmod(res,title=title)
  res
}

#Constrained hazard model for distance (point) data
#First fits a normal non-monotonic model, then fits a hazard with peak detection 
#probability constrained
fitDF.conhaz <- function(y, plotres=FALSE, title=NULL)
{	nmod <- fitDF(y, "point", "nor", "inc")
PkHt <- DFPeak(nmod)
names(PkHt) <- NULL
PH <- 1/(1+1/exp(15*PkHt-6.7)) ##-- this looks like the log model describing lower probability of being detected when too close to CT
res <- fitDF(y, "point", "haz", "inc", fixPk=PH)
if(plotres) plotmod(res,resln=20,title=title)
res
}

fitAll <- function(i,y)
{	if(i==1) fitDF(y) else
  if(i==2) fitDF(y,monotonic=F) else
    if(i==3) fitDF(y,"hazard") else
      if(i==4) fitDF(y,"hazard",monotonic=F) else
        stop("invalid input value")
}
fitBest <-function(y)
{	res <- sapply(1:4,fitAll,y)
AIC <- unlist(lapply(res,AIC))
Shape <- rep(c("normal","hazard"),each=2)
Monotonic <- rep(c("yes","no"),2)
print(data.frame(Shape, Monotonic, AIC))
res[[which(AIC==min(AIC))]]
}

fitIter <- function(i,y,strata)
{	snames=levels(strata)
mod <- fitBest(y[strata==snames[i]])
negLL <- mod@min
AIC <- AIC(mod)
coefs <- coef(mod)
if(length(coefs)==2) 
{	coefs <- c(coefs,NA,NA)
names(coefs)[3:4] <- c("s0","b0")
}
EDD <- EDDfunc(mod)
res <- c(negLL,AIC,coefs,EDD)
names(res)[1:2] <- c("negLL","AIC")
res
}
fitStrat <- function(y,strata)
{	snames <- levels(strata)
res <- sapply(1:length(snames),fitIter,y=y,strata=strata)
dimnames(res)[[2]] <- levels(strata)
list(StratRes=res, TotalAIC=sum(res[2,]))
}

#Chi-squared goodness of fit test for a model
GOF <- function(mod,brks=NULL)
{	y <- mod$model@data$y
n <- length(y)
if(is.null(brks)) hh <- hist(y,plot=F) else
  hh <- hist(y,breaks=brks,plot=F)
Obs <- hh$counts
Upr <- hh$breaks[-1]
Upr[length(Upr)] <- max(y)
Lwr <- hh$breaks[-length(hh$breaks)]
Lwr[1] <- 0
Exp <- sapply(hh$breaks, 
              function(x) integrate(PDF,0,x, prm=mod$model@coef, type=mod$type,
                                    monotonic=mod$monotonic, shape=mod$shape, maxy=max(y), log=F)$value)
Exp[c(1,length(Exp))] <- 0:1
Exp <- n*(Exp[-1] - Exp[-length(Exp)])
bins <- length(Exp)
if(min(Exp)<5)
{	for(i in bins:2)
{	if(Exp[i]<5)
{	bins <- bins-1
Exp[i-1] <- sum(Exp[(i-1):i])
Obs[i-1] <- sum(Obs[(i-1):i])
Exp <- Exp[-i]
Obs <- Obs[-i]
Upr <- Upr[-(i-1)]
Lwr <- Lwr[-i]
}
}
  if(Exp[1]<5) 
  {	bins <- bins-1
  Exp[2] <- sum(Exp[1:2])
  Obs[2] <- sum(Obs[1:2])
  Exp <- Exp[-1]
  Obs <- Obs[-1]
  Upr <- Upr[-1]
  Lwr <- Lwr[-2]
  }
}
Chisq <- sum((Obs-Exp)^2 / Exp)
P <- pchisq(Chisq,bins-1,lower.tail=F)
list(Table=rbind(Lwr,Upr,Obs,Exp), Chisq=Chisq, P=P)
}

#Plot distance frequencies, pdf and detection function curves
plotmod <- function(mod,resln=20,title=NULL)
{	y <- mod$model@data$y
coefs <- coef(mod$model)
if("PH" %in% names(coefs)) coefs <- coefs[-which(names(coefs)=="PH")]
type <- mod$type
monotonic <- mod$monotonic
shape <- mod$shape
PkHt <- optimise(PDF, c(0,max(y)), maximum=T, prm=coefs, type=type, 
                 monotonic=monotonic ,shape=shape, maxy=max(y), log=F)$objective
hh <- hist(y, plot=F, breaks=resln)
maxx <- max(hh$breaks)
maxy <- max(hh$density, PkHt)
plot(hh, freq=F, xlim=c(0,7), ylim=c(0,maxy), main=title, ylab="PDF", xlab="", 
     yaxp=c(0,trunc(10*maxy)/10,1))
lines(seq(0,maxx,maxx/100), 
      PDF(seq(0,maxx,maxx/100),coefs,type,monotonic,shape,max(y),F), 
      col=2)
EDD <- EDDcalc(mod)
points(EDD, 0, pch=16, cex=2)

if(type=="point")
{	barprobs <- hh$density/hh$mids
bpsum <- sum(barprobs)*(hh$breaks[2]-hh$breaks[1])
probsum <- integrate(DF,0,max(y), prm=coefs, type="line", 
                     monotonic=monotonic, shape=shape, maxy=max(y))$value
prob <- DF(seq(0,maxx,maxx/100), coefs, "line", monotonic, shape, max(y))
barprobs <- barprobs * probsum/bpsum
plot(hh$breaks, c(barprobs,0), type="s", xlim=c(0,7), ylim=c(0,max(c(barprobs,prob))), 
     ylab="DP", xlab="", bty="n", yaxp=c(0,1,1))
mat <- cbind(c(barprobs[1],barprobs[-length(barprobs)]),barprobs)
minima <- apply(mat,1,min)
for(j in 1:length(barprobs))	lines(rep(hh$breaks[j],2),c(0,minima[j]))
lines(c(0,max(hh$breaks)),rep(0,2))
lines(seq(0,maxx,maxx/100), prob, col=2)
}
}

minf <- function(EDD, prm, maxy, type, monotonic, shape)
{	if(type=="line") x <- EDD else x <- EDD^2/2
sumIN <- x - integrate(DF,0,EDD,prm=prm,type=type,monotonic=monotonic,shape=shape,
                       maxy=maxy, rel.tol=1e-7,stop.on.error=F)$value
sumOUT <- integrate(DF,EDD,maxy,prm=prm,type=type,monotonic=monotonic,shape=shape,
                    maxy=maxy, rel.tol=1e-7,stop.on.error=F)$value
(sumIN - sumOUT)^2
}

#Calculates Effective Detection Distance
#Input prm can be either a vector of coefficients or a fitted model
EDDcalc <- function(prm,maxy=NULL,type=NULL,monotonic=NULL,shape=NULL)
{	if(!is.numeric(prm))
{	type <- prm$type
monotonic <- prm$monotonic
shape <- prm$shape
maxy <- max(prm$model@data$y)
prm <- coef(prm$model)
if("PH" %in% names(prm)) prm <- prm[-which(names(prm)=="PH")]
}
  optimise(minf,c(0,maxy), prm=prm, maxy=maxy, 
           type=type, monotonic=monotonic, shape=shape)$minimum
}

EDDvar <- function(mod, reps=1000)
{	sig <- mod$model@vcov
diag(sig) <- abs(diag(sig))
parammat <- mvrnorm(reps, mu = mod$model@coef, sig, tol=1e-3)
res <- apply(parammat,1,EDDcalc, maxy=max(mod$model@data$y), type=mod$type,
             monotonic=mod$monotonic, shape=mod$shape)
CI <- quantile(res,c(0.025,0.975))
names(CI) <- c("LCL","UCL")
c(CI,SE=var(res)^0.5)
}

EDDfunc <- function(mod,calcvar=TRUE,reps=1000)
{	res <- EDDcalc(mod)
names(res) <- "EDD"
if(calcvar) res <- c(res, EDDvar(mod,reps))
res
}


##############################################
# Linear covariate detection model functions #   --- ask about what this corresponds to?
##############################################


library(bbmle)	#for mle2
library(MASS)	#for mvrnorm (multivariate random normal numbers, for EDD conf int)

DF <- function(x,s,b,s0,b0)
  x * (1-exp(-(x/exp(s))^-b)) / (1 + exp(-b0*(x-s0)))

DFintgrl <- function(i,maxx,s,b,s0,b0)
{	if(length(s)>1) s <- s[i]
if(length(s0)>1) s0 <- s0[i]
integrate(DF,0,maxx,s=s,b=b,s0=s0,b0=b0,
          rel.tol=1e-07,stop.on.error=F)$value
}

PDF <- function(x,s,b,s0,b0,log=FALSE)
{	if(length(s)>1 | length(s0)>1)
  intgrl <- sapply(1:length(x),DFintgrl,maxx=max(x),s=s,b=b,s0=s0,b0=b0) else
    intgrl <- DFintgrl(1,max(x),s,b,s0,b0)
  res <- DF(x,s,b,s0,b0) / intgrl
  if(log)
  {	res <- log(res)
  res[is.infinite(res)] <- -999
  }
  res
}

#Calculates Effective Detection Distance
minf <- function(EDD, s,b,s0,b0)
{	sumIN <- EDD^2/2 - integrate(DF,0,EDD,s=s,b=b,s0=s0,b0=b0,rel.tol=1e-7,stop.on.error=F)$value
sumOUT <- integrate(DF,EDD,Inf,s=s,b=b,s0=s0,b0=b0,rel.tol=1e-7,stop.on.error=F)$value
(sumIN - sumOUT)^2
}
EDDcalc <- function(s,b,s0,b0) optimise(minf,c(0,10*s), s=s,b=b,s0=s0,b0=b0)$minimum
EDDrep <- function(i,s,b,s0,b0)
{	if(length(s)>1) s <- s[i]
if(length(s0)>1) s0 <- s0[i]
EDDcalc(s,b,s0,b0)
}

#Calculates effective detection distances given a set of model coefficients 
#as a named vector and the associated covariates as a list
#Covar names must match those defining the coefficients
EDDlinmod <- function(coefs,covars)
{	nms <- names(coefs)
b <- coefs[which(nms=="b")]
b0 <- coefs[which(nms=="b0")]
covarnms <- names(covars)	
si <- grep("s.",nms,fixed=T)
if(length(si)==1) s <- coefs[si] else
{	s <- coefs[si[1]]
for(i in 2:length(si))
{	covr <- strsplit(nms[i],".",fixed=T)[[1]][2]
s <- s + covars[[grep(covr,covarnms)]] * coefs[grep(nms[si[i]],nms)]
}
}
s0i <- grep("s0.",nms,fixed=T)
if(length(s0i)==1) s0 <- coefs[s0i] else
{	s0 <- coefs[s0i[1]]
for(i in 2:length(s0i))
{	covr <- strsplit(nms[i],".",fixed=T)[[1]][2]
s0 <- s0 + covars[[grep(covr,covarnms)]] * coefs[grep(nms[s0i[i]],nms)]
}
}
if(length(si)>1) ii <- length(covars[[1]]) else ii <- length(covars[[2]])
sapply(1:ii, EDDrep, s=s,b=b,s0=s0,b0=b0)
}

























# 3D plot -----------------------------------------------------------------

# could be nice for visualisation

library(MASS)
library(misc3d)
library(sp)

x <- seq(0,10, length = 1000)

y <- dist_prob(x)

y_f <- dist_prob_f(x)

y_h <- dist_prob_h(x)

# Create a camera detection zone

dz <- data.frame(x=5, y=2, r=6, th=1, dir=0)

plot_wrap(path, lineargs = list(col="grey"))
plot_dzone(dz, border=2)


# function to generate probability density z axis for every x and y point in the dzone

z_axis <- function(x,y){
  r <- sqrt(x^2 + y^2) # distance from CT
  dist_prob(r)
}



# generate set of x and y points in the dzone:
xx <- seq(0,10, length = 1000)
yy <- seq(0,10, length = 1000)
# make polygon:

# get list of xx and yy values in that polygon

sq <- with(dz[1, ], seq(dir-th/2, dir+th/2, len=50))
poly <- with(dz[1, ], cbind(x + c(0, r*sin(sq)), y + c(0, r*cos(sq)))) # set of x and y coords representing the points on perimeter of the dz polygon

# make polygon from these vertices
vertices_x <- poly[,1]
vertices_y <- poly[,2]

# make Polygons object
poly1 <- sp::Polygon(cbind(vertices_x, vertices_y))
dz_poly <- sp::Polygons(list(poly1), ID = "A") # class Polygons
dz_spatialpoly <- sp::SpatialPolygons(list(dz_poly)) # class SpatialPolygons

# intersect points with the polygon:

# using sf package (if can get it to work)
library(sf) # struggling to install it though
xxyy_in_dz <- st_intersection(points, poly)

# or using spatialEco package:
library(spatialEco) # but not available for this version of R
new_shape <- point.in.poly(pts, polys)

# using overlay:
# but need the points to be a SpatialPointsDataframe 
xy <- cbind(xx, yy)
pts <- SpatialPoints(xy)
xxyy_in_dz <- over(pts, dz_spatialpoly)
xxyy_in_dz_rmNA <- na.omit(xxyy_in_dz)
# not sure if it's that helpful though


# using simulation functions:
pth <- data.frame(x = xx,
                  y = yy)
dzone <- dz

isin <- is_in_dz(pth, dzone) # returns true or false for whether each position in the path is in the detection zone - this is probably where changes need to be made - to the is_in_dz function

isin[1,] <- FALSE # ask Marcus why this is necessary?

isin <- as.vector(isin)

pth <- pth[rep(1:nrow(pth), nrow(dzone)), ] # what does this line do??

newseq <- tail(isin, -1) > head(isin, -1)

xy <- pth[isin, ]

xy_in_dz <- data.frame(xy)



# get x, y, z coords for plotting:

x_3D <- xy_in_dz$x
y_3D <- xy_in_dz$y

z_3D <- mapply(z_axis, x_3D, y_3D)


# plotting:

persp(x = x_3D, y = y_3D, z = outer(x_3D, y_3D, z_axis),
      theta = 30,
      phi = 15,
      col = "orange",
      xlim = c(0,10),
      ylim = c(0,10))

#kde3d(x_3D, y_3D, z_3D)

# maybe try a different way: make a kernel density plot using regents park position data

posdata <- read.csv("data/posdat.csv")

den3d <- kde2d(posdata$x, posdata$y)
persp(den3d,
      main="Probability density of detection by a camera trap\n(foxes and hedgehogs)",
      zlab = "Probability density",
      xlab = "X",
      theta = 30, phi = 15,
      col = "orange", shade = 0.4)

den3d_f <- kde2d(posdata[posdata$species=="Fox",]$x, posdata[posdata$species=="Fox",]$y)
persp(den3d_f,
      main="Probability density of detection by a camera trap\n(foxes)",
      zlab = "Probability density",
      xlab = "X",
      theta = 30, phi = 15,
      col = "green", shade = 0.4)

den3d_h <- kde2d(posdata[posdata$species=="Hedgehog",]$x, posdata[posdata$species=="Hedgehog",]$y)
persp(den3d_h,
      main="Probability density of detection by a camera trap\n(hedgehogs)",
      zlab = "Probability density",
      xlab = "X",
      theta = 30, phi = 15,
      col = "orange", shade = 0.4)

# to do to make these plots better:

# - make into the shape of a detection zone
# - do a heated surface or something instead (what Francis was describing - ask him again)
# - make sure you can actually see the CT/where it's meant to be



# deciding on sizing parameters -------------------------------------------

## choosing sizing parameters for the simulation based on Regent's Park & Panama data ##

require(ggplot2)

panama_06 <- read.table("data/rse217-sup-0006-speeddatapublished.txt")

posdata <- read.csv("data/posdat.csv")

# Radii and angles --------------------------------------------------------

## distribution of radii and angles:

ggplot(posdata, aes(x = radius))+
  geom_density()+
  ggtitle("Regent's Park")

# don't have radii and angles in Panama data

# --> looks like radius of 10m would be good (it's the max in the regent's park data)


ggplot(posdata, aes(x = angle))+
  geom_density()

min(posdata$angle)
max(posdata$angle)

# --> looks like angle of around 0.82 x 2 (= 1.64) would be good







# coming up with PDf for detection at distance using RP data --------------

## working out a probability density function for how likely an animal is to trigger a camera at a given distance from it

library(ggplot2)

## to ask:
# - is this even a good idea?
# - what was the radius of the CT used for the regent's park data?
# - do we have more data we could use to make this more robust
# - discuss thoughts on how to include habitat as a factor (is this something to include in simulations etc?)
# - good idea to do it separately for each species?
# - should we also include the likelihood of the position data being accompanied by enough other positions to measure speed? (e.g. the further away you are probably the more likely to only be caught only once by the CT - so can't measure speed)

ggplot(data = posdata, aes(x = radius, colour = species))+
  geom_density()


# probability density estimation: using the data to estimate the general density of probabilities beyond just the sample of data

# looks pretty normal-distribution-based

# estimate the parameters of the distribution from the data

# mean:
mean_f <- mean(log(posdata[posdata$species=="Fox",]$radius))
mean_h <- mean(log(posdata[posdata$species=="Hedgehog",]$radius))

# standard deviation:
sd_f <- sd(log(posdata[posdata$species=="Fox",]$radius))
sd_h <- sd(log(posdata[posdata$species=="Hedgehog",]$radius))


# maybe do the 2 species together for now then separate out later if needed
mean <- mean(posdata$radius)
sd <- sd(posdata$radius)

# parametric density estimation: normal distribution with mean and sd worked out here


# check if this estimated density is a good fit:

# plot it with the data and compare:

x <- seq(0,10, length = 1000)
y <- dnorm(x, mean = mean, sd = sd, log = F)

ggplot()+
  geom_density(aes(x = posdata$radius))+
  geom_smooth(aes(x = x, y = y))

#--> it's alright but not amazing

# data are a bit skewed - would be maybe be good to transform it before estimating parameters?

# e.g. logging it:
mean2 <- mean(log(posdata$radius))
sd2 <- sd(log(posdata$radius))
y2 <- dnorm(x, mean = mean2, sd = sd2, log = F)


#ylognorm <- dlnorm(x, mean = mean, sd = sd, log = F)

ggplot()+
  geom_density(aes(x = log(posdata$radius)))+
  geom_smooth(aes(x = x, y = y2))

# --> much better
# although does this weird drop below zero for some reason - not sure why




# so now that have this estimated probability density (normal with mean = mean2 and sd = sd2), need to find a way to apply it to a probability of being detected at various radii



