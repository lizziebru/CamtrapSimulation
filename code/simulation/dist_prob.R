## DISTANCE FROM CT PROBABILITY DENSITY OF BEING DETECTED ##

require(stats4)

# 2 ways of applying this:

# 1. resample TRUE/FALSE in is_in_dz function with p(TRUE) = p(getting detected at that distance based on the probability density function found here)
# - done this and made new function = is_in_dz2 in CamtrapSimulation.R

# 2. re-shape the dzone to take into account that you're more likely to get detected closer to the CT


# --> first method is better

# FINAL -------------------------------------------------------------------

# candidates:
# - my normal function derived from the regents' park data -- but check with M cause there's not THAT much data so might be better to stick to what the paper did 
# - Rowcliffe et al. 2011 hazard rate model (potentially then adding logistic mix if simulating smaller spp) - currently best is to work out the parameters from the regents' park data -- but check with M this is a good idea

# for each one: need to decide whether to use box foxes and hedgehogs combined or both separately instead


# work out the parameters needed to put in the hazard rate function:

hazard <- function(dist_or_angle, width, shape){ # == the probability density function estimated from fox & hedgehog data combined
  1 - exp(-(width/dist_or_angle)^shape)
}

posdata <- read.csv("data/posdat.csv")


# work out parameters using fox & hedgehog data together:

ggplot(posdata, aes(x = radius))+
  geom_density()

ggplot(posdata, aes(x = angle))+
  geom_density()

# try overlay with hazard model with random parameters and see what happens:

x <- seq(0,10, length = 1000)
y <- sapply(x, hazard, width = 2, shape = 2)

ggplot()+
  geom_density(aes(x = posdata$radius))+
  geom_smooth(aes(x = x, y = y))

# also work out parameters for either just foxes or just hedgehogs:


# max likelihood:

# negative log likelihood function:

hazard_nll <- function(width, shape){
  x <- seq(0,10, length = 1000)
  -sum(log(1 - exp(-(width/x)^shape)))
}


# compute MLE coefficient estimates:
est <- stats4::mle(minuslog=hazard_nll, start=list(width=2, shape=2), method = "Nelder-Mead")
summary(est)

# could use optim too?

## -- go from here: fit hazard to dist and angle using MLE to get the parameters and plonk those into the is_in_dz function


# instead: could use half normal function too:

half_normal <- function(dist_or_angle, sigma){
  exp(-dist_or_angle^2/2*sigma^2)
}

x <- seq(0,10, length = 1000)
y_hn <- sapply(x, half_normal, sigma = 1) # Marcus suggested sigma of 1 or 2 should be good but still doesn't look great

ggplot()+
  geom_density(aes(x = posdata$radius))+
  geom_smooth(aes(x = x, y = y_hn))


# hazard definitely looks better - just would be nice to find parameters than fit nicely



## best thing to do would be to find specific parameters/add on logistic mix etc depending on what category of animal I'm simulating

# but for now: ask Francis to help with how to do Max Likelihood



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






