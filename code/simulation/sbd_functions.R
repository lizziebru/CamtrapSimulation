## Calculating average speed ##

require(bbmle)
require(MASS)

setClass("sbm", representation("list"))

# how max likelihood works: 
# look for parameter values (mean & sd) that maximise the likelihood of the data given the model 
# (i.e. find the parameters to be put in the model to make it fit the best)


# Harmonic mean and standard error
# non-parametric
# (not fitting a distribution - just taking an average)
hmean <- function(x){
  mn <- 1/mean(1/x)
  se <- mn^2 * sqrt(var(1/x)/length(x))
  c(mean=mn, se=se)
}




# Size biased log normal probability density
dsblnorm <- function(x, lmean, lsig, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dlnorm(x, lmean-exp(lsig)^2/2, exp(lsig)) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}  

#Size biased gamma probability density
dsbgamma <- function(x, lmean, lrate, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dgamma(x, exp(lmean)*exp(lrate), exp(lrate)) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}  

#Size biased Weibull probability density
dsbweibull = function(x, lmean, lshape, log=FALSE, xlog=FALSE){
  lmean <- as.vector(lmean)
  if(xlog==TRUE) xx <- x^2 else xx <- x
  res <- dweibull(x, exp(lshape), exp(lmean)/gamma(1+1/exp(lshape))) * xx / exp(lmean)
  res[res==0] <- 5e-324
  if(log==TRUE) log(res) else (res)
}  



#Size biased model
#INPUT
# formula: a model formula with speed variable on the left and covariates (or 1) on the right
# data: a dataframe containing the speed variable and any covariates
# pdf: which (size-biased) distribution to fit
sbm <- function(formula, data, pdf=c("lnorm", "gamma", "weibull"),
                var.range=c(-4,4), trace=FALSE){
  dstrbn=match.arg(pdf)
  y <- model.response(model.frame(formula, data))
  lmn <- log(hmean(y)[1])
  lv <- switch(dstrbn,
               lnorm = log(sd(log(y))),
               gamma = log(exp(lmn)/var(y)),
               weibull = 0)
  startpars <- switch(dstrbn,
                      lnorm = list(lmean=lmn, lsig=lv),
                      gamma = list(lmean=lmn, lrate=lv),
                      weibull = list(lmean=lmn, lshape=lv))
  lwr <- switch(dstrbn,
                lnorm = c(lsig=var.range[1]),
                gamma = c(lrate=var.range[1]),
                weibull = c(lshape=var.range[1]))
  upr <- switch(dstrbn,
                lnorm = c(lsig=var.range[2]),
                gamma = c(lrate=var.range[2]),
                weibull = c(lshape=var.range[2]))
  f1 <- switch(dstrbn,
               lnorm = as.formula(paste(as.character(formula)[2], "~ dsblnorm(lmean, lsig)")),
               gamma = as.formula(paste(as.character(formula)[2], "~ dsbgamma(lmean, lrate)")),
               weibull = as.formula(paste(as.character(formula)[2], "~ dsbweibull(lmean, lshape)"))
  )
  f2 <- as.formula(paste("lmean ~", as.character(formula)[3]))
  model <- mle2(f1, start=startpars, data=data, method="L-BFGS-B",
                lower=lwr, upper=upr, parameters=list(f2), trace=trace)
  
  res <- list(model=model, pdf=dstrbn, formula=formula)
  class(res) <- "sbm"
  res
}


#Predict average speed
#INPUT
# mod: a size biased model created using sbm
# newdata: a dataframe containing covarariate values at which to predict speed
# reps: number of random replicates over which to calculate SE
predict.sbm <- function(mod, newdata=NULL, reps=1000){
  if(length(attr(terms(mod$formula), "term.labels")) > 0 & is.null(newdata))
    stop("Your model has covariates - please provide newdata")
  
  if(is.null(newdata)) newdata <- data.frame(lmean=0) else
    newdata$lmean <- 0
  cfs <- mod$model@coef
  scfs <- mvrnorm(reps, cfs, mod$model@vcov)
  i <- grep("lmean.", colnames(scfs))
  scfs <- scfs[,i]
  cfs <- cfs[i]
  ff <- formula(strsplit(mod$model@formula, ": ")[[1]][2])
  m <- model.frame(ff, newdata)
  nms <- names(m)[sapply(m[, 1:ncol(m)], class) == "factor"]
  for(nm in nms){
    if(nm %in% names(mod$model@data)) lvls <- levels(mod$model@data[[nm]]) else
      lvls <- levels(eval(as.name(nm)))
    levels(m[,nm]) <- lvls
  }
  mat <- model.matrix(ff, m)
  res <- exp(mat %*% t(scfs))
  outp <- data.frame(newdata[, -ncol(newdata)], 
                     est=exp(mat %*% matrix(cfs, ncol=1)),
                     se=apply(res, 1, sd),
                     lcl=apply(res, 1, quantile, 0.025),
                     ucl=apply(res, 1, quantile, 0.975)
  )
  names(outp)[1:(ncol(newdata))-1] <- names(newdata)[-ncol(newdata)]
  outp
}


#Fits all three size biased options
#INPUT 
# As for sbm
#OUTPUT
# A list containing:
#  models: a list containing the three fitted sbm models
#  AICtab: a table of AIC and deltaAIC values for each model
sbm3 <- function(formula, data, reps=1000){
  mods <- list(sbm(formula, data, "lnorm"),
               sbm(formula, data, "gamma"),
               sbm(formula, data, "weibull")
  )
  names(mods) <- c("lnorm", "gamma", "weibull")
  AICs <- unlist(lapply(mods, AIC))
  i <- order(AICs)
  tab <- data.frame(AIC=AICs, dAIC=AICs-min(AICs))[i, ]
  rownames(tab) <- names(mods)[i]
  list(models=mods, AICtab=tab)
}

#Extract AIC from a size biased model
AIC.sbm <- function(obj) AIC(obj$model)


#Plot a size-biased model data and fitted distributions
#INPUT
# obj: a size biased model fitted with sbm
# log: whether to plot the distribution log scale
# lpar: plotting paramaters defining fitted line characteristics
# ...: other plotting arguments, if breaks given, passed to hist definition, otherwise passed to plot
##--> NB: I've added a title to this now
plot.sbm <- function(obj, log=TRUE, lpar=list(col="red"), add=FALSE, title, ...){
  if(length(attr(terms(obj$formula), "term.labels")) > 0)
    stop("Cannot plot covariate models")
  
  xname <- as.character(obj$formula)[2]
  dat <- obj$model@data
  if(xname %in% names(obj$model@data)) x <- get(xname, dat) else x <- get(xname)
  dots <- list(...)
  argnames <- names(dots)
  if("breaks" %in% argnames) brks <- dots["breaks"] else brks <- 50
  cfs <- coef(obj$model)
  if(log){
    lnx <- log(x)
    h <- do.call(hist, c(list(x=lnx, plot=FALSE), brks))
    sq <- exp(seq(min(lnx), max(lnx), len=256))
  } else{
    h <- do.call(hist, c(list(x=x, plot=FALSE), brks))
    sq <- seq(1e-10, max(x), len=256)
  }
  h$xname <- "x"
  den <- switch(obj$pdf,
                gamma = dsbgamma(sq, cfs[1], cfs[2], xlog=log),
                lnorm = dsblnorm(sq, cfs[1], cfs[2], xlog=log),
                weibull = dsbweibull(sq, cfs[1], cfs[2], xlog=log)
  )
  dots <- dots[!argnames=="breaks"]
  dots <- c(list(h, freq=FALSE), dots)
  if(!("main" %in% argnames)) dots <- c(dots, main="")
  if(!("xlab" %in% argnames)) dots <- c(dots, xlab=xname)
  if(!("ylim" %in% argnames)) dots <- c(dots, list(ylim=c(0,max(c(den, h$density)))))
  if(!add) do.call(plot, dots)
  if(log) sq <- log(sq)
  do.call(lines, c(list(x=sq, y=den), lpar))
  title(paste(title, "model", sep = " "))
}

