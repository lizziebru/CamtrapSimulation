## sbd example ##

source("sbd_functions.R")

n <- 1000
x <- rnorm(n) # sample n values from a normal distribution (default mean = 0, sd = 1)
dat <- data.frame(s=rlnorm(n, x), # sample n values from a log normal distribution - but is x the meanlog? - surely that can't have multiple values
                  x=x)
# s = probably speed?

mods <- sbm3(s~1, dat) # replaced fit.sbd with sbm3 - does all 3 models in one


# plot models:

plot(mods$models[1], lpar=list(col=2)) # this doesn't work though - need to figure out what Marcus wanted to plot and how to do it
plot(gmod, lpar=list(col=3), add=T)
plot(wmod, lpar=list(col=4), add=T)
plot(lmod, log=FALSE)
plot(gmod, log=FALSE, lpar=list(col=3), add=T)
plot(wmod, log=FALSE, lpar=list(col=4), add=T)

# AICs:
mods$AICtab


predict(lmod)
hmean(dat$s)

lmod2 <- fit.spd(s~x, dat, "l")
AIC(lmod)
AIC(lmod2)
prdn <- predict(lmod2, data.frame(x=seq(-3,3,len=256)))
plot(prdn$x, log(prdn$est), type="l")
lines(prdn$x, log(prdn$lcl), lty=2)
lines(prdn$x, log(prdn$ucl), lty=2)