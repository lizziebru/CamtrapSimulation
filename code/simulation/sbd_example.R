## sbd example ##


source("sbd_functions.R")

n <- 1000
x <- rnorm(n)
dat <- data.frame(s=rlnorm(n, x), x=x)

lmod <- fit.spd(s~1, dat, "l")
gmod <- fit.spd(s~1, dat, "g")
wmod <- fit.spd(s~1, dat, "w")
plot(lmod, lpar=list(col=2))
plot(gmod, lpar=list(col=3), add=T)
plot(wmod, lpar=list(col=4), add=T)
plot(lmod, log=FALSE)
plot(gmod, log=FALSE, lpar=list(col=3), add=T)
plot(wmod, log=FALSE, lpar=list(col=4), add=T)

AIC(lmod)
AIC(gmod)
AIC(wmod)

predict(lmod)
hmean(dat$s)

lmod2 <- fit.spd(s~x, dat, "l")
AIC(lmod)
AIC(lmod2)
prdn <- predict(lmod2, data.frame(x=seq(-3,3,len=256)))
plot(prdn$x, log(prdn$est), type="l")
lines(prdn$x, log(prdn$lcl), lty=2)
lines(prdn$x, log(prdn$ucl), lty=2)