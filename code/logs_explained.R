x <- rlnorm(100, sd=3)
y <- rlnorm(100, sd=3)
plot(x, y)

# axes labels are the log(x) and log(y) values
plot(log(x), log(y))

# axes labels are the original x and y values but points are the logged values
plot(log(x), log(y), xaxt="n", yaxt="n")
axis(1, log(10^(-3:3)), 10^(-3:3))
axis(2, log(10^(-3:3)), 10^(-3:3))
# so the axes labels correspond to 10^(xcoord) and 10^(ycoord) -- easier to understand bc the axes have real-life meaning
# so this corresponds to the plots in Rowcliffe et al. 2016

# to work out lm fit of the logged values:
# lm(log10(y)~log10(x))



