# plots for presentation

require(ggplot2)



# density plot
dens_x <- rnorm(1000000,mean=5,sd=1)
dens_y <- rnorm(dens_x)
dens_df <- data.frame(x=dens_x,
                      y=dens_y)

dens_1 <- ggplot(dens_df,aes(x=dens_x))+
  geom_density()
dens_1

plot(density(dens_x))
