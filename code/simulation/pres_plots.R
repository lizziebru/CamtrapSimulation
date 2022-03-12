## plots for presentation ##

library(cowplot)

ggplot(data = data.frame(x = c(0, 5)), aes(x))+
  stat_function(fun = dnorm, n = 101, args = list(mean = 2.5, sd = 1), size = 3)+
  ylab("")+
  scale_y_continuous(breaks = NULL)+
  theme_minimal()+
  xlab("")+
  geom_vline(xintercept = 2.5, colour = "blue", size = 3)+
  geom_vline(xintercept = 2.3, colour = "red", size = 3)

