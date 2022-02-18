## DISTANCE FROM CT PROBABILITY DENSITY OF BEING DETECTED ##

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


# 2 ways of applying this:

# 1. resample TRUE/FALSE in is_in_dz function with p(TRUE) = p(getting detected at that distance based on the probability density function found here)
# - done this and made new function = is_in_dz2 in CamtrapSimulation.R

# 2. re-shape the dzone to take into account that you're more likely to get detected closer to the CT




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



# now that have x, y, z coords: try plotting using persp

x_3D <- xy_in_dz$x
y_3D <- xy_in_dz$y

z_3D <- mapply(z_axis, x_3D, y_3D)

persp(x = x_3D, y = y_3D, z = outer(x_3D, y_3D, z_axis))



