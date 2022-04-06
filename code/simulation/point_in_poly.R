## MARCUS' FUNCTIONS FOR FINDING WHETHER LINES/POINTS/POLYGONS INTERSECT ##

#https://www.geeksforgeeks.org/orientation-3-ordered-points/
orientation <- function(p1, p2, p3){
  sign((p2[,2]-p1[,2])*(p3[,1]-p2[,1]) - (p3[,2]-p2[,2])*(p2[,1]-p1[,1]))
}

#https://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/

#INPUT
# lines1, lines2: 4-column arrays of x,y start and end points (ordered x1,y1,x2,y2) with the same number of rows in each line
#VALUE 
# 1 = they intersect
# 0 = they don't intersect
lines_cross <- function(lines1, lines2){
  p1 <- lines1[, 1:2]
  q1 <- lines1[, 3:4]
  p2 <- lines2[, 1:2]
  q2 <- lines2[, 3:4]
  or1 <- orientation(p1, q1, p2)
  or2 <- orientation(p1, q1, q2)
  or3 <- orientation(p2, q2, p1)
  or4 <- orientation(p2, q2, q1)
  res <- ifelse(or1!=or2 & or3!=or4, 1, 0)
  return(res)
}


#Whether a point is on a line (given that orientation is colinear)
point_on_line <- function(ln1, ln2, pt){
  minx <- apply(cbind(ln1[,1], ln2[,1]), 1, min)
  miny <- apply(cbind(ln1[,2], ln2[,2]), 1, min)
  maxx <- apply(cbind(ln1[,1], ln2[,1]), 1, max)
  maxy <- apply(cbind(ln1[,2], ln2[,2]), 1, max)
  pt[,1]>=minx & pt[,2]>=miny & pt[,1]<=maxx & pt[,2]<=maxy
}

#https://www.geeksforgeeks.org/how-to-check-if-a-given-point-lies-inside-a-polygon/
#INPUT
# point, poly: two column arrays of co-ordinates (x,y in that order) defining points and a polygon
#              polygon co-ordinates need not be closed (first and last the same)
point_in_poly <- function(point, poly){
  lns.pnt <- cbind(point, min(poly[,1]-1), point[,2])
  lns.ply <- cbind(poly, rbind(tail(poly, -1), head(poly, 1)))
  cross <- lines_cross(lns.pnt, lns.ply)
  ncross <- apply(cross, 1, sum)
  ncross %% 2 == 1
}


## quadratic formula:
quad <- function(a, b, c) {
  if ((b^2 - 4 * a * c) > 0){
    solns <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
               (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
    return(solns)
  }
  else{
    return(NA)
  }
}

## line_arc_cross
# whether a line intersects an arc
# INPUT
# line: 4-column array of x,y start and end points (ordered x1,y1,x2,y2)
# arc: 4-column array of x,y centre of circle, radius r, and angle from centre to edge theta
# OUTPUT
# 1 = they intersect
# 0 = they don't intersect
line_arc_cross <- function(line, arc){
  # parametric equations for line:
  x1 <- line[1,1]
  y1 <- line[1,2]
  x2 <- line[1,3]
  y2 <- line[1,4]
  xvec <- x2 - x1
  yvec <- y2 - y1
  # paramx <- x1 + t*xvec
  # paramy <- y2 + t*yvec
  
  # parametric eqns for the circle which the arc is part of:
  xc <- arc[1,1]
  yc <- arc[1,2]
  r <- arc[1,3]
  th <- arc[1,4]
  # cparamx <- xc + rcos(th)
  # cparamy <- yc + rsin(th)
  
  # equate parametric equations for x and y:
  # x1 + t*xvec = xc + rcos(th)
  # y1 + t*yvec = yc + rsin(th)
  # use sin^2 + cos^2 = 1 to get quadratic equation for t, where:
  a <- xvec^2 + yvec^2
  b <- 2*x1*xvec - 2*xc*xvec + 2*y1*yvec - 2*yc*yvec
  c <- x1^2 + xc^2 - 2*x1*xc + y1^2 + yc^2 - 2*y1*yc - r^2

  t <- quad(a, b, c) # solve for t using quadratic formula
  
  if (is.na(t) == TRUE){ # if no solutions to t: they don't intersect
    answer <- 0
  }
  else{
    # use values of t to find x and y then check if the corresponding theta lies along the arc (i.e. check whether when they do intersect it's along that specific arc of the circle)
    x_soln1 <- x1 + t[1]*xvec
    y_soln1 <- y2 + t[1]*yvec
    x_soln2 <- x1 + t[2]*xvec
    y_soln2 <- y2 + t[2]*yvec
    
    # find value of theta for each solution:
    th_soln1 <- atan((y_soln1 - yc)/(x_soln1 - xc))
    th_soln2 <- atan((y_soln2 - yc)/(x_soln2 - xc))
    
    if ((-th <= th_soln1 & th_soln1 <= th) | (-th <= th_soln2 & th_soln2 <= th)){ # if either solution lies in the correct range of theta, the line does intersect the arc
      answer <- 1
    }
    else{
      answer <- 0
    }
  }
  return(answer)
}




# not needed atm ----------------------------------------------------------
## lines_cross - ORIGINAL
#INPUT
# lines1, lines2: 4-column arrays of x,y start and end points (ordered x1,y1,x2,y2)
# expand: if TRUE, calculates for all combinations of lines1 and lines2, otherwise row by row
#         If expand==FALSE, lines1 and lines2 must have the same number of rows.
#VALUE 
# 1 if crossed
# 0 if not crossed
# 0.5 if line 1 skims line 2 (ie an end point of 2 lies on 1)
# lines_cross <- function(lines1, lines2, expand=TRUE){
#   if(expand){
#     nr <- nrow(lines1)
#     ij <- expand.grid(1:nrow(lines1), 1:nrow(lines2))
#     lines1 <- lines1[ij$Var1, ]
#     lines2 <- lines2[ij$Var2, ]
#   }
#   p1 <- lines1[, 1:2]
#   q1 <- lines1[, 3:4]
#   p2 <- lines2[, 1:2]
#   q2 <- lines2[, 3:4]
#   or1 <- orientation(p1, q1, p2)
#   or2 <- orientation(p1, q1, q2)
#   or3 <- orientation(p2, q2, p1)
#   or4 <- orientation(p2, q2, q1)
#   res <- ifelse(or1!=or2 & or3!=or4, 1, 0)
#   skim <- (or1==0 & point_on_line(p1, q1, p2)) | (or2==0 & point_on_line(p1, q1, q2))
#   res <- res - ifelse(skim, 0.5, 0)
#   if(expand) res <- matrix(res, nrow=nr)
#   res
# }




# to test arc function:
# line <- data.frame(x1 = 2,
#                    y1 = 4,
#                    x2 = 8,
#                    y2 = 9)
# 
# arc <- data.frame(x = 2,
#                   y = 4,
#                   r = 10,
#                   th = pi/4)