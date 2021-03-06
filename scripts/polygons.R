### Understanding polygon
################################################
# https://statisticsglobe.com/r-polygon-function-plot/

plot(1, 1, col = "white", xlab = "X", ylab = "Y")   
polygon(x = c(0.7, 1.3, 1.2, 0.8),                           # X-Coordinates of polygon
        y = c(0.6, 0.8, 1.4, 1),                             # Y-Coordinates of polygon
        col = "#1b98e0")    


plot(0, 1, col = "white", xlab = "X", ylab = "Y")   
originX = 0.5
originY = 0.6
width = 0.5
height = 0.5
polygon(x = c(originX, originX,origin+width,origin+width),                           # X-Coordinates of polygon
        y = c(originY, originY+height,originY+height,originY),                             # Y-Coordinates of polygon
        col = "#1b98e0")    

####
plot.new()

n <- 100
xx <- c(0:n, n:0)
yy <- c(c(0, cumsum(stats::rnorm(n))), rev(c(0, cumsum(stats::rnorm(n)))))
plot   (xx, yy, type = "n", xlab = "Time", ylab = "Distance")
polygon(xx, yy, col = "gray", border = "red")
title("Distance Between Brownian Motions")


plot.new()
polygon(1:9, c(2,1,2,1,NA,2,1,2,1),
        density = c(10, 20), angle = c(-45, 45))
dev.off(
  
)

##

#vector of x-coordinates and a vector of y-coordinates.
# For example, the following command should generate a 
# rectangle that is 0.2 units long and 0.7 units high,
# with its lower-left hand corner at the origin. 
# Note that the order of the points is important because the outline of the shape will be traced in the order that the points are listed.

plot.new()
polygon(c(0,0,1,1,-1,-1,-1,-2),
        c(1,2,1,2,-1,-2,-3,-3))


############# 

makeSquare <- function(origin=0,width=0.5,height=0.5){
  xAx <- c(origin,origin,origin+width,origin+width)
  yAx <- c(origin,origin+height,origin+height,origin)
  out <- list(xAx,yAx)
  return (out)
}



plotPoly <- function(ranColor){
  for (i in seq(1,10)){
    or <- runif(1)
    wi <- runif(1)
    he <- runif(1)
    scaleOr <- sample(c(1,-1),1)
    scaleWi <- sample(c(1,-1),1)
    scaleHe <- sample(c(1,-1),1)
    
    polygon(makeSquare(or*mult,wi*scaleWi,he*scaleHe)[[1]],
            makeSquare(or*mult,wi*scaleWi,he*scaleHe)[[2]],
            col=adjustcolor(ranColor,alpha.f=0.25))
  }
}


plot.new()
for (i in seq(1,5000)){
    ranColor <- sample(colors(),1)
    #print(ranColor)
    plotPoly(ranColor)
}

#### Try
#https://github.com/quantixed/gBlocks/blob/master/r/grid_of_hysterical%20squares.R