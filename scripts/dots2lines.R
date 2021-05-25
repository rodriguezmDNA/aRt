getX <- function(t,a=6,b=2,c=14,d=3) { cos(t) + (cos(a*t)/b) + (sin(c*t) * 1/d) }
getY <- function(t,a=6,b=2,c=14,d=3) { sin(t) + (sin(a*t)/b) + (cos(c*t) * 1/d) }

xPts <- do.call(getX,list(seq(1,10,.15)))
yPts <- do.call(getY,list(seq(1,10,.15)))

#### Add markers to line
emptyCanvas(4)
lines(cbind(xPts,yPts))
for (i in seq(1,length(xPts))){
  if (i%%3 == 0){
    points(xPts[i],yPts[i],pch=22)
  }
  if (i%%4 == 0){
    points(xPts[i],yPts[i],pch=24)
  }
    
}

####

getX <- function(t,a=6,b=2,c=14,d=3) { cos(t)  }
getY <- function(t,a=6,b=2,c=14,d=3) { sin(t) + (cos(a*t)/b) + (cos(c*t) * 1/d) }

xPts <- do.call(getX,list(seq(1,10,.15)))
yPts <- do.call(getY,list(seq(1,10,.15)))

#### Add markers to line
emptyCanvas(4)
lines(cbind(xPts,yPts))
for (i in seq(1,length(xPts))){
  if (i%%3 == 0){
    points(xPts[i],pch=22)
  }
  if (i%%4 == 0){
    points(xPts[i],pch=24)
  }
  
}

