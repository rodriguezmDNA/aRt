library(grid)
plot.new()


center = 0 
radius = 0.5

plot(0, 0,
     xlim=c(-2,2),
     ylim=c(-2,2),
     col = "white", xlab = "", ylab = "", axes=F)
grid.circle(r = .103,x = 0.5205,y=0.511)
points(center,center,pch=20)


for (i in seq(1,30000/2)){
  x <- runif(1,-1.5,1.5)
  y <- runif(1,-1.5,1.5)
  if ( abs(x-center) > l*2 | abs(y-center) > l){
    #col <- sample(colors(),1)
    #pch <- sample(1:12,1)
    #points(x,y,pch='.',col=col)
    points(x,y,pch='.')
  }
}


for (i in seq(1,30000/2)){
  x <- runif(1,-1.5,1.5)
  y <- runif(1,-1.5,1.5)
  if ( abs(x-center) > l | abs(y-center) > l*4){
    #col <- sample(colors(),1)
    #pch <- sample(1:12,1)
    #points(x,y,pch='.',col=col)
    points(x,y,pch='.')
  }
}


############# Quadrants

plot(0, 0,
     xlim=c(-2,2),
     ylim=c(-2,2),
     col = "white", xlab = "", ylab = "", axes=F)
for (i in seq(1,30000/2)){
  x <- runif(1,-1.5,1.5)
  y <- runif(1,-1.5,1.5)
  if ( (abs(x-center) > l/2 & abs(y-center) > l/2) | (abs(x-center) > l/2 & abs(y-center) > l/2)){
    #col <- sample(colors(),1)
    #pch <- sample(1:12,1)
    #points(x,y,pch='.',col=col)
    points(x,y,pch='.')
  }
}



#### Hourglass
plot(0, 0,
     xlim=c(-2,2),
     ylim=c(-2,2),
     col = "white", xlab = "", ylab = "", axes=F)
for (i in seq(1,30000/2)){
  x <- runif(1,-1.5,1.5)
  y <- runif(1,-1.5,1.5)
  if ( (abs(x-center)/abs(y-center) < l**2) | (abs(x-center)/abs(y-center) < l**2)){
    #col <- sample(colors(),1)
    #pch <- sample(1:12,1)
    #points(x,y,pch='.',col=col)
    points(x,y,pch='.')
  }
}


#### Wierd shape
plot(0, 0,
     xlim=c(-2,2),
     ylim=c(-2,2),
     col = "white", xlab = "", ylab = "", axes=F)
for (i in seq(1,30000/2)){
  x <- runif(1,-1.5,1.5)
  y <- runif(1,-1.5,1.5)
  if ( ( (x-center)/(y-center) < cos(l) ) & (x-center)/(y-center) < tan(l) ){
    #col <- sample(colors(),1)
    #pch <- sample(1:12,1)
    #points(x,y,pch='.',col=col)
    points(x,y,pch='.')
  }
}