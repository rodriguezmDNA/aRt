


library(grid)
plot.new()


center = 0 
radius = 0.5

plot(0, 0,
     xlim=c(-2,2),
     ylim=c(-2,2),
     col = "white", xlab = "", ylab = "", axes=F)
grid.circle(r = .1,x = 0.5,y=0.5)
points(center,center,pch=20)
for (i in seq(1,3000)){
  x <- runif(1,-1.5,1.5)
  y <- runif(1,-1.5,1.5)
  if (((center-x)**2 + (center-y)**2) <= radius**2){
    #col <- sample(colors(),1)
    #pch <- sample(1:12,1)
    #points(x,y,pch='.',col=col)
    points(x,y,pch=0)
  }
}


#### Cross
center = 0 
l = 0.5
# plot(0, 0,
#      xlim=c(-2,2),
#      ylim=c(-2,2),
#      col = "white", xlab = "", ylab = "", axes=F)
#points(center,center,pch=20)
for (i in seq(1,2500)){
  x <- runif(1,-1.5,1.5)
  y <- runif(1,-1.5,1.5)
  if ( abs(x-center) > l | abs(y-center) > l){
    #col <- sample(colors(),1)
    #pch <- sample(1:12,1)
    #points(x,y,pch='.',col=col)
    points(x,y,pch=1)
  }
}

