

center = 0 
radius = 0.5

plot(0, 0,
     xlim=c(-2,2),
     ylim=c(-2,2),
     col = "white", xlab = "", ylab = "", axes=F)
points(center,center,pch=20)
for (i in seq(1,100000)){
  x <- runif(1,-1.5,1.5)
  y <- runif(1,-1.5,1.5)
  if (((center-x)**2 + (center-y)**2) >= radius**2){
    col <- sample(colors(),1)
    #pch <- sample(1:12,1)
    points(x,y,pch='.',col=col)
  }
}
