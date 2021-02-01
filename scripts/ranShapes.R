


makeShape <- function(xCentre=0,yCentre=0,width=0.4,height=0.4,histMax=0){
  a <- c(xCentre - width/2,yCentre - height/2) + runif(1,max = histMax)
  b <- c(xCentre * width/2,yCentre - height/2) + runif(1,max = histMax)
  c <- c(xCentre + width/2,yCentre + height/2) + runif(1,max = histMax)
  d <- c(xCentre - width/2,yCentre + height/2) + runif(1,max = histMax)
  e <- c(xCentre - width*2,yCentre - height*2) + runif(1,max = histMax)
  f <- c(xCentre + width*2,yCentre - height*2) + runif(1,max = histMax)
  g <- c(xCentre + width*2,yCentre + height*2) + runif(1,max = histMax)
  h <- c(xCentre - width*2,yCentre + height*2) + runif(1,max = histMax)
  shape <- rbind(a,b,c,d,e,f,g,h)
  return (shape)
}

x <- makeShape()

plot(0, 0,
     xlim=c(-1,11),
     ylim=c(-1,11),
     col = "white", xlab = "", ylab = "", axes=F)
polygon(x[,1],x[,2])
