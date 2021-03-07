
plot(0, 0,
     xlim=c(-5,5),
     ylim=c(-5,5),
     col = "white", xlab = "", ylab = "", axes=F)
xprev <- 0
yprev <- 0
for (i in seq(1:5000)){
  #points(xprev,yprev)
  xnew <- xprev +runif(1,-0.5,0.5)
  ynew <- yprev +runif(1,-0.5,0.5)
  
  ### Limit to canvas
  while (abs(xnew) >= 5 | abs(ynew) >=5 ){
    xnew <- xprev +runif(1,-0.5,0.5)
    ynew <- yprev +runif(1,-0.5,0.5)
  }
  
  lineCoords <- data.frame(rbind(c(xprev,yprev),c(xnew,ynew)))
  lineCoords
  lines(lineCoords,col=sample(colors(),1))
  
  xprev <- xnew
  yprev <- ynew
  
}

