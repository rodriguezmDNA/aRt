
plot(0, 0,
     xlim=c(-1,1),
     ylim=c(-1,1),
     col = "white", xlab = "", ylab = "", axes=F)
xprev <- -0.5
yprev <- -0.5
lineColor=sample(colors(),1)
for (i in seq(1:10)){
  #points(xprev,yprev)
  xnew <- xprev +runif(1,-0.5,0.5)
  ynew <- yprev +runif(1,-0.5,0.5)
  
  ### Limit to canvas
  while (abs(xnew) >= 1 | abs(ynew) >=1 ){
    xnew <- xprev +runif(1,-0.25,0.25)
    ynew <- yprev +runif(1,-0.25,0.25)
  }
  
  lineCoords <- data.frame(rbind(c(xprev,yprev),c(xnew,ynew)))
  lineCoords
  lines(lineCoords,col=lineColor)
  #lines(1/t(lineCoords))
  
  xprev <- xnew
  yprev <- ynew
  
}




