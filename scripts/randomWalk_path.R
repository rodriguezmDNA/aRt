
plot(0, 0,
     xlim=c(-10,10),
     ylim=c(-10,10),
     col = "white", xlab = "", ylab = "", axes=F)

for (xprev in seq(-9.5,9.5,1)){
  for (yprev in seq(-9.5,9.5,1)){
    lineColor=sample(colors(),1)
    points(xprev,yprev)
    for (i in seq(1:5)){
      
      xnew <- xprev +runif(1,-0.5,0.5)
      ynew <- yprev +runif(1,-0.5,0.5)
      
      ### Limit to canvas
      # while (abs(xnew) >= 1 | abs(ynew) >=1 ){
      #   xnew <- xprev +runif(1,-0.25,0.25)
      #   ynew <- yprev +runif(1,-0.25,0.25)
      # }
      # 
      lineCoords <- data.frame(rbind(c(xprev,yprev),c(xnew,ynew)))
      lineCoords
      lines(lineCoords,col=lineColor)
      #lines(1/t(lineCoords))
      
      xprev <- xnew
      yprev <- ynew
      
    }
    
  } 
}
 

