makeSquareHist <- function(xCentre,yCenter,histMax){
  lt <- c(xCentre - 0.4,yCentre - 0.4) + runif(1,max = histMax)
  rt <- c(xCentre + 0.4,yCentre - 0.4) + runif(1,max = histMax)
  rb <- c(xCentre + 0.4,yCentre + 0.4) + runif(1,max = histMax)
  lb <- c(xCentre - 0.4,yCentre + 0.4) + runif(1,max = histMax)
  shape <- rbind(lt,rt,rb,lb)
  return (shape)
}

plot(0, 0,
     xlim=c(-1,11),
     ylim=c(-1,11),
     col = "white", xlab = "", ylab = "", axes=F)

for (i in seq(1,10)){
  for (j in seq(1,10)) {
    
      
    thet <- sample(seq(0,360,5),1)
    rotatedXY <- t(apply(tmp,1,function(x){ rotXY(x[1],x[2],thet,20/i,20/j) }))
    polygon(rotatedXY[,1],rotatedXY[,2],col='blue')  
    if ((i > minXY & i < maxXY) | (j > minXY & j < maxXY)){ 
      
      print(if (verbose) c(i,j,'yes'))
      
      
      maxIJ = (i * levHist) + (j * levHist)
      tmp <- makeSquareHist(i,j,histMax = runif(1,0,maxIJ))
      polygon(tmp[,1],tmp[,2]+j,col='pink')  
      
    } else {
      print(if (verbose) c(i,j,'yes'))
      
      tmp <- makeSquareHist(i,j,histMax = histm)
      polygon(tmp[,1],tmp[,2]+j,col='skyblue')
    }
    
    
    
    
    
  }}
