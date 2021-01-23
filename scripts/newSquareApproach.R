xWave <- seq.int(1:10)
yWave <- seq.int(1:10)


xCentre <- 1
yCentre <- 1

makeSquare2 <- function(xCentre,yCenter){
  lt <- c(xCentre - 0.4,yCentre - 0.4)
  rt <- c(xCentre + 0.4,yCentre - 0.4)
  rb <- c(xCentre + 0.4,yCentre + 0.4)
  lb <- c(xCentre - 0.4,yCentre + 0.4)
  shape <- rbind(lt,rt,rb,lb)
  return (shape)
}

new_shape_end <- rbind(rt,rb,lb,lt)
new_shape <- cbind(new_shape_start,new_shape_end)

plot(0, 0,
     xlim=c(1,10),
     ylim=c(1,10),
     col = "white", xlab = "", ylab = "", axes=F)
for (i in seq(1,10)){
  for (j in seq(1,10)) {
  
    tmp <- makeSquare2(i,j)
    polygon(tmp[,1],tmp[,2]+j)  
}}

histMax = .1

makeSquareHist <- function(xCentre,yCenter,histMax){
  lt <- c(xCentre - 0.4,yCentre - 0.4) + runif(1,max = histMax)
  rt <- c(xCentre + 0.4,yCentre - 0.4) + runif(1,max = histMax)
  rb <- c(xCentre + 0.4,yCentre + 0.4) + runif(1,max = histMax)
  lb <- c(xCentre - 0.4,yCentre + 0.4) + runif(1,max = histMax)
  shape <- rbind(lt,rt,rb,lb)
  return (shape)
}

histm <- 0
levHist = 0.02
verbose = FALSE
minXY <- 2
maxXY <- 8

plot(0, 0,
     xlim=c(-2,12),
     ylim=c(-2,12),
     col = "white", xlab = "", ylab = "", axes=F)

for (i in seq(1,10)){
  for (j in seq(1,10)) {
    
    
    if ((i > minXY & i < maxXY) | (j > minXY & j < maxXY)){ 
      
      print(if (verbose) c(i,j,'yes'))
      
      
      maxIJ = (i * levHist) + (j * levHist)
      tmp <- makeSquareHist(i,j,histMax = runif(1,0,maxIJ))
      polygon(tmp[,1],tmp[,2]+j,col='transparent')  
      
    } else {
      print(if (verbose) c(i,j,'yes'))
      
      tmp <- makeSquareHist(i,j,histMax = histm)
      polygon(tmp[,1],tmp[,2]+j)
    }
    
  }}

plot(0, 0,
     xlim=c(-2,2),
     ylim=c(-2,2),
     col = "white", xlab = "", ylab = "", axes=F)

tmp <- makeSquareHist(1,1,histMax = 0)

polygon(tmp[,1],tmp[,2],col='transparent')  


rotX=function(x,y,angle) {x*cos(angle)-y*sin(angle)}
rotY=function(x,y,angle) {x*sin(angle)+y*cos(angle)}

rotTMP <- t(apply(tmp,1,function(x){rbind(rotX(x[1],x[2],45),rotY(x[1],x[2],45))}))
polygon(rotTMP[,1]+1.4,rotTMP[,2]-.4,col='transparent')  

angle=15
rotTMP <- t(apply(tmp,1,function(x){rbind(rotX(x[1],x[2],angle),rotY(x[1],x[2],angle))}))
polygon(rotTMP[,1],rotTMP[,2],col='transparent')  


