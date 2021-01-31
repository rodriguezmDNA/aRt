
makeSquareHist <- function(xCentre=0,yCentre=0,width=0.4,height=0.4,histMax=0){
  lt <- c(xCentre - width,yCentre - height) + runif(1,max = histMax)
  rt <- c(xCentre + width,yCentre - height) + runif(1,max = histMax)
  rb <- c(xCentre + width,yCentre + height) + runif(1,max = histMax)
  lb <- c(xCentre - width,yCentre + height) + runif(1,max = histMax)
  shape <- rbind(lt,rt,rb,lb)
  return (shape)
}

getHist <- function(histMax) {c(runif(1,max = histMax),
                        runif(1,max = histMax),
                        runif(1,max = histMax),
                        runif(1,max = histMax))}


rotXY = function(x,y,theta,p=0,q=0){
  xP = ((x-p)*cos(theta)) - ((y-q)*sin(theta)) + p
  yP = ((x-p)*sin(theta)) + ((y-q)*cos(theta)) + q
  return (c(xP,yP))
}

rotation <- function(x,thet,p,q){
  t(apply(polyg,1,function(x){rotXY(x[1],x[2],theta = thet,p,q)}))
}


rotXY = function(x,y,theta){
  xP = ((x)*cos(theta)) - ((y)*sin(theta)) 
  yP = ((x)*sin(theta)) + ((y)*cos(theta)) 
  return (c(xP,yP))
}

rotation <- function(polyg,thet){
  t(apply(polyg,1,function(x){rotXY(x[1],x[2],theta = thet)}))
}


p<-0
q<-0
histeresis <- getHist(0.25)
square <- makeSquareHist(p,q)
squareHist <- square + histeresis


plot(0,0)
polygon(square[,1],square[,2],col = 'transparent')
polygon(squareRot[,1],squareRot[,2],col = 'transparent')

squareRot <- rotation(square,45)
squareRotHist <- squareRot + histeresis

polygon(squareHist[,1],squareHist[,2],col = 'transparent')
polygon(squareRotHist[,1],squareRotHist[,2],col = 'transparent')





plot(0,0)
rep = 0 
while (rep < 10){
  p = sample(seq(-0.75,0.75,.01),1)
  q = sample(seq(-0.75,0.75,.01),1)
  thet <- sample(seq(0,360,5),1)
  maxH = sample(seq(0,.2,.01),1)
  print(c(p,q,thet,rep))
  #####
  polyg <- makeSquareHist(p,q,width = .25,height = .25,histMax = maxH)
  polygon(polyg[,1],polyg[,2],col = 'transparent')
  #####
  rotPoly =  t(apply(polyg,1,function(x){rotXY(x[1],x[2],theta = thet,p = p,q = q)}))
  polygon(rotPoly[,1],rotPoly[,2],col = 'transparent')
  
  # rotPoly =  t(apply(polyg,1,function(x){rotXY(x[1],x[2],theta = thet,p=p,q=q)}))
  # polygon(rotPoly[,1]+q,rotPoly[,2]+p,col = 'transparent')
  points(p,q,pch=3)
  ##
  rep=rep+1


}



plot(0,0)
rep = 0 
while (rep < 20){
  p = sample(seq(-0.75,0.75,.01),1)
  q = sample(seq(-0.75,0.75,.01),1)
  print(c(p,q))
  
  ##
  histeresis <- getHist(0.25)
  square <- makeSquareHist(p,q)
  squareHist <- square + histeresis
  squareRot <- rotation(square,45)
  squareRotHist <- squareRot + histeresis
  
  
  choose <- sample(seq(1:4),1)
  if (choose == 1) polygon(square[,1],square[,2],col = 'transparent')
  if (choose == 2) polygon(squareRot[,1],squareRot[,2],col = 'transparent')
  if (choose == 3) polygon(squareHist[,1],squareHist[,2],col = 'transparent')
  if (choose == 4) polygon(squareRotHist[,1],squareRotHist[,2],col = 'transparent')
  rep=rep+1
}


plot(0,0)
rep = 0 
while (rep < 20){
  p = sample(seq(-0.75,0.75,.01),1)
  q = sample(seq(-0.75,0.75,.01),1)
  print(c(p,q))
  
  ##
  histeresis <- getHist(0.25)
  square <- makeSquareHist(p,q)
  squareHist <- square + histeresis
  squareRot <- rotation(square,45)
  squareRotHist <- squareRot + histeresis
  
  
  choose <- sample(seq(1:4),1)
  if (choose == 1) polygon(square[,1],square[,2],col = 'transparent')
  if (choose == 2) polygon(squareRot[,1],squareRot[,2],col = 'transparent')
  if (choose == 3) polygon(squareHist[,1],squareHist[,2],col = 'transparent')
  if (choose == 4) polygon(squareRotHist[,1],squareRotHist[,2],col = 'transparent')
  rep=rep+1
}



plot(0, 0,
     xlim=c(-1,11),
     ylim=c(-1,11),
     col = "white", xlab = "", ylab = "", axes=F)
for (p in seq(1,10)){
  for (q in seq(1,10)) {
    
    histeresis <- getHist(0.25)
    square <- makeSquareHist(p,q)
    squareHist <- square + histeresis
    
    thet <- sample(seq(0,360,5),1)
    squareRot <- rotation(square,thet,p,q)
    squareRotHist <- squareRot + histeresis
    
    
    choose <- sample(seq(1:4),1)
    if (choose == 1) polygon(square[,1],square[,2],col = 'transparent')
    if (choose == 2) polygon(squareRot[,1],squareRot[,2],col = 'transparent')
    if (choose == 3) polygon(squareHist[,1],squareHist[,2],col = 'transparent')
    if (choose == 4) polygon(squareRotHist[,1],squareRotHist[,2],col = 'transparent')
    rep=rep+1
  }}

