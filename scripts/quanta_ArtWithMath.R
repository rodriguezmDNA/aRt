


### https://www.quantamagazine.org/how-to-create-art-with-mathematics-20151008
### https://www.quantamagazine.org/solution-creating-art-with-mathematics-20151030
x <- function(t,a=6,b=2,c=14,d=3) { cos(t) + (cos(a*t)/b) + (sin(c*t) * 1/d) }
y <- function(t,a=6,b=2,c=14,d=3) { sin(t) + (sin(a*t)/b) + (cos(c*t) * 1/d) }





getCurve <- function(a=6,b=2,c=14,d=3){
  out <- rbind(cbind(x(0),y(0)))
  # a = sample(seq(1,11),1)
  # b = sample(seq(1,11),1)
  # c = sample(seq(1,15),1)
  # d = sample(seq(1,11),1)
  #print(c(a,b,c,d))
  for (t in seq(0,sample(seq(9,15),1),.01)){
    out <- rbind(out,cbind(x(t,a=a,b=b,c=c,d=d),y(t,a=a,b=b,c=c,d=d)))
  }
  out <- out[-1,]
  return (out)
}

drawParamCurves <- function(){
  
  out <- getCurve()
  emptyCanvas(2)
  ### Mark certain points
  
  ## Alternating
  points(out[cos(out[,1])/sin(out[,1]) < sin(out[,2])/2,],pch=c(15,22,17))
  points(out[cos(out[,1])/sin(out[,1]) > sin(out[,2])/2,],pch=c(22,17,15))
  
  #lines(out[,1],out[,2])
}


##### 20210519
## Alternating shapes by passing a list to pch
out <- getCurve()
emptyCanvas(2)
points(out[cos(out[,1])/sin(out[,1]) < sin(out[,2])/2,],pch=c(15,22,17))
points(out[cos(out[,1])/sin(out[,1]) > sin(out[,2])/2,],pch=c(22,17,15))


##### 20210520
## Sparce points
out <- getCurve()
emptyCanvas(2)
points(out[seq(1,nrow(out),2),],pch=c(15,22,17))
#points(out[seq(1,nrow(out),3),],pch='O')  #Beads on a string
lines(out)


drawParamCurves()


grpA = data.frame(getCurve())
grpB = data.frame(getCurve() + 2)

#### Grid of parametric curves
drawGrid <- function(){
  emptyCanvas(10)
  for (i in seq(0,9,3)){
    for (j in seq(0,9,3)){
      grpA = data.frame(getCurve())
      grpA[,1] <- grpA[,1] + i
      grpA[,2] <- grpA[,2] + j
      lines(grpA[,1],grpA[,2])
    }}
  }

drawGrid()
drawGrid()

emptyCanvas(2)
for (i in seq(6,10,2)){
  lines(getCurve(6))
  lines(getCurve(7))
  
}








