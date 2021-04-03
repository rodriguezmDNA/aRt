


### https://www.quantamagazine.org/how-to-create-art-with-mathematics-20151008
### https://www.quantamagazine.org/solution-creating-art-with-mathematics-20151030
x <- function(t,a=6,b=2,c=14,d=3) { cos(t) + (cos(a*t)/b) + (sin(c*t) * 1/d) }
y <- function(t,a=6,b=2,c=14,d=3) { sin(t) + (sin(a*t)/b) + (cos(c*t) * 1/d) }



drawParamCurves <- function(){
  out <- rbind(cbind(x(0),y(0)))
  a = sample(seq(0,11),1)
  for (t in seq(0,sample(seq(9,15),1),.01)){
    out <- rbind(out,cbind(x(t,a=a),y(t,a=a)))
  }
  out <- out[-1,]
  print(paste0('a=',a))
  emptyCanvas(2)
  
  lines(out[,1],out[,2])
  
}

drawParamCurves()

