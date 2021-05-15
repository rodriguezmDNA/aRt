emptyCanvas <- function(pltLimit = 5 ){
  plot(0, 0,
       xlim=c(-pltLimit,pltLimit),
       ylim=c(-pltLimit,pltLimit),
       col = "transparent", xlab = "", ylab = "", axes=F)
}




emptyCanvas(5)
subColors = colors()[!colors() %in% 'gray']
col <- sample(length(subColors),1)
for (j in seq(1,200)){
  out <- rbind(c(NA,NA))
  scalingX = j * sample(c(-1,1),1) * 0.15 #rnorm(1,mean = 0.5,sd = 0.1)
  scalingY = j * sample(c(-1,1),1) * 0.15 #rnorm(1,mean = 0.5,sd = 0.1)
  
  for (i in seq(1,10)){
    x=rnorm(1) + scalingX
    y=rnorm(1) + scalingY
    out <- rbind( out, rbind(c(x,y)) )
  }
  
  lines(out,col=subColors[col+j*2])
}




