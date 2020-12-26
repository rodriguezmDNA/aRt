library(tidyverse)


sinDF <- function(n,id='1'){
  df <- data.frame("x"=runif(n),'id'=id)
  df$y <- sin(df$x)
  return (df)
}

cosDF <- function(n,id='1'){
  df <- data.frame("x"=runif(n),'id'=id)
  df$y <- cos(df$x)
  return (df)
}



defTable <- function(i1,i2,id=NA){
  xRanges = getN(i1,i2)
  x = seq(xRanges[1], xRanges[2], by = 2)
  #
  yRanges = getN(i1,i2)
  y = seq(yRanges[1], yRanges[2], by = 2)
  
  cutOff <- min(c(length(x),length(y)))
  
  out <- data.frame(x=x[1:cutOff],
                    y=y[1:cutOff])
  if (!is.na(id)){
    out$id <- id
  }
  return(out)
}


getN <- function(i1,i2) {
  a=0
  b=0
  while (a==b){
    #print('fix')
    a <- sample(seq(i1,i2/2),1,replace = T)
    b <- sample(seq(i2,i2*2),1,replace = T)
  }
  out <- sort(c(a,b))
  return (out)
  }

sample(1,9999)

defTable(38,98,'a')


tabDF <- rbind(
  defTable(3,98,'a'),
  defTable(8,98,'b'),
  defTable(38,99,'c'),
  defTable(3,-1,'x')
)



ggplot(tabDF, aes(x, sin(y))) + 
  geom_point() +
  geom_line(data = tabDF) +
  theme_void()
