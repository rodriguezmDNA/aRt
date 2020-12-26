
library(tidyverse)

makeY <- function(){
  ix <- sample(seq(1,length(x)),1)
  x1 <- x[ix]
  x2 <- x[ix + sample(c(1,-1),1)]
  y = (x1**sample(1:5,1)+x2**sample(1:5,1)) * (sample(c(1,-1),1))  
  return (tibble(c(x1,y)))
}


x <- seq(-5,5,0.05)
x <- x[x != 0]

generatePoints <- function(){
  XY <- replicate(500,makeY())
  XY <- data.frame(do.call('rbind',XY)) %>% tibble()
  colnames(XY) <- c('X','Y')  
  return (XY)
}


dat = generatePoints()

ggplot(data = generatePoints()) +
  geom_point(aes(X,Y),size=0.5) +
  theme_void()

