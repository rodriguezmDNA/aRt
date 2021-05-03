## https://cdr6934.medium.com/how-i-used-excel-to-create-abstract-album-artwork-fee740d4414f
library(tidyverse)
valX <- function(t,a,b){ ( (a-b) * cos(t) ) + b*cos(t* ((a/b)-1))}
valY <- function(t,a,b){ ( (a-b) * sin(t) ) + b*sin(t* ((a/b)-1))}


drawSpirograph <- function(a=4,b=15,step=1){
  
  x = c()
  y = c()
  for (t in seq(1,100,step)){
    x <- c(x,valX(t,a,b))
    y <- c(y,valY(t,a,b))
  }
  out <- data.frame( cbind(x,y))
  
  spiro <- ggplot(out,aes(x=x,y=y)) +
    geom_path(color='lightgray') +
    theme(
      axis.ticks =   element_blank(),
      axis.text =    element_blank(),
      axis.title =   element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background=element_rect(fill = "black"),
      panel.background = element_rect(fill = 'black'))
  print(spiro)
  return(out)
}

#### 20210501
## logo 618 design studio 
drawSpirograph(4,15) 
### Also can be drawin with lines
tmp <- drawSpirograph(4,-4)
emptyCanvas(12)
lines(tmp)

#### 20210502
## New parameters for next post
### Also can be drawing with lines
newSpiro <- drawSpirograph(15,11,.01) 
emptyCanvas(20)
lines(newSpiro)


#### 20210503
### spirospirospiro
newSpiro <- drawSpirograph(15,11,.01) 
newSpiro2 <- drawSpirograph(11,15,.01)
newSpiro3 <- drawSpirograph(4,5,.01) 
emptyCanvas(20)
lines(newSpiro)
lines(newSpiro2)
lines(newSpiro3)

ggplot() +
  geom_path(newSpiro,mapping = aes(x,y),color='lightgray') +
  geom_path(newSpiro2,mapping = aes(x,y),color='lightgray') +
  geom_path(newSpiro3,mapping = aes(x,y),color='lightgray') +
    theme(
    axis.ticks =   element_blank(),
    axis.text =    element_blank(),
    axis.title =   element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'))
