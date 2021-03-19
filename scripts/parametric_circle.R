##http://www.sineofthetimes.org/the-art-of-parametric-equations-2/
### Messing around
library(tidyverse)

newX <- function(t,Xcenter){  (sin(t)+Xcenter)    }
newY <- function(t,Ycenter){  (cos(t)+Ycenter)   }

#makeParametricCurves <- function(a,b,maxReps=10,step=.01){

makeCircle <- function(Xcenter=0,Ycenter=0,group='a'){
    xa = c()
    ya = c()
    for (t in seq(1,1000,1)){
      xa <- c(xa,newX(t,Xcenter))
      ya <- c(ya,newY(t,Ycenter))
    }
  out <- data.frame( cbind(xa,ya))
  out$group <- group
  return (out)
}

listOfCircles <- list(makeCircle(0,0,'a'),
                      makeCircle(0,3,'b'),
                      makeCircle(3,0,'c'),
                      makeCircle(3,3,'d'))

out2 = do.call('rbind',listOfCircles)
tail(out2)

ggplot(out2,aes(x=xa,y=ya)) +
  geom_path(aes(color=group)) +
  #theme_void()
  theme(
    axis.ticks =   element_blank(),
    axis.text =    element_blank(),
    axis.title =   element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'))
  NULL

