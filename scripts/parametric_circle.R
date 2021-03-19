##http://www.sineofthetimes.org/the-art-of-parametric-equations-2/
### Messing around
library(tidyverse)

newX <- function(t,center){  (sin(t)+center)    }
newY <- function(t,center){  (cos(t)+center)   }

#makeParametricCurves <- function(a,b,maxReps=10,step=.01){

makeCircle <- function(center=0,group='a'){
    xa = c()
    ya = c()
    for (t in seq(1,1000,1)){
      xa <- c(xa,newX(t,center))
      ya <- c(ya,newY(t,center))
    }
  out <- data.frame( cbind(xa,ya))
  out$group <- group
  return (out)
}

listOfCircles <- list(makeCircle(1,'a'),
                      makeCircle(3,'b'),
                      makeCircle(5,'c'))

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

