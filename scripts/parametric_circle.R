##http://www.sineofthetimes.org/the-art-of-parametric-equations-2/
### Messing around
library(tidyverse)

newX <- function(t,center){  (sin(t)+center)     }
newY <- function(t,center){  (cos(t)+center)   }

#makeParametricCurves <- function(a,b,maxReps=10,step=.01){
xa = c()
ya = c()
for (t in seq(1,10,0.5)){
  xa <- c(xa,newX(t,3))
  ya <- c(ya,newY(t,3))
}
out3 <- data.frame( cbind(xa,ya))

a = cbind(out2,'a')
b = cbind(out3,'b')

data.frame(c(a,b))
#return (out2)
#}

ggplot(out2,aes(x=xa,y=ya)) +
  geom_path(color='black') +
  #theme_void()
  # theme(
  #   axis.ticks =   element_blank(),
  #   axis.text =    element_blank(),
  #   axis.title =   element_blank(),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   plot.background=element_rect(fill = "black"),
  #   panel.background = element_rect(fill = 'black'))
  NULL

