##http://www.sineofthetimes.org/the-art-of-parametric-equations-2/
### Messing around
library(tidyverse)

newX <- function(t){  (sin(t))     }
newY <- function(t){  (cos(t))   }

#makeParametricCurves <- function(a,b,maxReps=10,step=.01){
xa = c()
ya = c()
for (t in seq(1,10,0.5)){
  xa <- c(xa,newX(t))
  ya <- c(ya,newY(t))
}
out2 <- data.frame( cbind(xa,ya))
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

