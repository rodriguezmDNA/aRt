##http://www.sineofthetimes.org/the-art-of-parametric-equations-2/
### Messing around
library(tidyverse)

newX <- function(t,a,b){  (cos(a*t)) + ((cos(b*t))/2)     }
newY <- function(t,a,b){  (sin(a*t)) + ((sin(b*t))/2)     }

makeParametricCurves <- function(a,b,maxReps=10,step=.01){
  xa = c()
  ya = c()
  for (t in seq(1,maxReps,step)){
    xa <- c(xa,newX(t,a,b))
    ya <- c(ya,newY(t,a,b))
  }
  out2 <- data.frame( cbind(xa,ya))
  return (out2)
}



emptyCanvas <- function(pltLimit = 5 ){
  plot(0, 0,
     xlim=c(-pltLimit,pltLimit),
     ylim=c(-pltLimit,pltLimit),
     col = "transparent", xlab = "", ylab = "", axes=F)
}
emptyCanvas()
lines(makeParametricCurves(0,10))

emptyCanvas()

for (i in seq(1,2)){
  for (j in seq(1,8)){
    lines(makeParametricCurves(i,j))
    lines(makeParametricCurves(i,j)*-1)
  }}

emptyCanvas()
lines(makeParametricCurves(1,1))
lines(makeParametricCurves(1,2))
lines(makeParametricCurves(1,2)* -1) 



# ggplot(out2,aes(x=xa,y=ya)) +
#   geom_path(color='lightgray') +
#   theme(
#     axis.ticks =   element_blank(),
#     axis.text =    element_blank(),
#     axis.title =   element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.background=element_rect(fill = "black"),
#     panel.background = element_rect(fill = 'black'))
