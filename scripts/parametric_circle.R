##http://www.sineofthetimes.org/the-art-of-parametric-equations-2/
### Messing around
library(tidyverse)

newX <- function(t,Xcenter){  (sin(t)+Xcenter)    }
newY <- function(t,Ycenter){  (cos(t)+Ycenter)   }

#makeParametricCurves <- function(a,b,maxReps=10,step=.01){

makeCircle <- function(Xcenter=0,Ycenter=0,group='a'){
    xa = c()
    ya = c()
    for (t in seq(1,10,.05)){
      xa <- c(xa,newX(t,Xcenter))
      ya <- c(ya,newY(t,Ycenter))
    }
  out <- data.frame( cbind(xa,ya))
  out$group <- group
  return (out)
}

listOfCircles = list()
seq = 1
for (i in seq(0,4)){
  for (j in seq(0,4)){
    grp = letters[seq]
    seq = seq + 1
    listOfCircles[[grp]] = makeCircle(i,j,grp)
}}
length(listOfCircles)
names(listOfCircles)

out2 = do.call('rbind',listOfCircles)
tail(out2)

emptyCanvas <- function(pltLimit = 5 ){
  plot(0, 0,
       xlim=c(-pltLimit,pltLimit),
       ylim=c(-pltLimit,pltLimit),
       col = "transparent", xlab = "", ylab = "", axes=F)
}
emptyCanvas()
for (name in names(listOfCircles)){
  lines(out2[out2['group'] == name,])
}


ggplot(out2,aes(x=xa,y=ya)) +
  geom_path(aes(size=group)) +
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

