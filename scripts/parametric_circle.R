##http://www.sineofthetimes.org/the-art-of-parametric-equations-2/
### Messing around
library(tidyverse)

newX <- function(t,Xcenter){  (sin(t)+Xcenter)    }
newY <- function(t,Ycenter){  (cos(t)+Ycenter)   }

#makeParametricCurves <- function(a,b,maxReps=10,step=.01){

makeCircle <- function(Xcenter=0,Ycenter=0,group='a',max=10,step=.01){
    xa = c()
    ya = c()
    for (t in seq(1,max,step)){
      xa <- c(xa,newX(t,Xcenter))
      ya <- c(ya,newY(t,Ycenter))
    }
  out <- data.frame( cbind(xa,ya))
  out$group <- group
  return (out)
}




#### Disappearing circles
emptyCanvas(10)
for (i in seq(-10,10,2)){
  for (j in seq(-10,10,2)){
  points(makeCircle(i+2,j+2,max=abs(i)+abs(j)+1,step = .5),pch=".")
}}

#### Shapes
emptyCanvas(10)
for (i in seq(-10,10,2)){
  for (j in seq(-10,10,2)){
    lines(makeCircle(i+2,j+2,max=abs(i)+abs(j)+1,step = abs(i)+.1*abs(j)),pch=".")
  }}


#### Shapes
emptyCanvas(10)
for (i in seq(-10,10,2)){
  for (j in seq(-10,10,2)){
    lines(makeCircle(i+2,j+2,max=1+((abs(j))/abs(i)),step = .05),pch=".")
  }}


#### Disappearing
emptyCanvas(20)
for (i in seq(15,1,-1)){
  for (j in seq(15,1,-1)){
    lines(makeCircle(i+2,j+2,max=1+((abs(j))/abs(i)),step = .05),pch=".")
  }}

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



ggplot(makeCircle(max=10,step=.01),aes(x=xa,y=ya)) +
  geom_path(aes(size=group)) #+
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

