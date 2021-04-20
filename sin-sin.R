x <- function(t,a=2,b=2) { a*sin(t) -   sin(t*b)}
y <- function(t,a=2,b=2) { a*cos(t) -   cos(t*b)}

emptyCanvas <- function(pltLimit = 5 ){
  plot(0, 0,
       xlim=c(-pltLimit,pltLimit),
       ylim=c(-pltLimit,pltLimit),
       col = "transparent", xlab = "", ylab = "", axes=F)
}

##### 20210417
### tear shape
emptyCanvas(20)
out <- rbind(cbind(x(0),y(0)))
for (t in seq(0,10,.01)){
  
  xP <- x(t,4)
  yP <- y(t,15)
  out <- rbind(out,cbind(xP,yP))
  points(xP,yP,pch='.')
}
out <- out[-1,]
out <- data.frame(out)
colnames(out) <- c('X','Y')
ggplot(out,aes(x=X,y=Y)) +
  geom_path() #+




##### 20210418
### Make into a function, generate different curves 
sinsin <- function(a=2,b=2,newCanvas=FALSE,makePlot=FALSE){
  out <- rbind(cbind(x(0),y(0)))
  if (newCanvas) {emptyCanvas()}
  for (t in seq(0,10,.01)){
    xP <- x(t,a)
    yP <- y(t,b)
    out <- rbind(out,cbind(xP,yP))
    if (makePlot) {lines(xP,yP,pch='.')}
    
  }
  out <- out[-1,]
  out <- data.frame(out)
  colnames(out) <- c('X','Y')
return(out)
}

emptyCanvas(20)
for (i in seq(1,10,4)){
  for (j in seq(1,10,2)){
    emptyCanvas(20)
    lines(sinsin(j,i))
}}


##### 20210419
### Concentric circles
emptyCanvas(20)
for (i in seq(-10,2,4)){ 
  for (j in seq(-10,1,2)){
    lines(sinsin(j+i,i+j))
  }}


##### 20210420
### Groovy
emptyCanvas(20)
for (i in seq(-10,2,4)){ 
  for (j in seq(-10,1,2)){
    lines(sinsin(abs(j-i),-abs(i-j)))
  }}

### Jupiter and beyond
emptyCanvas(20)
for (i in seq(-10,2,4)){ 
  for (j in seq(-10,1,2)){
    lines(sinsin(j+i,(i*i/(j+i)) ))
  }}
