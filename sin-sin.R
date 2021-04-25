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


##### 20210421
### sin - cos
x <- function(t,a=2,b=2) { a*sin(t) -   cos(t*b)}
y <- function(t,a=2,b=2) { a*sin(t) -   cos(t*b)}
sincos <- function(a=2,b=2,newCanvas=FALSE,makePlot=FALSE){
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
for (i in seq(0,12,4)){ 
  for (j in seq(0,12,2)){
    lines(sincos(i-j,j+i))
  }}


##### 20210422
### punto de fuga
emptyCanvas(20)
for (i in seq(-10,2,4)){ 
  for (j in seq(-10,1,2)){
    lines(sincos(abs(j+i)**2,abs(i-j)**2))
  }}





##### 20210423
### tantan
x <- function(t,a=2,b=2) { a*tan(t) -   tan(t*b)}
y <- function(t,a=2,b=2) { a*tan(t) -   tan(t*b)}
tantan <- function(a=2,b=2,newCanvas=FALSE,makePlot=FALSE){
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

### tangent
emptyCanvas(20)
for (i in seq(0,10,2)){ 
    lines(tantan(0, i))
  }


##### 20210424
## limiting lines
emptyCanvas(20)
for (i in seq(0,10,2)){ 
  #lines(head(tantan(0, i),150))
  #lines(head(tantan(i, 0),150))
  lines(tail(tantan(0, i),500))
  lines(tail(tantan(i, 0),500))
}


##### 20210425
## adding color
emptyCanvas(20)
for (i in seq(0,10,2)){ 
  #lines(head(tantan(0, i),150))
  #lines(head(tantan(i, 0),150))
  color = sample(colors(),1)
  lines(tail(tantan(0, i),500),col=color)
  lines(tail(tantan(i, 0),500),col=color)
}


