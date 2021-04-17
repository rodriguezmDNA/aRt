x <- function(t,a=2,b=2) { a*sin(t) -   sin(t*b)}
y <- function(t,a=2,b=2) { a*cos(t) -   cos(t*b)}


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
