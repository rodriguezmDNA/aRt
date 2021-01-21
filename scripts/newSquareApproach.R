xWave <- seq.int(1:10)
yWave <- seq.int(1:10)


xCentre <- 1
yCentre <- 1

makeSquare2 <- function(xCentre,yCenter){
  lt <- c(xCentre - 0.4,yCentre - 0.4)
  rt <- c(xCentre + 0.4,yCentre - 0.4)
  rb <- c(xCentre + 0.4,yCentre + 0.4)
  lb <- c(xCentre - 0.4,yCentre + 0.4)
  shape <- rbind(lt,rt,rb,lb)
  return (shape)
}

new_shape_end <- rbind(rt,rb,lb,lt)
new_shape <- cbind(new_shape_start,new_shape_end)

plot(0, 0,
     xlim=c(1,10),
     ylim=c(1,10),
     col = "white", xlab = "", ylab = "", axes=F)
for (i in seq(1,10)){
  for (j in seq(1,10)) {
  
    tmp <- makeSquare2(i,j)
    polygon(tmp[,1],tmp[,2]+j)  
}}



