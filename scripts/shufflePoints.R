The inputs are as follows:
Center X and center Y are the coordinates of the center point of the polygon. Set initially to 550, 550. Note that the y coordinate is positive downwards, to conform to the convention in most computer software. Positive x is to the right.
The number of sides. Must be greater than 2. Set initially to 5.
The radius is the distance from the center to a vertex. Set initially to 100.
Start angle is the position of the first vertex.
This angle is in degrees and is the angle starting at 3 o'clock going counter clockwise. So for example if you want the first vertex to be at 12 o'clock, set this to 90. Set initially to blank (auto).


plot(0,0)
points(0,0)
points(0,0.4)
points(0.2,0.4)
points(0.4,0.4)
points(0.4,0.2)
points(0.4,0)
points(0.4,-0.2)
points(0.4,-0.4)
points(0.2,-0.4)
points(0,-0.4)
points(-0.2,-0.4)
points(-0.4,-0.4)
points(-0.4,-0.2)
points(-0.4,0)
points(-0.4,0.2)
points(-0.4,0.4)
points(-0.2,0.4)

df <- rbind(
  #c(0,0),
  c(0,0.2),
  c(0,0.4),
  c(0.4,0.4),
  c(0.4,0),
  c(0.4,-0.4),
  c(0,-0.4),
  c(-0.4,-0.4),
  c(-0.4,0),
  c(-0.4,0.4))

polygon(df[,1],df[,2],col = 'transparent')


df2 <- rbind(
  c(0,0),
  c(0,0.4),
  c(0.2,0.4),
  c(0.4,0.4),
  c(0.4,0.2),
  c(0.4,0),
  c(0.4,-0.2),
  c(0.4,-0.4),
  c(0.2,-0.4),
  c(0,-0.4),
  c(-0.2,-0.4),
  c(-0.4,-0.4),
  c(-0.4,-0.2),
  c(-0.4,0),
  c(-0.4,0.2),
  c(-0.4,0.4),
  c(-0.2,0.4))
plot(0,0)
polygon(df2[,1],df2[,2])




cuad = c()
xCenter <- 0.2
yCenter <- 0.2
for (i in seq(-0.4-xCenter,yCenter+0.4,0.2)){ 
  for (j in seq(0.4+yCenter,-0.4-xCenter,-0.2)){
    cuad <- rbind(cuad,c(i,j))
}}

plot(0,0)
polygon(cuad[,1],cuad[,2])




rDF <- cuad[sample(nrow(cuad)),]
rDF <- rDF + 0.5
rDF <- rDF - 0.5
polygon(rDF[,1],rDF[,2])





df2 <- rbind(
  c(0,0),
  c(0,0.4),
  c(0.2,0.4),
  c(0.4,0.4),
  c(0.4,0.2),
  c(0.4,0),
  c(0.4,-0.2),
  c(0.4,-0.4),
  c(0.2,-0.4),
  c(0,-0.4),
  c(-0.2,-0.4),
  c(-0.4,-0.4),
  c(-0.4,-0.2),
  c(-0.4,0),
  c(-0.4,0.2),
  c(-0.4,0.4),
  c(-0.2,0.4))