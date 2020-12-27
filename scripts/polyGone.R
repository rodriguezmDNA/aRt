plot(-2, 2, col = "white", xlab = "X", ylab = "Y")   





makeSquare(1,0)
makeSquare(1,0.01)
makeSquare(1,0.02)
makeSquare(1,1.5)

plot.new()
for (i in seq(0,1.5,.025)){
  for (j in seq(0,1.5,.025)){
    makeSquare(i,j)
}}


makeSquare <- function(scaleX=1,scaleY=1){
  originX = 0.5 * scaleX
  originY = 0.6 * scaleY
  width = 0.3 * scaleX
  height = 0.2 * scaleY
  
  
  polygon(x = c(originX, originX,originX+width,originX+width),                           # X-Coordinates of polygon
          y = c(originY, originY+height,originY+height,originY),                             # Y-Coordinates of polygon
          col = "transparent")
}
