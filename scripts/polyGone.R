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


offsetY <- 0.3
offsetY <- 0.3

originX = 0.5 * .01
originY = 0.6 * scaleY
width = 0.3 * scaleX 
height = 0.2 * scaleY

plot.new()
offset <- 0
polygon(x = c(.05+offset, .05+offset,.08+offset,.08+offset),                           
        y = c(.05, .08,.08,.05),                             # Y-Coordinates of polygon
        col = "transparent")

offset <- .09
polygon(x = c(.05+offset, .05+offset,.08+offset,.08+offset),                           
        y = c(.05, .08,.08,.05),                             # Y-Coordinates of polygon
        col = "transparent")

offsetX <- .18
polygon(x = c(.05+offsetX, .05+offsetX,.08+offsetX,.08+offsetX),                           
        y = c(.05, .08,.08,.05),                             # Y-Coordinates of polygon
        col = "transparent")
  
############


ranColor <- function(x) sample(colors(),1)
myCol <- function(color=True,alpha=0.25){
  ifelse(color,adjustcolor(ranColor(),alpha.f=0.25),'transparent')
}

plot.new()

width <- .03
height <- .03
origin <- 0.01
SeqoffsetX <- seq(0,1,.09)
SeqoffsetY <- seq(0,1,.09)

seq(0,0.8,.01)


for(offsetX in SeqoffsetX){
  for(offsetY in SeqoffsetY){
  ranColor <- sample(colors(),1)
  polygon(x = c(origin+offsetX, origin+offsetX, origin+width+offsetX, origin+width+offsetX),                           
          y = c(origin+offsetY, origin+height+offsetY, origin+height+offsetY, origin+offsetY),                             # Y-Coordinates of polygon
          col = myCol(F))  
}
}

