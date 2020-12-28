

width <- .03
height <- .03
origin <- 0.01
SeqoffsetX <- seq(0,1,.09)
SeqoffsetY <- seq(0,1,.09)

plot(1, 1, col = "white", xlab = "X", ylab = "Y")   
####### Rhombus
polygon(x = c(1.0, 0.8, 1.0, 1.2),                           
        y = c(1.2, 1, 0.8, 1),                             
        col = "transparent")    

####### Right arrowhead
polygon(x = c(0.8, 0.8, 0.8, 1.2),                          
        y = c(1.2, 1, 0.8, 1),                             
        col = "transparent")

####### Left arrowhead
polygon(x = c(1.2, 1.2, 1.2, 0.8),                          
        y = c(1.2, 1, 0.8, 1),                            
        col = "transparent")

####### Two parallelograms
plot(1, 1, col = "white", xlab = "X", ylab = "Y")   
polygon(x = c(1.2, 1.2, 0.8, 0.8),                           
        y = c(1.2, 1, 0.8, 1),                             
        col = "transparent")

polygon(x = c(0.8, 0.8, 1.2, 1.2),                           
        y = c(1.2, 1, 0.8, 1),                             
        col = "transparent")


####### Rectangle
plot(1, 1, col = "white", xlab = "X", ylab = "Y")   
polygon(x = c(0.8, 0.8,1.2, 1.2),                           # X-Coordinates of polygon
        y = c(0.8, 1,0.8, 1),                             # Y-Coordinates of polygon
        col = "transparent")

################

drawSquare <- function(origin=0,height=0.03,width=0.03,offsetX=0,offsetY=0) {
  polygon(x = c(origin+offsetX, origin+offsetX, origin+width+offsetX, origin+width+offsetX),                           
          y = c(origin+offsetY, origin+height+offsetY, origin+height+offsetY, origin+offsetY))
}

drawRhomb <- function(origin=0,height=0.03,width=0.03,offsetX=0,offsetY=0) {
  polygon(x = c(origin+offsetX, origin-width+offsetX, origin+offsetX, origin+width+offsetX),                           
          y = c(origin+height+offsetY, origin+offsetY, origin-height+offsetY, origin+offsetY))
}  

graveButterfly <- function(origin=0,height=0.03,width=0.03,offsetX=0,offsetY=0) {
  polygon(x = c(origin+offsetX, origin+offsetX, origin+width+offsetX, origin+width+offsetX),                           
          y = c(origin+offsetY, origin+height+offsetY, origin-height+offsetY, origin+offsetY))
}

acuteButterfly <- function(origin=0,height=0.03,width=0.03,offsetX=0,offsetY=0) {
  polygon(x = c(origin+offsetX, origin+offsetX, origin+width+offsetX, origin+width+offsetX),                           
          y = c(origin+offsetY, origin-height+offsetY, origin+height+offsetY, origin+offsetY))
}


drawAcuteParallel <- function(origin=0,height=0.03,width=0.03,offsetX=0,offsetY=0) {
  polygon(x = c(origin+offsetX, origin+offsetX, origin+width+offsetX,origin+width+offsetX),                           
          y = c(origin+offsetY, origin-height+offsetY, origin+offsetY, origin+height+offsetY))
}

drawGraveParallel <- function(origin=0,height=0.03,width=0.03,offsetX=0,offsetY=0) {
  polygon(x = c(origin+offsetX, origin+offsetX, origin+width+offsetX,origin+width+offsetX),                           
          y = c(origin+offsetY, origin+height+offsetY, origin+offsetY, origin-height+offsetY))
}




####### Rhombus
polygon(x = c(1.0, 0.8, 1.0, 1.2),                           
        y = c(1.2, 1, 0.8, 1),                             
        col = "transparent")    

plot.new()

for (offY in seq(0,0.5,.04)){
  for (offX in seq(0,0.5,.04)){
    drawSquare(offsetX = offX,offsetY = offY)
  }
}

for (offY in seq(0.5+.04,1,.04)){
  for (offX in seq(0.5+.04,1,.04)){
    drawRhomb(width = 0.03,offsetX = offX,offsetY = offY)
  }
}


library(grid)
plot.new()
for (offY in seq(0.5+.04,1,.04)){
  for (offX in seq(0,0.5,.04)){
    grid.circle(r = .025,x = offX,y=offY)
  }
}

for (offY in seq(0.5+.04,1,.04)){
  for (offX in seq(0,0.5,.04)){
    grid.circle(r = .015,x = offX,y=offY)
  }
}

plot.new()
for (offY in seq(0,0.5,.04)){
  for (offX in seq(0.5+.04,1,.04)){
    drawAcuteParallel(offsetX = offX+.03,offsetY = offY)
    drawGraveParallel(offsetX = offX+.03,offsetY = offY)
  }
}





