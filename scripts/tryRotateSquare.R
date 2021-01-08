

plot.new()
for (offX in seq(-2,1,.01)){
  #for (offY in seq(-2,1,.01)){
    originX = 0.5 + offX 
    originY = 0.6 - .05
    width = 0.3 + offX 
    height = 0.2  - .05
    
    polygon(x = c(originX, originX,originX+width,originX+width),                           # X-Coordinates of polygon
            y = c(originY, originY+height,originY+height,originY),                             # Y-Coordinates of polygon
            col = "transparent")
  #}
}


