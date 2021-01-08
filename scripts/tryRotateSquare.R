### https://math.stackexchange.com/questions/270194/how-to-find-the-vertices-angle-after-rotation

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

)

theta = 15


do.call('cbind',apply(sq,1, function(x) { rotXY(x[1],x[2])}))


rotated = t(data.frame(apply(sq,1, function(x) { rotXY(x[1],x[2])}),row.names = c('x','y')))


plot.new()
polygon(rotated[,'x'],rotated[,'y'])

originX = 0.5 #+ offX 
originY = 0.6 - .05
width = 0.3 #+ offX 
height = 0.2  - .05

    
  
plot.new()
polygon(x =sq[,'x'],
        y =sq[,'y'],
        col = "black")





rotated = t(data.frame(apply(sq,1, function(x) { rotXY(x[1],x[2])}),row.names = c('x','y')))

plot(-1, 1, col = "white", xlab = "X", ylab = "Y")
plot.new()
polygon(x =sq[,'x'],
        y =sq[,'y'],
        col = "blue")
polygon(x =c(-0.7375023,-0.8350454, -1.0629518, -0.9654086 ),
        y =c(-0.09268443,-0.20663762,-0.01155127,-0.10240192),
        col = "transparent")

apply(sq,1,function(x){x[2]-x[1]})
plot.new()
polygon(x =sq[,'x']+.05,
        y =sq[,'y']+.05,
        col = "blue")



c(originX, originX,originX+width,originX+width)
c(originY, originY+height,originY+height,originY)

sq <- data.frame(
  x = c(.5,.5,.8,.8),
  y = c(.6,.8,.8,.6)
)
plot.new()
polygon(x=sq[,'x'],
        y=sq[,'y'])


sq <- data.frame(
  x = c(1,1,5,5)/5,
  y = c(1,5,5,1)/5
)
plot.new()
polygon(x=sq[,'x'],
        y=sq[,'y'])
points(0.8,0.8)

points(-0.8,0.8,type='blue')

sq$x-sq$y



rotXY = function(x,y,p,q){
  xP = ((x-p)*cos(theta)) - ((y-q)*sin(theta)) + p
  yP = ((x-p)*sin(theta)) + ((y-q)*cos(theta)) + q
  return (c(xP,yP))
}

plot.new()
theta=.25
rotated=t(data.frame(apply(sq,1, function(x) { rotXY(x[1],x[2],p = .5,q=.5)}),row.names = c('x','y')))
polygon(x=rotated[,'x'],
        y=rotated[,'y'])
polygon(x=sq[,'x']-.9,
        y=sq[,'y'])
