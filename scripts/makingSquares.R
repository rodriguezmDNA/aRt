plot.new()
makeSquare()
makeSquare(.02,offsetX = .02)

sapply(seq(0,1,.02), function(x){makeSquare(x,offsetX = x)})
sapply(seq(1,0,-.02), function(x){makeSquare(x,offsetY = x)})
sapply(seq(1,0,.02), function(x){makeSquare(x,offsetY = x)})
sapply(seq(0,1,.03), function(x){makeSquare(x,offsetY = x)})
sapply(seq(0,1,.04), function(x){makeSquare(x,offsetY = x)})

plot.new()
sapply(seq(0,1,.01),function(step){ 
  sapply(seq(0,step,.01),function(x){ makeSquare(x,offsetX = x) } )
  sapply(seq(0,step,.01),function(x){ makeSquare(x,offsetX = -x) } )
  sapply(seq(0,step,.01),function(x){ makeSquare(x,offsetY = x) } )
  sapply(seq(0,step,.01),function(x){ makeSquare(x,offsetY = -x) } )
  ###
  sapply(seq(0,step,.01),function(x){ makeSquare(x,offsetX = x,offsetY = x) } )
  sapply(seq(0,step,.01),function(x){ makeSquare(x,offsetX = x,offsetY = -x) } )
  sapply(seq(0,step,.01),function(x){ makeSquare(x,offsetX = -x,offsetY = x) } )
  sapply(seq(0,step,.01),function(x){ makeSquare(x,offsetX = -x,offsetY = -x) } )
})
