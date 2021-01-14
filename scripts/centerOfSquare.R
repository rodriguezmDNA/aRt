plot.new()



drawSQ <- function( Xor=0.5,Xwi = 0.3,Yor = 0.5,Yhe = 0.2,offX=0,offY=0){
  
  p= (Xor + Xwi - ((Xor + Xwi - Xor)/2)) + offX
  q= (Yor + Yhe - ((Yor + Yhe - Yor)/2)) + offY
  
  
  polygon(c(Xor,Xor,Xor+Xwi,Xor+Xwi)+offX,
        c(Yor,Yor+Yhe,Yor+Yhe,Yor)+offY,col='transparent')
  points(p,q)
  return(c(p,q))
}


plot.new()
for (i in seq(-1,1,.05)){
  for (j in seq(-1,1,.05)){
    drawSQ(Xwi = 0.2+j+i,Yhe = +j+i,offX = i,offY = j)  
  }
}

drawSQ(offX = 0.1)
drawSQ(offX = 0.1,offY = -0.5)


