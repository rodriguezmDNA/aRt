
plot.new()

Xor = 0.5
Yor = 0.5
Yhe = 0.7
offX = -0.3
offY = -0.5

polygon(c(Xor,Xor,Xor+Xwi,Xor+Xwi),
        c(Yor,Yor+Yhe,Yor+Yhe,Yor),col='transparent')

p= (Xor + Xwi) - ( Xwi/2)
q= (Yor + Yhe) - ( Yhe/2)
points(p,q)


polygon(c(Xor,Xor,Xor+Xwi,Xor+Xwi)+offX,
        c(Yor,Yor+Yhe,Yor+Yhe,Yor)+offY,col='transparent')


drawSQ <- function( Xor=0.5,Xwi = 0.3,Yor = 0.5,Yhe = 0.2,offX=0,offY=0,centerPoint=T){
  
  p= (Xor + Xwi) - ( Xwi/2) + offX
  q= (Yor + Yhe) - ( Yhe/2) + offY
  
  polygon(c(Xor,Xor,Xor+Xwi,Xor+Xwi)+offX,
        c(Yor,Yor+Yhe,Yor+Yhe,Yor)+offY,col='transparent')
  if (centerPoint){
    points(p,q)
  }
  #return(c(p,q))
}


plot.new()
for (i in seq(-1,1,.05)){
  for (j in seq(-1,1,.05)){
    setpoint = sample(c(T,F),prob = c(0.65,0.35),size=1)
    
    drawSQ(Xwi = 0.2+j+i,
           Yhe = 0.2+j+i,offX = i,offY = j,centerPoint = setpoint)  
  }
}





plot.new()
for (i in seq(-1,1,.05)){
  for (j in seq(-1,1,.05)){
    setpoint = sample(c(T,F),prob = c(0.8,0.2),size=1)
    wi=sample(seq(0.3,1,.01),1)
    he=sample(seq(0.3,1,.01),1)
    xS = sample(seq(1,-1),1)
    yS = sample(seq(1,-1),1)
    drawSQ(Xwi = wi*j*-i,
           Yhe = he*j*i,offX = i+wi*xS,offY = j+he*yS,centerPoint = setpoint)  
  }
}

##

plot.new()
for (i in seq(-1,1,.05)){
  for (j in seq(-1,1,.05)){
    setpoint = sample(c(T,F),prob = c(0.8,0.2),size=1)
    wi=sample(seq(0.3,1,.01),1)
    he=sample(seq(0.3,1,.01),1)
    xS = sample(seq(1,-1),1)
    yS = sample(seq(1,-1),1)
    if (!setpoint){
      setpoint = sample(c(T,F),prob = c(0.8,0.2),size=1)
      drawSQ(Xwi = wi+j+i,
           Yhe = he+j+i,offX = i+wi*xS,offY = j+he*yS,centerPoint = setpoint)  
    }
  }
}

plot.new()
drawSQ(Xor = 0.5,Yor = 0.5,Xwi = 0.7)
drawSQ(Xor = 0.5,Yor = 0.5,Yhe = 0.7,offX = -0.3,offY = -0.3)
drawSQ(Xor = 0.5,Yor = 0.5,Yhe = 0.7,offX = -0.3,offY = -0.3)
