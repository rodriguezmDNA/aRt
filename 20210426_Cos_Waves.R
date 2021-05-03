

##### 20210426
## cos waves
emptyCanvas()
for (i in seq(-20,20,.1)){
  #points(i,cos(i),pch='.')
  for (j in seq(-10,10,.1)){
  points(i+j,cos(i)+j,pch='.')
}}


##### 20210427
### tri-trigo
emptyCanvas(10)
for (i in seq(-10,10,1)){
  for (j in seq(-10,10,.1)){
    points(j,sin(j)+i,pch='.',col='blue')
    points(j,cos(j)+i,pch='.',col='red')
    points(j,tan(j)+i,pch='.',col='black')
  }
}


##### 20210428
### Lines - sincos
emptyCanvas(10)
for (i in seq(-10,10,1)){
  sinF <- rbind(cbind(0,0))
  cosF <- rbind(cbind(0,0))
  for (j in seq(-10,10,.1)){
    sinF <- rbind(sinF,cbind(i+j,sin(j)+i))
    cosF <- rbind(cosF,cbind(i+j,cos(j)+i))
  }
  lines(sinF[-1,]+i)
  lines(cosF[-1,]+i)
}


##### 20210429
### grid and waves
emptyCanvas(10)
for (i in seq(-10,10,1)){
  sinF <- rbind(cbind(0,0))
  cosF <- rbind(cbind(0,0))
  hLine <- rbind(cbind(0,0))
  vLine <- rbind(cbind(0,0))
  for (j in seq(-10,10,.1)){
    sinF <- rbind(sinF,cbind(i+j,sin(j)+i))
    cosF <- rbind(cosF,cbind(i+j,cos(j)+i))
    vLine <- rbind(vLine,cbind(i,j))
    hLine <- rbind(hLine,cbind(j,i))
  }
  lines(sinF[-1,]+i)
  lines(cosF[-1,]+i)
  lines(hLine[-1,]+i)
  lines(vLine[-1,]+i)
}


##### 20210430
### wavy cos-sin sin-cos
emptyCanvas(10)
for (i in seq(-10,10,1)){
  sinF <- rbind(cbind(0,0))
  cosF <- rbind(cbind(0,0))
  hLine <- rbind(cbind(0,0))
  vLine <- rbind(cbind(0,0))
  for (j in seq(-10,10,.1)){
    sinF <- rbind(sinF,cbind(cos(j+i)+j,sin(j)+i))
    cosF <- rbind(cosF,cbind(sin(j+i)+j,cos(j)+i))
  }
  lines(sinF[-1,])
  lines(cosF[-1,])
}

### ribbon
emptyCanvas(10)
for (i in seq(-10,10,1)){
  sinF <- rbind(cbind(0,0))
  cosF <- rbind(cbind(0,0))
  hLine <- rbind(cbind(0,0))
  vLine <- rbind(cbind(0,0))
  for (j in seq(-10,10,.1)){
    sinF <- rbind(sinF,cbind(cos(j+i)+j,sin(j)))
    cosF <- rbind(cosF,cbind(sin(j+i)+j,cos(j)))
  }
  lines(sinF[-1,])
  lines(cosF[-1,])
}

##### 20210501
###  repeat ribbons
ribbon <- function(offset=0){
  for (i in seq(-10,10,1)){
    sinF <- rbind(cbind(0,0))
    cosF <- rbind(cbind(0,0))
    hLine <- rbind(cbind(0,0))
    vLine <- rbind(cbind(0,0))
    for (j in seq(-10,10,.5)){
      sinF <- rbind(sinF, cbind( cos(j+i)+j , sin(j)  ))
      cosF <- rbind(cosF, cbind( sin(j+i)+j , cos(j)  ))
    }
    lines(sinF[-1,] + offset )
    lines(cosF[-1,] - offset)
  }
}


emptyCanvas(10)

for (i in seq(-10,10,2.5)){
  print(i)
  ribbon(i)
}
