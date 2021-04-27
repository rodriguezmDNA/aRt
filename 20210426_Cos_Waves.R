

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
