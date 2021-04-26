

##### 20210426
## cos waves
emptyCanvas()
for (i in seq(-20,20,.1)){
  #points(i,cos(i),pch='.')
  for (j in seq(-10,10,.1)){
  points(i+j,cos(i)+j,pch='.')
}}
