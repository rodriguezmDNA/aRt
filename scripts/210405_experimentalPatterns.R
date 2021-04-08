
#### Repeating patterns

emptyCanvas(100)
for (i in seq(-256,256,1)){
  for (j in seq(-256,256,1)){
    if (sin(i)/cos(j) <= tan(i)){
      print('True')
      points(i,j,pch='.')
    }
  }}


### Dot matrix pattern
dev.off()
emptyCanvas(100)
for (i in seq(-256,256,1)){
  for (j in seq(-256,256,1)){
    if (tan(i)/cos(j) <= sin(i)){
      #print('True')
      points(i,j,pch='.')
    }
  }}



dev.off()
emptyCanvas(100)
for (i in seq(0,256,1)){
  for (j in seq(0,256,1)){
    if (tan(i)*cos(j) >= cos(i)){
      #print('True')
      points(i,j,pch='.')
    }
  }}

for (i in seq(-256,0,1)){
  for (j in seq(-256,0,1)){
    if (tan(i)/cos(j) <= sin(i)){
      #print('True')
      points(i,j,pch='.')
    }
  }}


for (i in seq(-256,0,1)){
  for (j in seq(-256,0,1)){
    if (tan(i)*cos(j) >= cos(i)){
      #print('True')
      points(i,j,pch='.')
    }
  }}


#### Perspective
emptyCanvas(100)
for (i in seq(-256,256,1)){
  for (j in seq(-256,256,1)){
    if (i!=0 & j!=0){
      if (abs(i)%%abs(j) == 0){
        #print('True')
        points(i,j,pch='.')
      }
    }
  }}


## Symmetric 
emptyCanvas(100)
for (i in seq(0,256,1)){
  for (j in seq(0,256,1)){
    if (i!=0 & j!=0){
      if (abs(i)%%abs(j) > 5 ){  #| (abs(i)%%abs(j) == 2)) {
        #print('True')
        points(i,j,pch='.')
        points(-i,-j,pch='.')
        points(-i,j,pch='.')
        points(i,-j,pch='.')
      }
    }
  }}



## Symmetric 
emptyCanvas(100)
for (i in seq(0,256,1)){
  for (j in seq(0,256,1)){
    if (i!=0 & j!=0){
      if (abs(i)%%abs(j) > 5 ){  #| (abs(i)%%abs(j) == 2)) {
        #print('True')
        points(i,j,pch='.')
        points(-i,-j,pch='.')
        points(-i,j,pch='.')
        points(i,-j,pch='.')
      }
    }
  }}

