
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
