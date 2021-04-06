
#### Repeating patterns

emptyCanvas(100)
for (i in seq(-256,256,1)){
  for (j in seq(-256,256,1)){
    if (sin(i)/cos(j) <= tan(i)){
      print('True')
      points(i,j,pch='.')
    }
  }}