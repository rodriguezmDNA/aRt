
emptyCanvas <- function(pltLimit = 5 ){
  plot(0, 0,
       xlim=c(-pltLimit,pltLimit),
       ylim=c(-pltLimit,pltLimit),
       col = "transparent", xlab = "", ylab = "", axes=F)
}


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



## Symmetric 
emptyCanvas(100)
for (i in seq(-256,256,1)){
  for (j in seq(-256,256,1)){
    if (i!=0 & j!=0){
      if (cos(abs(i))/sin(abs(j)) >  tan(abs(i)) ){  #| (abs(i)%%abs(j) == 2)) {
        #print('True')
        points(i,j,pch='O')
      }
    }
  }}

## Mixed pattern
emptyCanvas(100)
for (i in seq(-256,0,1)){
  for (j in seq(-256,0,1)){
    if (tan(i)/cos(j) <= sin(i)){
      #print('True')
      points(i,j,pch='.')
      points(-i,-j,pch='.')
    } 
    if (sin(i)/tan(j) <= cos(i)){
      points(-i,j,pch='.')
      points(i,-j,pch='.')
    }
  }}

#### 4 conditions
emptyCanvas(100)
for (i in seq(-256,0,1)){
  for (j in seq(-256,0,1)){
    if (tan(i)/cos(j) <= sin(i)){
      #print('True')
      points(i,j,pch='.')
    } 
    if (cos(i)/tan(j) <= sin(i)){
      points(-i,j,pch='.')
    }
    if (sin(i)/cos(j) <= tan(i)){
      points(i,-j,pch='.')
    } 
    if (sin(i)/tan(j) <= cos(i)){
      points(-i,-j,pch='.')
    }
    
  }}

##### Diamond pattern
emptyCanvas(100)
for (i in seq(-256,0,1)){
  for (j in seq(-256,0,1)){
  if (sin(i)/cos(j) <= tan(i)){
    points(i,-j,pch='.')
    points(-i,-j,pch='.')
    points(i,j,pch='.')
    points(-i,j,pch='.')
  } 
  }}




##### Bar code
emptyCanvas(100)
for (i in seq(-256,0,1)){
  for (j in seq(-256,0,1)){
    if (sin(i)*cos(j) <= tan(i)){
      points(i,j,pch='.')
      points(j,i,pch='.') ##Invert to make it a criss-cross pattern
      #points(-i,-j,pch='.')
      #points(j,i,pch='.') 
      #points(-j,-i,pch='.')
    } 
  }}




##### 20210412
### Triangle - asymmetric dot matrix
emptyCanvas(100)
for (i in seq(-256,0,1)){
  for (j in seq(-256,0,1)){
    if (sin(i)*cos(j) <= tan(i)){
      points(i,j+i,pch='.')
      points(abs(j-i),i+j,pch='.') ##Invert to make it a criss-cross pattern
      #points(-i,-j,pch='.')
      #points(j,i,pch='.') 
      #points(-j,-i,pch='.')
    } 
  }}



##### 20210413
### Mirrored triangles
emptyCanvas(100)
for (i in seq(-256,0,1)){
  for (j in seq(-256,0,1)){
    if (sin(i)*cos(j) <= tan(i)){
      #points(i,j+i,pch='.')
      points(abs(j-i),i+j,pch='.') ##Invert to make it a criss-cross pattern
      points(-abs(j-i),-(i+j),pch='.') ##Invert to make it a criss-cross pattern
      
      points(i,j+i,pch='.')
      points(abs(i),abs(j+i),pch='.')
      
      
      #points(-i,-j,pch='.')
      #points(j,i,pch='.') 
      #points(-j,-i,pch='.')
    } 
  }}



##### 20210414
### GGplot lattice
out <- cbind(0,0)
emptyCanvas(100)
for (i in seq(0,256,1)){
  for (j in seq(0,256,1)){
    if (sin(i)*cos(j) <= tan(i)){
    
      out <- rbind(out,cbind(abs(j-i),i+j))
      #points(i,j+i,pch='.')
      points(abs(j-i),i+j,pch=5) ##Invert to make it a criss-cross pattern
      #points(-abs(j-i),-(i+j),pch='.') ##Invert to make it a criss-cross pattern
      
      # points(i,j+i,pch='.')
      # points(abs(i),abs(j+i),pch='.')
      
      
      #points(-i,-j,pch='.')
      #points(j,i,pch='.') 
      #points(-j,-i,pch='.')
    } 
  }}

outDF <- data.frame(out)
head(outDF)

library(ggplot2)

ggplot(outDF,aes(x=X1,y=X2)) +
  geom_point(color='white',size=0.05) +
  #theme_void()
  theme(
    axis.ticks =   element_blank(),
    axis.text =    element_blank(),
    axis.title =   element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'))
NULL


##### 20210414
### Mirrored different condition
emptyCanvas(100)
for (i in seq(-256,0,1)){
  for (j in seq(-256,0,1)){
    if (sin(i)*cos(j) <= tan(i)){
      #points(i,j+i,pch='.')
      points(abs(j-i),i+j,pch='.') ##Invert to make it a criss-cross pattern
    }
    if (sin(i)*tan(j) <=  cos(i)){
        points(-(j-i),-(abs(i+j)),pch='.') ##Invert to make it a criss-cross pattern

    } 
  }}
