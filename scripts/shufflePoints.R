
squFunc <- function(Xcenter = 0,Ycenter = 0,wi = 0.4,he = 0.4){
  shape <- rbind(
      c(Xcenter+wi,Ycenter+he),
      c(Xcenter+wi,Ycenter-he),
      c(Xcenter-wi,Ycenter-he),
      c(Xcenter-wi,Ycenter+he))
  return (shape)
  }
      

plot(0, 0)
newSq <- squFunc(0.5,0.7)
polygon(newSq[,1],newSq[,2])


octaFunc <- function(Xcenter = 0,Ycenter = 0,wi = 0.4,he = 0.4){
  oct <- rbind(
        c(Xcenter+(wi/2),Ycenter+he),
        c(Xcenter+wi,Ycenter+(he/2)),
        c(Xcenter+wi,Ycenter-(he/2)),
        c(Xcenter+(wi/2),Ycenter-he),
        c(Xcenter-(wi/2),Ycenter-he),
        c(Xcenter-wi,Ycenter-(he/2)),
        c(Xcenter-wi,Ycenter+(he/2)),
        c(Xcenter-(wi/2),Ycenter+he))
  return (oct)
  }



plot(0, 0,
     xlim=c(-5,17),
     ylim=c(-5,17),
     col = "white", xlab = "", ylab = "", axes=F)
for (p in seq(1,15)){
  for (q in seq(1,15)) {
    oct <- octaFunc(p,q)
    oct <- oct[sample(nrow(oct)),]
    polygon(oct[,1],oct[,2])
}}


plot(0, 0,
     xlim=c(-1,17),
     ylim=c(-1,17),
     col = "white", xlab = "", ylab = "", axes=F)
for (p in seq(1,15)){
  for (q in seq(1,15)) {
    
    dec <- sample(1:2,1)
     ifelse(dec == 1, shape <- squFunc(p,q,0.3,0.3),
                              shape <- octaFunc(p,q))
    shuf <- sample(c(T,F),1)
    print(shuf)
    ifelse(shuf == T, shape[sample(nrow(shape)),],shape)
    
    polygon(shape[,1]+.2,shape[,2]+.2)
  }}


##### Variations on octagon

plot(0, 0,
     xlim=c(-5,17),
     ylim=c(-5,17),
     col = "white", xlab = "", ylab = "", axes=F)
for (p in seq(1,15)){
  for (q in seq(1,15)) {
    oct <- octaFunc(p,q)
    oct <- oct[sample(nrow(oct)),]
    polygon(oct[,1],oct[,2])
  }}


Xcenter = 0
Ycenter = 0
wi = 0.4
he = 0.4
plot(0, 0)

oct <- hexFunc()
polygon(oct[,1],oct[,2])

hexFunc <- function(Xcenter = 0,Ycenter = 0,wi = 0.4,he = 0.4){
  shape <- rbind(
    c(Xcenter,Ycenter+he),
    c(Xcenter+wi,Ycenter+(he/2)),
    c(Xcenter+wi,Ycenter),
    c(Xcenter+wi,Ycenter-(he/2)),
    c(Xcenter,Ycenter-he),
    c(Xcenter-wi,Ycenter-(he/2)),
    c(Xcenter-wi,Ycenter),
    c(Xcenter-wi,Ycenter+(he/2)))
  return (shape)
}


#### Working on a pentagon

plot(0, 0)
pentFunc <- function(Xcenter = 0,Ycenter = 0,wi = 0.4,he = 0.4){
  shape <- rbind(
    c(Xcenter,Ycenter+(he/2)),
    c(Xcenter+wi,Ycenter+(he/2)),
    c(Xcenter+wi+(wi/2),Ycenter-(he/2)),
    c(Xcenter,Ycenter-he),
    c(Xcenter-wi-(wi/2),Ycenter-(he/2)),
    c(Xcenter-wi,Ycenter+(he/2)))
  return (shape)
}
out <- pentFunc()
plot(0, 0)
polygon(out[,1],out[,2])


plot(0, 0,
     xlim=c(0,15),
     ylim=c(0,15),
     col = "white", xlab = "", ylab = "", axes=F)
for (p in seq(1,15)){
  for (q in seq(1,15)) { 
    
    choice <- sample(1:5,1)
    w = sample(seq(0.3,0.8,0.5),1)
    h = sample(seq(0.3,0.8,0.5),1)
    if (choice == 1){
      fig <-  octaFunc(p,q,w,h)
    }
    if (choice == 2){
      fig <-  pentFunc(p,q,w,h)
    }
    if (choice == 3){
      fig <-  squFunc(p,q,w,h)
    } 
    if (choice == 4){

      fig <-  hexFunc(p,q,w,h)
    }
    
    if (choice != 5){
      scramble <- sample(c(T,F),1,prob = c(0.9,0.1))
      if (scramble) { fig <- fig[sample(nrow(fig)),] }

      polygon(fig[,1],fig[,2])
    }
    else(
      points(p,q,pch=1,cex=sample(seq(0.5,3,0.5),1))
    )
  }}

plot(0, 0,
     xlim=c(0,15),
     ylim=c(0,15),
     col = "white", xlab = "", ylab = "", axes=F)

for (i in 1:15){
  for (j in 1:15){
    print(c(i,j))
  points(i,j,pch=i+j,cex=2)
}}
plot.new()
points(0,0,pch=1,cex=3)


squFuncHist <- function(Xcenter = 0,Ycenter = 0,wi = 0.4,he = 0.4,maxHist=0){
  shape <- rbind(
    c(Xcenter+wi,Ycenter+he) - runif(1,max = maxHist),
    c(Xcenter+wi,Ycenter-he) - runif(1,max = maxHist),
    c(Xcenter-wi,Ycenter-he) - runif(1,max = maxHist),
    c(Xcenter-wi,Ycenter+he) - runif(1,max = maxHist))
  return (shape)
}


plot(0, 0,
     xlim=c(-5,17),
     ylim=c(-5,17),
     col = "white", xlab = "", ylab = "", axes=F)

for (p in seq(1,15)){
  for (q in seq(1,15)) {
    fig <- squFuncHist(p,q,maxHist = runif(1,0,0.5))
    #fig <- fig[sample(nrow(fig)),]
    polygon(fig[,1],fig[,2])
  }}


plot(0, 0,
     xlim=c(0,15),
     ylim=c(0,15),
     col = "white", xlab = "", ylab = "", axes=F)

hexFuncHist <- function(Xcenter = 0,Ycenter = 0,wi = 0.4,he = 0.4,maxHist=0){
  shape <- rbind(
    c(Xcenter,Ycenter+he)  + runif(1,max = maxHist),
    c(Xcenter+wi,Ycenter+(he/2)) + runif(1,max = maxHist),
    c(Xcenter+wi,Ycenter) + runif(1,max = maxHist),
    c(Xcenter+wi,Ycenter-(he/2)) + runif(1,max = maxHist),
    c(Xcenter,Ycenter-he) + runif(1,max = maxHist),
    c(Xcenter-wi,Ycenter-(he/2)) + runif(1,max = maxHist),
    c(Xcenter-wi,Ycenter) + runif(1,max = maxHist),
    c(Xcenter-wi,Ycenter+(he/2)) + runif(1,max = maxHist) )
  return (shape)
}

pentFunc <- function(Xcenter = 0,Ycenter = 0,wi = 0.4,he = 0.4,maxHist=0){
  shape <- rbind(
    c(Xcenter,Ycenter+(he/2))  + runif(1,max = maxHist),
    c(Xcenter+wi,Ycenter+(he/2))  + runif(1,max = maxHist),
    c(Xcenter+wi+(wi/2),Ycenter-(he/2))  + runif(1,max = maxHist),
    c(Xcenter,Ycenter-he)  + runif(1,max = maxHist),
    c(Xcenter-wi-(wi/2),Ycenter-(he/2))  + runif(1,max = maxHist),
    c(Xcenter-wi,Ycenter+(he/2)) + runif(1,max = maxHist) )
  return (shape)
}


plot(0, 0,
     xlim=c(-5,17),
     ylim=c(-5,17),
     col = "white", xlab = "", ylab = "", axes=F)

for (p in seq(1,15)){
  for (q in seq(1,15)) {
    fig <- pentFunc(p,q,maxHist = runif(1,0,0.5))
    #fig <- fig[sample(nrow(fig)),]
    polygon(fig[,1],fig[,2])
  }}


plot(0, 0,
     xlim=c(0,15),
     ylim=c(0,15),
     col = "white", xlab = "", ylab = "", axes=F)
for (p in seq(1,15)){
  for (q in seq(1,15)) { 
    
    choice <- sample(1:5,1)
    w = sample(seq(0.3,0.8,0.5),1)
    h = sample(seq(0.3,0.8,0.5),1)
    if (choice == 1){
      fig <-  octaFunc(p,q,w,h)
    }
    if (choice == 2){
      fig <-  pentFunc(p,q,w,h,maxHist = 0.5)
    }
    if (choice == 3){
      fig <-  squFuncHist(p,q,w,h,maxHist = 0.5)
    } 
    if (choice == 4){
      
      fig <-  hexFuncHist(p,q,w,h,maxHist = 0.5)
    }
    
    if (choice != 5){
      scramble <- sample(c(T,F),1,prob = c(0.9,0.1))
      if (scramble) { fig <- fig[sample(nrow(fig)),] }
      
      polygon(fig[,1],fig[,2])
    }
    else(
      points(p,q,pch=1,cex=sample(seq(0.5,3,0.5),1))
    )
  }}

for (p in seq(1,15)){
  for (q in seq(1,15)) { 
    
    choice <- sample(1:5,1)
    w = sample(seq(0.3,0.8,0.5),1)
    h = sample(seq(0.3,0.8,0.5),1)
    if (choice == 1){
      fig <-  octaFunc(p,q,w,h)
    }
    if (choice == 2){
      fig <-  pentFunc(p,q,w,h,maxHist = 0.5)
    }
    if (choice == 3){
      fig <-  squFuncHist(p,q,w,h,maxHist = 0.5)
    } 
    if (choice == 4){
      
      fig <-  hexFuncHist(p,q,w,h,maxHist = 0.5)
    }
    
    if (choice != 5){
      scramble <- sample(c(T,F),1,prob = c(0.9,0.1))
      if (scramble) { fig <- fig[sample(nrow(fig)),] }
      
      polygon(fig[,1],fig[,2])
    }
    else(
      points(p,q,pch=1,cex=sample(seq(0.5,3,0.5),1))
    )
  }}
