
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


plot(0, 0,
     xlim=c(-5,17),
     ylim=c(-5,17),
     col = "white", xlab = "", ylab = "", axes=F)
for (p in seq(1,15)){
  for (q in seq(1,15)) {
    
    choice <- sample(c(T,F),1,prob = c(0.9,1))
    
    if (choice) { oct <-  octaFunc(p,q) ; print(choice) } else {oct <- squFunc(p,q)}
    #choice <- sample(c(T,F),1,prob = c(0.9,0.1))
    #if (choice) { oct <- oct[sample(nrow(oct)),] }
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
pent <- rbind(
  c(Xcenter,Ycenter+(he/2)),
  c(Xcenter+wi,Ycenter+(he/2)),
  c(Xcenter+wi+(wi/2),Ycenter-(he/2)),
  c(Xcenter,Ycenter-he),
  c(Xcenter-wi-(wi/2),Ycenter-(he/2)),
  c(Xcenter-wi,Ycenter+(he/2)))

polygon(pent[,1],pent[,2])
