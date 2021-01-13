plot.new()


Xor <- 0.5
Xwi <- 0.3
Yor <- 0.5
Yhe <- 0.2
polygon(c(Xor,Xor,Xor+Xwi,Xor+Xwi),
        c(Yor,Yor+Yhe,Yor+Yhe,Yor),col='transparent')

p=Xor + Xwi - ((Xor + Xwi-Xor)/2)
q=Yor+Yhe - ((Yor+Yhe-Yor)/2)
points(p,q)
