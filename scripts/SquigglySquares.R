
plot.new()
polygon(seq(Xor,Xor+Xwi,.01),
        seq(Yor,Yor+Yhe,.01))


plot.new()
Xor = 0.5
Yor = 0.5
Yhe = 0.5

polygon(c(Xor,Xor+.05,Xor,Xor+Xwi,Xor+Xwi),
        c(Yor,Yor+Yhe/2,Yor+Yhe,Yor+Yhe,Yor),col='transparent')


plot.new()
xInter <- sort(Xor + c(sample(seq(0+.0001,.05,.001),8)) * replicate(8,plusminus()))
yInter <- sort(Yhe + c(sample(seq(Yor+.0001,Yor+Yhe,.001),8)))
polygon(sort(c(Xor,xInter)),
        sort(c(Yor,yInter)))

plot.new()

polygon(c(.05,.05),
        c(.05,07))

plot.new()
yInter <- seq(0.05,7-.01,.01)
xInter <- rep(.05,length(yInter)) * runif(length(yInter),-1,1)
polygon(c(xInter,yInter,xInter+.5,yInter),
        c(yInter,xInter,yInter,xInter+1))


