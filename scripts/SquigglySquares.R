
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
xInter <- Xor +sort(sample(seq(0,.05,.001),8)) * replicate(8,plusminus())
yInter <- Yhe + sort(sample(seq(Yor,Yor+Yhe,.001),8))
polygon(c(Xor,  ,Xor),
        c(Yhe,   ,Yhe))


plusminus <- function(){
  out <- sample(c(1,-1),1)
  return(out)
  }



