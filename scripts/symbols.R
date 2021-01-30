rep = 0 
plot(0,0,col = "white", xlab = "", ylab = "", axes=F)
while (rep < 2000){
  points(sample(seq(-0.75,0.75,.01),1),
         sample(seq(-0.75,0.75,.01),1),pch = sample(seq(1,25,1),1),col=sample(colors(),1))
  rep=rep+1
}
