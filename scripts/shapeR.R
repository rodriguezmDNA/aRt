squFuncHist <- function(Xcenter = 0,Ycenter = 0,wi = 0.4,he = 0.4,maxHist=0){
  shape <- rbind(
    c(Xcenter+wi,Ycenter+he) - runif(1,max = maxHist),
    c(Xcenter+wi,Ycenter-he) - runif(1,max = maxHist),
    c(Xcenter-wi,Ycenter-he) - runif(1,max = maxHist),
    c(Xcenter-wi,Ycenter+he) - runif(1,max = maxHist))
  return (shape)
}