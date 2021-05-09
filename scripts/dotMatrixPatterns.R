


sincos <- function(i) {sin(i)/cos(j) <= tan(i)}
cossin <- function(i) {cos(i)/sin(j) <= tan(i)}

dotmatrix <- function(size=256,step=10,
                      printDot=FALSE,
                      ggprint=TRUE,returnValues=FALSE,
                      operation = function(i) {cos(i)/sin(j) <= tan(i)}) {
  out <- rbind(c(NA,NA))
  if (printDot) {emptyCanvas()}
  
  for (i in seq(-size,size,step)){
    for (j in seq(-size,size,step)){
      if (operation(i)){
        #print('True')
        out <- rbind(out,rbind(c(i,j)))
        if (printDot) {points(i,j,pch='.')}
      }
    }}
  
  outDF <- data.frame(out)
  colnames(outDF) <- c('x','y')
  
  if (ggprint){
    
    ggout <- ggplot(outDF[-1,],aes(x=x,y=y)) +
      geom_point(color='white',size=0.05) +
      theme(
        axis.ticks =   element_blank(),
        axis.text =    element_blank(),
        axis.title =   element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background=element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'))
      NULL
    print(ggout)
    
    }
  if (returnValues){
    return (outDF)
  }
  
}


dotmatrix(size=256,step=2)
dotmatrix(size=256,step=2,operation = function(i) {cos(i)/sin(j) <= tan(i)})



dotmatrix(size=200,step=2,condition = function(i){ tan(i)/sin(j) <= cos(i)} )

