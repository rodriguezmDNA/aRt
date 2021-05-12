library(tidyverse)

##### 20210509
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

png('~/Desktop/618studiodesign/20210509_cossin.png', pointsize=10, height=4000, width=3200, res=600)
dotmatrix(size=20,step=.2)
dev.off()


##### 20210510
dotmatrix <- function(size=256,step=10,
                      printDot=FALSE,
                      ggprint=TRUE,returnValues=FALSE,
                      operation = function(i) {cos(i)/tan(j) <= sin(i)}) {
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

png('~/Desktop/618studiodesign/20210510_chess45.png', pointsize=10, height=4000, width=3200, res=600)
dotmatrix(size=20,step=.2)
dev.off()



##### 20210511
dotmatrix <- function(size=256,step=10,
                      printDot=FALSE,
                      ggprint=TRUE,returnValues=FALSE,
                      operation = function(i) {sin(i)*tan(j) <= cos(i)/tan(j)}) {
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

png('~/Desktop/618studiodesign/20210511_ratios.png', pointsize=10, height=4000, width=3200, res=600)
dotmatrix(size=20,step=.2)
dev.off()
