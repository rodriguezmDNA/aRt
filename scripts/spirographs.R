## https://cdr6934.medium.com/how-i-used-excel-to-create-abstract-album-artwork-fee740d4414f
library(tidyverse)
valX <- function(t,a,b){ ( (a-b) * cos(t) ) + b*cos(t* ((a/b)-1))}
valY <- function(t,a,b){ ( (a-b) * sin(t) ) + b*sin(t* ((a/b)-1))}

x = c()
y = c()

a=4
b=15
for (t in seq(1,100,1)){
  x <- c(x,valX(t,a,b))
  y <- c(y,valY(t,a,b))
}

pltLimit <- 83
plot(0, 0,
     xlim=c(-pltLimit,pltLimit),
     ylim=c(-pltLimit,pltLimit),
     col = "transparent", xlab = "", ylab = "", axes=F)
out <- data.frame( cbind(x,y))
lines(out)
head(out)

ggplot(out,aes(x=x,y=y)) +
  geom_path(color='lightgray') +
  theme(
    axis.ticks =   element_blank(),
    axis.text =    element_blank(),
    axis.title =   element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'))

  
