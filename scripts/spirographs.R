library(tidyverse)
valX <- function(t,a,b){ ( (a-b) * cos(t) ) + b*cos(t* ((a/b)-1))}
valY <- function(t,a,b){ ( (a-b) * sin(t) ) + b*sin(t* ((a/b)-1))}

x = c()
y = c()
a=65.5
b=-30
for (t in seq(2,300,2.5)){
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

  
