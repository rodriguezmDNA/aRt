##http://www.sineofthetimes.org/the-art-of-parametric-equations-2/
### Messing around
library(tidyverse)
newX <- function(t,a,b){  (cos(a*t)) + ((cos(b*t))/2)    }
newY <- function(t,a,b){  (sin(a*t)) + ((sin(b*t))/2)       }


a=1
b=5
xa = c()
ya = c()
for (t in seq(1,1000,1)){
  xa <- c(xa,newX(t,a,b))
  ya <- c(ya,newY(t,a,b))
}

pltLimit <- max(max(xa),max(ya)) 
plot(0, 0,
     xlim=c(-pltLimit,pltLimit),
     ylim=c(-pltLimit,pltLimit),
     col = "transparent", xlab = "", ylab = "", axes=F)
out2 <- data.frame( cbind(xa,ya))
lines(out2)
head(out2)

# ggplot(out2,aes(x=xa,y=ya)) +
#   geom_path(color='lightgray') +
#   theme(
#     axis.ticks =   element_blank(),
#     axis.text =    element_blank(),
#     axis.title =   element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.background=element_rect(fill = "black"),
#     panel.background = element_rect(fill = 'black'))
