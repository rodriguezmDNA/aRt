##http://www.sineofthetimes.org/the-art-of-parametric-equations-2/

library(tidyverse)
newX <- function(t,a,b,c){  (cos(a*t)) + ((cos(b*t))/2)  + ((sin(c*t))/3)      }
newY <- function(t,a,b,c){  (sin(a*t)) + ((sin(b*t))/2)  + ((cos(c*t))/3)      }

xa = c()
ya = c()
a=4
b=3
c=3
for (t in seq(1,10,.01)){
  xa <- c(xa,newX(t,a,b,c))
  ya <- c(ya,newY(t,a,b,c))
}
pltLimit <- 3
plot(0, 0,
     xlim=c(-pltLimit,pltLimit),
     ylim=c(-pltLimit,pltLimit),
     col = "transparent", xlab = "", ylab = "", axes=F)
out2 <- data.frame( cbind(xa,ya))
lines(out2)
head(out2)
