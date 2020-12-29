library(grid)

plot(1, 1, col = "white", xlab = "X", ylab = "Y")
## Circle
grid.circle(r = 0.1,x = 0.575,y=0.45)

#### Square
polygon(x = c(0.8, 0.8, 1.2,  1.2),
        y = c(0.8, 1.0, 1.0, 0.8),    
        col = "transparent")
## Rhomb
polygon(x = c(1.0, 0.8, 1.0, 1.2),                           
        y = c(0.8, 0.9, 1.0, 0.9),                             
        col = "transparent")    

## Acute line
polygon(x = c(0.8, 1.2),                           
        y = c(0.8, 1.0),                             
        col = "transparent")

## Grave line
polygon(x = c(0.8, 1.2),                           
        y = c(1.0, 0.8),                             
        col = "transparent")


###
library(ggplot2)

d=data.frame(x=c(1,2,2, 3,4,4),
             y=c(1,1,2, 2,2,3),
             t=c('a', 'a', 'a',  'b', 'b', 'b'), r=c(1,2,3, 4,5,6))
ggplot() +
  geom_polygon(data=d, mapping=aes(x=x, y=y,group=t)) +
  NULL  
