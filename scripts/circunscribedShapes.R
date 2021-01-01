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


makeTriangle <- function(or,wd,hg,grp=NA){
  df = data.frame(x=c(or,or+wd,or+wd),
             y=c(or,or,or+hg),
             grp=grp)
  return(df)
}

d1 = makeTriangle(1,1,1,'a')
d2 = makeTriangle(1.75,1,1,'b')
d = rbind(d1)

scl <- 0.25
d <- mutate(d, x = ifelse(grp == 'a', x*scl, x))
d <-mutate(d, y = ifelse(grp == 'b', y*scl, y))  
  
ggplot() +
  geom_polygon(d,mapping=aes(x=x, y=y,group=grp),fill='white',color='black') +
  theme_void() +
  NULL  

###
d1 = makeTriangle(1,1,1,'a')
d = rbind(d1)


scaleList <- seq(0.1,1,.05)

makeArt <- function(scaleList){

  d = list()
  for (each in rev(seq(1,length(scaleList)))) {
    #print(let)
    scl <- scaleList[each]
    group <- letters[each]
    d <- rbind(d,makeTriangle(scl,scl,scl,group))
  }
  
  ggplot() +
    geom_polygon(d,mapping=aes(x=x, y=y,group=grp),fill='transparent',color='black') +
    geom_polygon(d,mapping=aes(x=y, y=x,group=grp),fill='transparent',color='black') +
    geom_polygon(d,mapping=aes(x=x*-1, y=y*-1,group=grp),fill='transparent',color='black') +
    geom_polygon(d,mapping=aes(x=y*-1, y=x*-1,group=grp),fill='transparent',color='black') +
    # geom_polygon(d %>% mutate(y=rev(y)),
    #              mapping=aes(x=y, y=x,group=grp),fill='transparent',color='black') +
    geom_polygon(d %>% mutate(x=rev(x)),
                 mapping=aes(x=y, y=x*-1,group=grp),fill='transparent',color='black') +
    geom_polygon(d %>% mutate(x=rev(x)),
                 mapping=aes(x=y*-1, y=x,group=grp),fill='transparent',color='black') +
    
    theme_void() +
    NULL  
}  
  
  
  

scaleList <- seq(0.1,1,.05)  
makeArt(scaleList)

makeArt( seq(0,2,.05) )
makeArt( seq(-1,1,.05) )
makeArt( seq(-2,2,.05) )
