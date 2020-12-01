library(tidyverse)


makeSpiral <- function(a,b){
  theta <- seq(0,10*pi,0.01)
  r <- a + b*theta
  df <- data.frame(x=r*cos(theta), 
                   y=r*sin(theta)) # Cartesian coords
  
  return(df)
  }



makeSpiral(2,2) %>% head()
makeSpiral(2,500) %>% head()

ggplot(makeSpiral(200000,300000),aes(x,y)) + 
  geom_point(size=0.05,color='blue') +
  theme_void()
