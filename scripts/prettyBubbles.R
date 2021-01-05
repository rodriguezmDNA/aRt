# Libraries
library(ggplot2)
library(dplyr)
library(reshape)

# The dataset is provided in the gapminder library


data <- iris

# Most basic bubble plot
data %>%
  mutate(country = factor(Species),
         SepArea=Sepal.Width*Sepal.Length,
         PetArea=Petal.Length*Petal.Width) %>%
  ggplot(aes(x=Sepal.Length, y=Sepal.Width, size=SepArea, color=Species)) +
  geom_point(alpha=0.25,show.legend = FALSE) +
  geom_point(alpha=0.25,aes(y=Petal.Length, x=Petal.Width, size=PetArea, color=Species),show.legend = FALSE) +
  theme_void()


genData <- function(x) runif(500)

data <- replicate(5,genData())
colnames(data) <- letters[1:ncol(data)]
data <- tibble(data,'ids'=seq(1,nrow(data)))

tibble(data) %>% pivot_longer(!ids,cols=c(a,b))

data = anscombe %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)"
  )

data %>% mutate(z=x*y,
                w=sin(x),
                v=tan(y)) %>%
  ggplot(aes(x=x, y=y, size=z, color=set)) +
  geom_point(alpha=0.15,show.legend = FALSE) +
  geom_point(aes(x=y,y=x,size=w**2),alpha=0.75,show.legend = FALSE) +
  geom_point(aes(x=w/2,y=v*w,size=1/z**2),alpha=0.45,show.legend = FALSE) +
  geom_point(aes(x=v,y=w,size=z**2),alpha=0.45,show.legend = FALSE) +
  theme_void()  



repMe <- function(x){rep(x,9)}

ranShape <- function() sample(seq(1,24),1)
ranColor <- function() sample(colors(),1)
ranSize <- function() sample(seq(1,3),1)


shapes=replicate(90,ranShape())
colors=replicate(90,ranColor())
sizes=replicate(90,ranSize())

modify = function(x){
  y=length(x)
  repMe(y) * runif(y)}


step = function() sample(seq(0,3,0.02),1)

stepA = replicate(90,step())
stepB = replicate(90,step())


makeData <- function() {
  sapply(seq(0,9),repMe) %>% melt() %>% mutate(xA=(X1*runif(1) + stepA),
                                      xB=(X2*runif(1)) + stepB)
}

ggplot() +
  geom_point(makeData(), mapping = aes(x=xA,y=xB),shape=shapes,size=sizes,color=colors) +
  geom_point(makeData(), mapping = aes(x=xA,y=xB),shape=shapes,size=sizes,color=colors) +
  geom_point(makeData(), mapping = aes(x=xA,y=xB),shape=shapes,size=sizes,color=colors) +
  geom_point(makeData(), mapping = aes(x=xA,y=xB),shape=shapes,size=sizes,color=colors) +
  #geom_point(aes(x/2),color='blue',size=0.15,shape=11) +
  #geom_point(aes(.05 + x/2),color='blue',size=1,shape=ranShape()) +
  # geom_point(aes(x/3),color='red',size=0.15) +
  # geom_point(aes(sin(x),sin(y)),color='red',size=0.15) +
  # geom_point(aes(cos(x),cos(y)),color='pink',size=0.15,shape='*') +
  
  theme_void()


    
  

