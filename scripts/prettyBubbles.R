# Libraries
library(ggplot2)
library(dplyr)

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
