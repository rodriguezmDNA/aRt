library(tidyverse)
library(ambient) 
library(scico)
library(here)




ranWalk <- function(n){
  exp = sample(c(-5:-0.5,0.5:-5),1)
  return (tibble(
    x = runif(n, min = 0, max = 2), # uniform random number between 0 and 2
    y = sin(x**exp), # uniform random number between 0 and 2
    z = (x**5) ) )
}

palette_name <- "corkO"
ggplot(ranWalk(50)) +
  geom_line(aes(x,y,color=z),show.legend = FALSE) +
  theme_void() +
  scale_color_scico(palette = palette_name)
  NULL

