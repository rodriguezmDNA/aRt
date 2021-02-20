library(tidyverse)


allColors <- tibble(colors=colors())

dim(allColors)

crgb <- col2rgb(cc <- colors())
colnames(crgb) <- cc
rgbCols <- data.frame(t(crgb))
# Add names
rgbCols <- tibble(rgbCols,name=colors())




rgbCols$parent <- sapply(rgbCols$name,function(x){gsub('[0-9]','',x)})
length(unique(rgbCols$parent))
