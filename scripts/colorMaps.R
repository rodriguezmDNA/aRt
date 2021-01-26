library(tidyverse)
library(ggrepel)

#rgb2hex <- function(r,g,b) sprintf('#%s',paste(as.hexmode(c(r,g,b)),collapse = ''))
## hadley wickham:
rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255) #https://gist.github.com/mbannert/e9fcfa86de3b06068c83
col2hex_alpha <- function(col, alpha) rgb(t(col2rgb(col)), alpha=alpha, maxColorValue=255)


allColors <- colors()
length(allColors)

rgbAllColors <- data.frame(do.call('rbind',lapply(allColors,function(x){t(col2rgb(x))})))
rgbAllColors$color <- allColors
rgbAllColors$hex = apply(rgbAllColors,1,function(x){rgb2hex(x[1],x[2],x[3])})

head(rgbAllColors)

rgbAllColors[490,]
baseColor <- gsub('[0-9]|alice|antique|burly|spring|slate|steel|rosy|royal|dark|
     sandy|powder|pale|medium|light|midnight|navy|blush|indian|hot|dodger|deep','',rgbAllColors$color)
rgbAllColors$basecolor <- baseColor

rgbAllColors_unique <- rgbAllColors[!duplicated(rgbAllColors$hex),]
rownames(rgbAllColors_unique) <- rgbAllColors_unique$color

View(rgbAllColors_unique)

datPCA = rgbAllColors_unique[,1:3]
rownames(datPCA) = rgbAllColors_unique[,'color']

pca_important <- prcomp(t(datPCA), center = F,scale. = F)



rgbAllColors_unique_aug <- cbind(rgbAllColors_unique,pca_important$rotation[rownames(rgbAllColors_unique),1:2])



exData = rgbAllColors_unique_aug %>% filter(color %in% sample(rownames(rgbAllColors_unique_aug),30))

customColors <- exData$color
names(customColors) <- exData$hex

dat <- rgbAllColors_unique_aug #%>% sample_n(40)
dat %>%  
  ggplot(aes(x=PC1,y=PC2)) + #,color=superclass.name)) +
  geom_point(color=dat$hex,size=26,alpha=0.15) +
  geom_text_repel(aes(label=basecolor),color=dat$hex,alpha=0.1) +
  theme_void()
  


