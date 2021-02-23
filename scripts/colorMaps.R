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

rgbAllColors$basecolor <- ifelse(rgbAllColors$basecolor == '',rgbAllColors$color,rgbAllColors$basecolor)


rgbAllColors_unique <- rgbAllColors[!duplicated(rgbAllColors$hex),]
rownames(rgbAllColors_unique) <- rgbAllColors_unique$color

View(rgbAllColors_unique)

datPCA = rgbAllColors_unique[,1:3]
rownames(datPCA) = rgbAllColors_unique[,'color']

pca_important <- prcomp(t(datPCA), center = F,scale. = F,rank. = 5)


#### Distance based on PCA
############################################################
dist_colors = dist(pca_important$rotation,diag = T,upper = T,method = 'manhattan')
dist_colors <- data.frame(as.matrix(dist_colors))
dist_colors <- tibble('color'=rownames(dist_colors),dist_colors)
dim(dist_colors)

dist_colors[0:10,0:10]
### Fix redundancy
dist_colors[upper.tri(dist_colors)] <- NA
dist_colors[0:10,0:10]

library(reshape)

adj_mat = dist_colors %>% pivot_longer(cols = -color)
adj_mat <- adj_mat %>% filter(!is.na(value))


adj_mat <- adj_mat %>% left_join(rgbAllColors_unique %>% select(color,hex,basecolor),by = c('color'='color')) %>% 
  left_join(rgbAllColors_unique %>% select(color,hex,basecolor),by = c('name'='color'))

colnames(adj_mat) <- c('source','target','distance','source_hex','source_base','target_hex','target_base')
dim(adj_mat)
head(adj_mat)

adj_mat_filt <- adj_mat %>% filter(distance > 0.3)
dim(adj_mat_filt)


write.table(adj_mat,file = '~/Desktop/aRt/networks/color_networks_distance/colorsnetwork.txt',quote = F,sep = '\t',row.names = F)
write.table(adj_mat_filt,file = '~/Desktop/aRt/networks/color_networks_distance/colorsnetwork_filter.txt',quote = F,sep = '\t',row.names = F)
write.table(rgbAllColors_unique,file = '~/Desktop/aRt/networks/color_networks_distance/metacolors.txt',quote = F,sep = '\t')


adj_mat  %>% group_by(source_base) %>% summarise(count=n()) %>% 
  ungroup() %>% arrange(desc(count)) %>% top_n(10) %>% select(1) %>% unlist

adj_mat  %>% group_by(target_base) %>% summarise(count=n()) %>% 
  ungroup() %>% arrange(desc(count)) %>% top_n(10) %>% select(1) %>% unlist



degreeColor =adj_mat %>% group_by(target) %>% summarise(n=n())

####



rgbAllColors_unique_aug <- cbind(rgbAllColors_unique,pca_important$rotation[rownames(rgbAllColors_unique),1:3])


####
exData = rgbAllColors_unique_aug %>% filter(color %in% sample(rownames(rgbAllColors_unique_aug),30))

customColors <- exData$color
names(customColors) <- exData$hex

dat <- rgbAllColors_unique_aug #%>% sample_n(40)
dat %>%  
  ggplot(aes(x=PC2,y=PC3)) + #,color=superclass.name)) +
  geom_point(color=dat$hex,size=26,alpha=0.15) +
  geom_text_repel(aes(label=basecolor),color=dat$hex,alpha=9) +
  theme_void()
  
######### K-means
library(class)
##https://afit-r.github.io/kmeans_clustering

knnData <- rgbAllColors_unique

head(knnData)
head(knnData[,1:3])


length(unique(knnData$basecolor))

km.res <- kmeans(knnData[,1:3], 60, nstart = 25)

head(km.res$centers)


