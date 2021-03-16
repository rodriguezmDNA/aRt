### https://github.com/marcusvolz/mathart/blob/master/plots/rapidly_exploring_random_tree.png

rapidly_exploring_random_tree <- function(n = 1000, X = 1000, delta = 2.5) {
  # Set up data frames for points and edges
  points <- data.frame(x = numeric(n), y = numeric(n))
  points[1, ] <- runif(2, 0, X)
  edges <- data.frame(x = numeric(n), y = numeric(n), xend = numeric(n), yend = numeric(n))
  edges[1, ] <- c(as.numeric(points[1, ]), as.numeric(points[1, ]))
  
  # Main loop
  i <- 2
  while(i <= n) {
    valid <- FALSE
    while(!valid) {
      # Sample a random point
      rp <- runif(2, 0, X)
      # Find the nearest neighbour to rp
      temp <- points[1:(i-1), ] %>%
        mutate(dist = sqrt((rp[1] - x)^2 + (rp[2] - y)^2)) %>%
        arrange(dist)
      np <- as.numeric(temp[1, c("x", "y")])
      # Limit the maximum edge length
      if(temp$dist[1] > delta) {
        rp2 <- np + (rp - np) / temp$dist[1] * delta
        rp <- rp2
      }
      # Check if the line segment between rp and np intersects an existing edge
      temp2 <- edges[1:(i-1), ] %>%
        mutate(intersects = does_intersect(rp, np, c(x, y), c(xend, yend)))
      if(sum(temp2$intersects) <= 0) {
        points[i, ] <- rp
        edges[i, ] <- c(np, rp)
        valid <- TRUE
      }
    }
    i <- i + 1
    print(paste0("Iteration: ", i, " of ", n))
  }
  edges
}


#' Does intersect
#'
#' Determine if two line segments, ab and cd, intersect
#' @param a Numeric vector of length 2 containin Point a (x, y) co-ordinates
#' @param b Numeric vector of length 2 containin Point b (x, y) co-ordinates
#' @param c Numeric vector of length 2 containin Point c (x, y) co-ordinates
#' @param d Numeric vector of length 2 containin Point d (x, y) co-ordinates
#' @param smidgin Tolerance parameter used when checking if the intersection is in the interior of both line segments
#' @keywords euclidean line segment intersect
#' @export
#' @examples
#' does_intersect()

does_intersect <- function(a, b, c, d, smidgin = 1e-6) {
  if(identical(a, b) |
     identical(a, c) |
     identical(a, d) |
     identical(b, c) |
     identical(b, d) |
     identical(c, d)) {
    return(FALSE)
  }
  
  x1 <- a[1]
  y1 <- a[2]
  x2 <- b[1]
  y2 <- b[2]
  x3 <- c[1]
  y3 <- c[2]
  x4 <- d[1]
  y4 <- d[2]
  
  i <- c(((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) /
           ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)),
         ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) /
           ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)))
  
  ifelse(sum(is.na(i)) <= 0 &
           (i[1] > (min(a[1], b[1]) + smidgin) & i[1] < (max(a[1], b[1]) - smidgin)) & (i[1] > (min(a[1], b[1]) + smidgin) & i[1] < (max(a[1], b[1]) - smidgin)) &
           (i[2] > (min(a[2], b[2]) + smidgin) & i[2] < (max(a[2], b[2]) - smidgin)) & (i[2] > (min(a[2], b[2]) + smidgin) & i[2] < (max(a[2], b[2]) - smidgin)) &
           (i[1] > (min(c[1], d[1]) + smidgin) & i[1] < (max(c[1], d[1]) - smidgin)) & (i[1] > (min(c[1], d[1]) + smidgin) & i[1] < (max(c[1], d[1]) - smidgin)) &
           (i[2] > (min(c[2], d[2]) + smidgin) & i[2] < (max(c[2], d[2]) - smidgin)), TRUE, FALSE)
}

set.seed(1)
df <- rapidly_exploring_random_tree(100) %>% mutate(id = 1:nrow(.))

# Create plot
p <- ggplot() +
  geom_segment(aes(x, y, xend = xend, yend = yend, size = -id, alpha = -id), df, lineend = "round",color='white') +
  coord_equal() +
  scale_size_continuous(range = c(0.1, 0.75)) +
  scale_alpha_continuous(range = c(0.1, 1)) +
    theme(legend.position = 'none',
      axis.ticks =   element_blank(),
      axis.text =    element_blank(),
      axis.title =   element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background=element_rect(fill = "black"),
      panel.background = element_rect(fill = 'black'))
p  
