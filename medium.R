# library(devtools)
# install_github("pneuvial/adjclust")

diss <- read.csv("./dissimilarity2.txt", sep = "")
n <- length(diss)
d <- as.data.frame(diss^2)
total <- sum(d)
#transform using equation
f <- function(x, i, j){
  return(-0.5*(d[i, j] - (1/n)*sum(d[i, ]) - (1/n)*sum(d[, j]) - (1/n^2)*total))
}
m <- matrix(mapply(f, diss, row(diss), col(diss)), nrow = nrow(diss))

# adjacency-constrained clustering
library("adjclust")

cluster <- function(h){
  diag(mat) <- 1
  x <- t(mat)
  d <- 5
  resP <- adjclust:::HeapHop(x, length(mat), d, 0)
  resp <- data.matrix(resP)
  resP[is.nan(resP)] = 0
  
  mod <- list(merge = resP[c(1, 2), ], height = resP[c(3), ], labels = colnames(m))
  return(mod)
}
h <- 5
hc <- cluster(h)
