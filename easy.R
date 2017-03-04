#install.packages("rioja")
library(rioja)

plotChclust <- function(diss){
  # method = coniss
  diss <- as.dist(diss)
  clust <- chclust(diss, method = "coniss")
  plot(clust, hang=-1, sub = "Method: coniss")
  
  # method = conslink
  clust <- chclust(diss, method = "conslink")
  plot(clust, hang=-1, sub = "Method: conslink")
}

diss <- read.csv("./dissimilarity.txt", sep = "")
plotChclust(diss)