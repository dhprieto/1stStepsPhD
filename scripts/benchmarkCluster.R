### benchmark for clustering

library(clValid)
library(kohonen)
library(mclust)

benchmarkCluster <- function(tablaNum, numclust){comparacion <- clValid(
  obj        = tablaNum,
  nClust     = 2:10,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", 
                  "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)
return(comparacion)
}
