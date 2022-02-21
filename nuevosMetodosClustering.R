library(factoextra)
library(ggpubr)
source("scripts/preprocess.R")

# lectura

orinaFlav <- removeOutliers(preprocessTables("data/", "tablaOrinaFlav.csv")$tablaFactors)
orinaAnt <- removeOutliers(preprocessTables("data/", "tablaorinaAnt.csv")$tablaFactors)
plasmaAnt <- removeOutliers(preprocessTables("data/", "tablaplasmaAnt.csv")$tablaFactors)
plasmaFlav <- removeOutliers(preprocessTables("data/", "tablaplasmaFlav_adjusted.csv")$tablaFactors)


# prueba ClValid

library(clValid)
library(mclust)
library(kohonen)

comparacionOF <- clValid(
  obj        = orinaFlav %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                Endulzante, Sexo, numVol, Tiempo)),
  nClust     = 3,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionOF) # som
 
# Optimal Scores:
#   
#   Score  Method       Clusters
# APN          0.1499 sota         3       
# AD           0.1179 som          3       
# ADM          0.0274 som          3       
# FOM          0.0480 sota         3       
# Connectivity 7.9952 hierarchical 3       
# Dunn         0.2201 hierarchical 3       
# Silhouette   0.4086 som          3  

comparacionOA <- clValid(
  obj        = orinaAnt %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                      Endulzante, Sexo, numVol, Tiempo)),
  nClust     = 3,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionOA) # hierarchical

# Optimal Scores:
#   
#   Score   Method       Clusters
# APN           0.1127 diana        3       
# AD            0.0931 pam          3       
# ADM           0.0175 hierarchical 3       
# FOM           0.0360 model        3       
# Connectivity 16.5925 hierarchical 3       
# Dunn          0.1596 hierarchical 3       
# Silhouette    0.4082 hierarchical 3   


comparacionPF <- clValid(
  obj        = plasmaFlav %>% select(-c(IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                        Endulzante, Sexo, numVol, Tiempo)),
  nClust     = 3,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionPF) # PAM

# Optimal Scores:
#   
#   Score   Method       Clusters
# APN           0.2064 hierarchical 3       
# AD            0.3295 sota         3       
# ADM           0.1297 sota         3       
# FOM           0.1766 pam          3       
# Connectivity 18.8262 hierarchical 3       
# Dunn          0.1068 clara        3       
# Silhouette    0.3251 clara        3       

comparacionPA <- clValid(
  obj        = plasmaAnt %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                       Endulzante, Sexo, numVol, Tiempo)),
  nClust     = 3,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionPA) # kmeans
 
# Optimal Scores:
#   
#   Score   Method       Clusters
# APN           0.1589 diana        3       
# AD            0.3581 som          3       
# ADM           0.0687 sota         3       
# FOM           0.1204 som          3       
# Connectivity 40.5036 hierarchical 3       
# Dunn          0.1496 diana        3       
# Silhouette    0.2637 diana        3       

# PlasmaAnt diana
# orinaAnt hierarchical
# orinaFlav som
# plasmaFlav(+Peso) pam

# plasmaAnt ----

library(dendextend)
library(RColorBrewer)

# diana

fviz_nbclust(plasmaAnt %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                 Endulzante, Sexo, numVol, Tiempo)), hcut, hc_func = "diana")

dianaPlasmaAnt <- diana(plasmaAnt)

clusterPLasmaANt <- cutree(dianaPlasmaAnt, k = 3)

fviz_dend(x = dianaPlasmaAnt,
          label_cols = as.numeric(plasmaAnt$Endulzante))

table(clusterPLasmaANt, plasmaAnt$Endulzante, plasmaAnt$Tiempo)
table(clusterPLasmaANt, plasmaAnt$Sexo)


# discordancia de grupos y plot

# orinaAnt ----


orinaAnt <- preprocessTables("data/", "tablaOrinaAnt.csv")$tablaFactors

for (i in colnames(orinaAnt)) {
  
  if (is.numeric(orinaAnt[,i])){
    
    orinaAnt <- orinaAnt[!orinaAnt[, i] %in% boxplot.stats(orinaAnt[,i])$out,]
  }
} 

tablaNumMet <- orinaAnt  %>% 
  dplyr::select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, Endulzante, Sexo, numVol, Tiempo))

matrizDist <- dist(tablaNumMet, method= "euclidean")
 
set.seed(123)


hc_euclidea_completo <- hclust(d = matrizDist, method = "complete")

library(NbClust)
numClusters <- NbClust(tablaNumMet, distance = "euclidean", 
                       method = "complete",
                       index = "alllong")

# 3

hc_euclidea_single   <- hclust(d = matrizDist, method = "single")



numClusters <- NbClust(tablaNumMet, distance = "euclidean", 
                       method = "single",
                       index = "alllong")
## 2

hc_euclidea_average  <- hclust(d = matrizDist, method = "average")


numClusters <- NbClust(tablaNumMet, distance = "euclidean", 
                       method = "average",
                       index = "alllong")

# 2


fviz_dend(x = hc_euclidea_completo,
          k=3,
          label_cols = as.numeric(orinaAnt$Sexo))

fviz_dend(x = hc_euclidea_single,
          k=2,
          label_cols = as.numeric(orinaAnt$Sexo))

fviz_dend(x = hc_euclidea_average,
          k=2,
          label_cols = as.numeric(orinaAnt$Sexo))


# orinaFlav ----

## kmeans ----
orinaFlav <- preprocessTables("data/", "tablaOrinaFlav.csv")$tablaFactors

for (i in colnames(orinaFlav)) {
  
  if (is.numeric(orinaFlav[,i])){
    
    orinaFlav <- orinaFlav[!orinaFlav[, i] %in% boxplot.stats(orinaFlav[,i])$out,]
  }
} 

tablaNumMet <- orinaFlav  %>% 
  dplyr::select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, Endulzante, Sexo, numVol, Tiempo))


numClusters <- NbClust(tablaNumMet, distance = "euclidean", 
                       method = "kmeans",
                       index = "alllong")
# 3 

kmeansOrinaFlav <- kmeans(tablaNumMet, 3)

fviz_cluster(kmeansOrinaFlav, data = tablaNumMet,show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE)

table(kmeansOrinaFlav$cluster, orinaFlav$Endulzante)

table(kmeansOrinaFlav$cluster, orinaFlav$Sexo)


# plasmaFlav


plasmaFlav <- preprocessTables("data/", "tablaPlasmaFlav_adjusted.csv")$tablaFactors

for (i in colnames(plasmaFlav)) {
  
  if (is.numeric(plasmaFlav[,i])){
    
    plasmaFlav <- plasmaFlav[!plasmaFlav[, i] %in% boxplot.stats(plasmaFlav[,i])$out,]
  }
} 

tablaNumMet <- plasmaFlav  %>% 
  dplyr::select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, Endulzante, Sexo, numVol, Tiempo))

numclusters <- NbClust(tablaNumMet, distance = "euclidean", 
        method = "centroid",
        index = "alllong")

# 2 

clustersPlasmaFlav <- pam(tablaNumMet, 2)
fviz_cluster (clustersPlasmaFlav, data = tablaNumMet)

table(clustersPlasmaFlav$cluster, plasmaFlav$Endulzante)

table(clustersPlasmaFlav$cluster, plasmaFlav$Sexo)

### ----
