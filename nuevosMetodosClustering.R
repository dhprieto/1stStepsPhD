library(factoextra)
library(ggpubr)
source("scripts/preprocess.R")

# lectura

orinaFlav <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaOrinaFlav.csv")$tablaFactors))
orinaAnt <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaorinaAnt.csv")$tablaFactors))
plasmaAnt <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaplasmaAnt.csv")$tablaFactors))
plasmaFlav <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaplasmaFlav_adjusted.csv")$tablaFactors))


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

summary(comparacionOF) # hierarchical
 
# APN           0.0431 hierarchical 3       
# AD            0.5632 kmeans       3       
# ADM           0.0725 hierarchical 3       
# FOM           0.2228 hierarchical 3       
# Connectivity 18.3302 hierarchical 3       
# Dunn          0.2251 hierarchical 3       
# Silhouette    0.3737 hierarchical 3 

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
# APN           0.0471 hierarchical 3       
# AD            0.5133 som          3       
# ADM           0.0913 hierarchical 3       
# FOM           0.2025 clara        3       
# Connectivity 13.0802 hierarchical 3       
# Dunn          0.1997 hierarchical 3       
# Silhouette    0.2766 hierarchical 3 


comparacionPF <- clValid(
  obj        = plasmaFlav %>% select(-c(IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                        Endulzante, Sexo, numVol, Tiempo)),
  nClust     = 3,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionPF) # hierarchical

# Optimal Scores:
#   
#   Score  Method       Clusters
# APN          0.2737 diana        3       
# AD           0.4165 pam          3       
# ADM          0.1359 hierarchical 3       
# FOM          0.2198 kmeans       3       
# Connectivity 7.3341 hierarchical 3       
# Dunn         0.2288 hierarchical 3       
# Silhouette   0.2770 diana        3      

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
# APN           0.1071 hierarchical 3       
# AD            0.6536 kmeans       3       
# ADM           0.1036 hierarchical 3       
# FOM           0.2177 diana        3       
# Connectivity 38.3520 hierarchical 3       
# Dunn          0.2400 hierarchical 3       
# Silhouette    0.2369 hierarchical 3       


# plasmaAnt ----

matriz_distancias <- dist(x = plasmaAnt %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                                      Endulzante, Sexo, numVol, Tiempo)), method = "euclidean")
hc_completo <- hclust(d = matriz_distancias, method = "complete")
hc_average  <- hclust(d = matriz_distancias, method = "average")
hc_single   <- hclust(d = matriz_distancias, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersPA <- cutree(hc_completo, k=3)


fviz_dend(x = hc_completo, k = 3,
          label_cols = as.numeric(plasmaAnt$Endulzante))

table(clustersPA, plasmaAnt$Endulzante, plasmaAnt$Tiempo)
table(clustersPA, plasmaAnt$Sexo)

library(ggdendro)


# colors: ST-RED, SU-GREEN, SA-BLACK


# Viejo ----


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
