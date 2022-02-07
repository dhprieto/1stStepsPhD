library(tidyverse)
library(factoextra)
library(ggpubr)
library(scales)
source("scripts/reading.R")


# Lectura de tablas

# Tablas crónico con datos antropométricos ----

listaTablas <- preprocessTables("data/", "tablaPlasmaAnt.csv")$tablaFactors

for (i in colnames(listaTablas)) {
  
  if (is.numeric(listaTablas[,i])){
    
    listaTablas <- listaTablas[!listaTablas[, i] %in% boxplot.stats(listaTablas[,i])$out,]
  }
} 

tablaNumMet <- listaTablas  %>% 
  dplyr::select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, Endulzante, Sexo, numVol, Tiempo))

plasmaFlav <- tablaNumMet


# tutorial SOM

library(cluster)
library(lattice)
library(kohonen)

pruebaSOM <- som(as.matrix(tablaNumMet), somgrid(4,4, "hexagonal")) 
plot(pruebaSOM, type = "codes")
plot(pruebaSOM, type = "mapping", col = listaTablas$Endulzante, pch=19)
plot(pruebaSOM, type = "mapping", col = listaTablas$Sexo, pch=19)


## comparación Kmeans

library(NbClust)
numClusters <- NbClust(tablaNumMet, distance = "euclidean", 
                       method = "kmeans",
                       index = "alllong")


pruebaKmeans <- kmeans(tablaNumMet, 3)

plot(pruebaSOM, type = "mapping", col = pruebaKmeans$cluster, pch=19)

fviz_cluster(object = pruebaKmeans, data = tablaNumMet, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")


# prueba ClValid

library(clValid)
library(mclust)

comparacion <- clValid(
  obj        = plasmaFlav,
  nClust     = 3,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacion)

# PlasmaAnt DIANA
# orinaAnt hierarchical
# orinaFlav sota/kmeans
# plasmaFlav(+Peso) clara/sota

# plasmaAnt ----

library(dendextend)
library(RColorBrewer)

# 3 clkusters

fviz_nbclust(plasmaAnt, hcut, hc_func = "diana")

dianaPlasmaAnt <- diana(plasmaAnt)

dianaPlasmaAnt%>%
  set(what="labels_color",as.numeric(listaTablas$Endulzante)) %>%
  pltree(main="Plasma Antocianos")



rect.hclust(dianaPlasmaAnt, k = 3, border = 2:10) %>%
  

clusterPLasmaANt <- cutree(dianaPlasmaAnt, k = 3)


tableEdulcorantePF <- table(listaTablas$Endulzante)


fviz_dend(x = dianaPlasmaAnt,
          k=3,
          label_cols = as.numeric(listaTablas$Endulzante))
legend("topleft", legend = c("ST", "SU", "SA"), 
       col = as.numeric(listaTablas$Endulzante))

table(clusterPLasmaANt, listaTablas$Endulzante)
table(clusterPLasmaANt, listaTablas$Sexo)


# tenemos un grupo grande que contiene

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
