# CLustering

library(tidyverse)
library(factoextra)
library(ggpubr)
library(scales)
source("scripts/reading.R")


# Lectura de tablas

# Tablas crónico con datos antropométricos ----

listaTablas <- preprocessTables("data/", "tablaOrinaAnt.csv")$tablaFactors

for (i in colnames(listaTablas)) {
  
  if (is.numeric(listaTablas[,i])){
    
    listaTablas <- listaTablas[!listaTablas[, i] %in% boxplot.stats(listaTablas[,i])$out,]
  }
} 

tablaNum <- listaTablas  %>% 
  dplyr::select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, Endulzante, Sexo, numVol, Tiempo))


# Validación de método de clustering ----

## Manual ----

datos_simulados <- map_df(tablaNum,
                          .f = function(x){runif(n = length(x),
                                                 min = min(x),
                                                 max = max(x))
                          }
)


pca_datos_A      <- prcomp(tablaNum)
pca_datos_simulados <- prcomp(datos_simulados)

p1 <- fviz_pca_ind(X = pca_datos_A, habillage = listaTablas$Sexo,
                   geom = "point", title = "PCA - datos sexo",
                   pallete = "jco") +
  theme_bw() + theme(legend.position = "bottom")
p2 <- fviz_pca_ind(X = pca_datos_simulados, geom = "point",
                   title = "PCA - datos simulados", pallete = "jco") +
  theme_bw() + theme(legend.position = "bottom")

ggarrange(p1, p2)#, common.legend = TRUE)

### Aplicamos clustering ----

# K-means clustering

km_datos_A <- kmeans(x = tablaNum, centers = 2)
p1 <- fviz_cluster(object = km_datos_A, data = tablaNum,
                   ellipse.type = "norm", geom = "point", main = "Datos sexo",
                   stand = FALSE, palette = "jco") +
  theme_bw() + theme(legend.position = "none")
km_datos_simulados <- kmeans(x = datos_simulados, centers = 2)
p2 <- fviz_cluster(object = km_datos_simulados, data = datos_simulados,
                   ellipse.type = "norm", geom = "point",
                   main = "Datos simulados", stand = FALSE, palette = "jco") +
  theme_bw() + theme(legend.position = "none")

# Hierarchical clustering
p3 <- fviz_dend(x = hclust(dist(tablaNum)), k = 2, k_colors = "jco",
                show_labels = FALSE, main = "Datos sexo")
p4 <- fviz_dend(x = hclust(dist(datos_simulados)), k = 2, k_colors = "jco",
                show_labels = FALSE, main = "Datos simulados")

ggarrange(p1, p2)

ggarrange(p3, p4)

### Estadístico de Hopkins ----

library(clustertend)
set.seed(101)

hopkins(tablaNum, n = nrow(tablaNum)-1)
hopkins(datos_simulados, n = nrow(datos_simulados)-1)

### VAT (Comparación visual) ----

dist_datos_A      <- dist(tablaNum, method = "euclidean")
dist_datos_simulados <- dist(datos_simulados, method = "euclidean")

p1 <- fviz_dist(dist.obj = dist_datos_A, show_labels = FALSE) +
  labs(title = "Datos sexo") + theme(legend.position = "bottom")
p2 <- fviz_dist(dist.obj = dist_datos_simulados, show_labels = FALSE) +
  labs(title = "Datos simulados") + theme(legend.position = "bottom")

ggarrange(p1, p2)


## Automática ----

library(clValid)
library("kohonen")
comparacion <- clValid(
  obj        = tablaNum,
  nClust     = c(2,3,4,5,6),
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)
summary(comparacion)

# Número de clusters ----

library(NbClust)

numero_clusters <- NbClust(data = tablaNum, distance = "euclidean", min.nc = 2,
                           max.nc = 5, method = "kmeans", index = c("kl","ch","hartigan", "cindex", "db"))

fviz_nbclust(numero_clusters)

# Non hierarchical clustering ----

## K-means ----

### Number of centers

fviz_nbclust(x = tablaNum, FUNcluster = kmeans, method = "wss", k.max = 10, 
             diss = get_dist(tablaNum, method = "pearson"), nstart = 50)

### Clustering

set.seed(123)

km_clusters <- kmeans(x = tablaNum, centers = 3, nstart = 1000)


### Plotting

fviz_cluster(object = km_clusters, data = tablaNum,
             show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "K-means - Cronico Orina Antocianos + Datos Antropométricos") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


## K-medoids ----

library(cluster)

### Number of centers

fviz_nbclust(x = tablaNum, FUNcluster = pam, method = "wss", k.max = 10, 
             diss = get_dist(tablaNum, method = "manhattan"), nstart = 50)

### Clustering

pam_clusters <- pam(x = tablaNum, k = 4, metric = "manhattan")

fviz_cluster(object = pam_clusters, data = tablaNum, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "PAM - Cronico Orina Antocianos + Datos Antropométricos") +
  theme(legend.position = "none")


# Hiearchical clustering ----

# Matriz de distancias euclídeas

mat_dist <- dist(x = tablaNum, method = "manhattan")
# Dendrogramas con linkage complete y average
hc_euclidea_complete <- hclust(d = mat_dist, method = "complete")
hc_euclidea_average  <- hclust(d = mat_dist, method = "average")
cor(x = mat_dist, cophenetic(hc_euclidea_complete))
cor(x = mat_dist, cophenetic(hc_euclidea_average))

set.seed(101)

fviz_dend(x = hc_euclidea_average, k = 2, cex = 0.6) +
       labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Linkage average, K=2")

library("igraph")
set.seed(101)
fviz_dend(x = hc_euclidea_average,
          k = 5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          cex = 0.8,
          type = "phylogenic",
          repel = TRUE)


# Otros métodos ----

## DBSCAN ----

library(fpc)
library(dbscan)
library(Rcpp)

dbscan::kNNdistplot(tablaNum, k=5) # no acaba


set.seed(321)

# DBSCAN con epsilon = 0.15 y minPts = 5
dbscan_cluster <- fpc::dbscan(data = tablaNum, eps = 0.06, MinPts = 5)

# Resultados de la asignación
head(dbscan_cluster$cluster)

fviz_cluster(object = dbscan_cluster, data = tablaNum, stand = FALSE,
             geom = "point", ellipse = FALSE, show.clust.cent = FALSE,
             pallete = "jco") +
  theme_bw() +
  theme(legend.position = "bottom")



##  SOM ----

somObject <- som(as.matrix(tablaNum), somgrid(4,4,"hexagonal"))

par(mfrow = c(1,2) )
plot(somObject, type="codes")
plot(somObject, type="mapping", col = listaTablas$Sexo, pch = 19)



# HEATMAPS ----

heatmap(x = as.matrix(tablaNum), scale = "none",
        distfun = function(x){dist(x, method = "euclidean")},
        hclustfun = function(x){hclust(x, method = "average")},
        cexRow = 0.7)

library(pheatmap)
pheatmap(mat = as.matrix(tablaNum), scale = "none", clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean", clustering_method = "average",
         cutree_rows = 4, fontsize = 6)
