# CLustering

library(factoextra)
library(dplyr)
library(scales)
# Lectura de tablas

# Tablas crónico con datos antropométricos ----

c_O_A.A <- read.csv("data/cronicoOrinaAnt_Antro.csv")

# Retiramos datos ordinales y texto

c_O_A.A$Endulzante <- factor(c_O_A.A$Endulzante, levels = c("SA", "ST", "SU"))
c_O_A.A$Sexo <- factor(c_O_A.A$Sexo, levels = c("HOMBRE", "MUJER"))
c_O_A.A$Tiempo <- factor(c_O_A.A$Tiempo, levels = c("0", "Final"))


set.A <- c_O_A.A[,-c(1,2,4)]

set.A_num <- set.A[,-c(1,7,26)]



set.A_rescaled <- set.A_num %>% mutate_each_(list(~rescale(.) %>% as.vector), 
                                         vars = colnames(set.A_num))

# Validación de método de clustering ----

## Manual ----

library(purrr)
library(ggpubr)
datos_simulados <- map_df(set.A_rescaled,
                          .f = function(x){runif(n = length(x),
                                                 min = min(x),
                                                 max = max(x))
                          }
)


pca_datos_A      <- prcomp(set.A_rescaled)
pca_datos_simulados <- prcomp(datos_simulados)
p1 <- fviz_pca_ind(X = pca_datos_A, habillage = set.A$Sexo,
                   geom = "point", title = "PCA - datos sexo",
                   pallete = "jco") +
  theme_bw() + theme(legend.position = "bottom")
p2 <- fviz_pca_ind(X = pca_datos_simulados, geom = "point",
                   title = "PCA - datos simulados", pallete = "jco") +
  theme_bw() + theme(legend.position = "bottom")

ggarrange(p1, p2)#, common.legend = TRUE)

### Aplicamos clustering ----

# K-means clustering

km_datos_A <- kmeans(x = set.A_rescaled, centers = 2)
p1 <- fviz_cluster(object = km_datos_A, data = set.A_rescaled,
                   ellipse.type = "norm", geom = "point", main = "Datos sexo",
                   stand = FALSE, palette = "jco") +
  theme_bw() + theme(legend.position = "none")
km_datos_simulados <- kmeans(x = datos_simulados, centers = 2)
p2 <- fviz_cluster(object = km_datos_simulados, data = datos_simulados,
                   ellipse.type = "norm", geom = "point",
                   main = "Datos simulados", stand = FALSE, palette = "jco") +
  theme_bw() + theme(legend.position = "none")

# Hierarchical clustering
p3 <- fviz_dend(x = hclust(dist(set.A_rescaled)), k = 2, k_colors = "jco",
                show_labels = FALSE, main = "Datos sexo")
p4 <- fviz_dend(x = hclust(dist(datos_simulados)), k = 2, k_colors = "jco",
                show_labels = FALSE, main = "Datos simulados")

ggarrange(p1, p2)

ggarrange(p3, p4)

### Estadístico de Hopkins ----

library(clustertend)
set.seed(101)

hopkins(set.A_rescaled, n = nrow(set.A_rescaled)-1)
hopkins(datos_simulados, n = nrow(datos_simulados)-1)

### VAT (Comparación visual) ----

dist_datos_A      <- dist(set.A_rescaled, method = "euclidean")
dist_datos_simulados <- dist(datos_simulados, method = "euclidean")

p1 <- fviz_dist(dist.obj = dist_datos_A, show_labels = FALSE) +
  labs(title = "Datos sexo") + theme(legend.position = "bottom")
p2 <- fviz_dist(dist.obj = dist_datos_simulados, show_labels = FALSE) +
  labs(title = "Datos simulados") + theme(legend.position = "bottom")

ggarrange(p1, p2)


## Automática ----

library(clValid)

comparacion <- clValid(
  obj        = set.A_rescaled,
  nClust     = 2:6,
  clMethods  = c("hierarchical", "kmeans", "pam"),
  validation = c("stability", "internal")
)
summary(comparacion)

# Número de clusters ----

library(NbClust)

numero_clusters <- NbClust(data = scale(set.A_num), distance = "euclidean", min.nc = 2,
                           max.nc = 5, method = "kmeans", index = c("kl","ch","hartigan",  "cindex", "db"))

fviz_nbclust(numero_clusters)

# Non hierarchical clustering ----

## K-means ----

### Number of centers

fviz_nbclust(x = set.A_rescaled, FUNcluster = kmeans, method = "wss", k.max = 10, 
             diss = get_dist(set.A_rescaled, method = "pearson"), nstart = 50)

### Clustering

set.seed(123)

km_clusters <- kmeans(x = set.A_rescaled, centers = 5, nstart = 1000, )


### Plotting

fviz_cluster(object = km_clusters, data = set.A_rescaled,
             show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "K-means - Cronico Orina Antocianos + Datos Antropométricos") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


## K-medoids ----

library(cluster)

### Number of centers

fviz_nbclust(x = set.A_rescaled, FUNcluster = pam, method = "wss", k.max = 10, 
             diss = get_dist(set.A_rescaled, method = "manhattan"), nstart = 50)

### Clustering

pam_clusters <- pam(x = set.A_rescaled, k = 4, metric = "manhattan")

fviz_cluster(object = pam_clusters, data = set.A_rescaled, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "PAM - Cronico Orina Antocianos + Datos Antropométricos") +
  theme(legend.position = "none")


# Hiearchical clustering ----

# Matriz de distancias euclídeas
mat_dist <- dist(x = set.A_rescaled, method = "manhattan")
# Dendrogramas con linkage complete y average
hc_euclidea_complete <- hclust(d = mat_dist, method = "complete")
hc_euclidea_average  <- hclust(d = mat_dist, method = "average")
cor(x = mat_dist, cophenetic(hc_euclidea_complete))
cor(x = mat_dist, cophenetic(hc_euclidea_average))

set.seed(101)

fviz_dend(x = hc_euclidea_average, k = 2, cex = 0.6) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
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

dbscan::kNNdistplot(set.A_rescaled, k=5) # no acaba




# HEATMAPS ----

heatmap(x = as.matrix(set.A_rescaled), scale = "none",
        distfun = function(x){dist(x, method = "euclidean")},
        hclustfun = function(x){hclust(x, method = "average")},
        cexRow = 0.7)

library(pheatmap)
pheatmap(mat = as.matrix(set.A_rescaled), scale = "none", clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean", clustering_method = "average",
         cutree_rows = 4, fontsize = 6)
