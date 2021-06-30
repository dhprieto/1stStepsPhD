### Clustering pacientes + modelo

library(factoextra)

orinaFlav <- read.csv("data/cronicoOrinaFlavLimpio.csv", sep = ";", dec = ",")

orinaFlav[is.na(orinaFlav)]<-0

# tabla <- stack(as.data.frame(orinaFlav[,-1]))
# tabla$Condiciones <- rep(orinaFlav$X, 8) 
orinaFlav$Endulzante <- rep("SA", nrow(orinaFlav))
orinaFlav$Tiempo <- rep("0", nrow(orinaFlav))

# A = Estevia B = sucralosa C = sacarosa

for (i in seq(1, nrow(orinaFlav))){
    
  if (grepl(pattern = "A", x = orinaFlav$X[i])){
    orinaFlav$Endulzante[i] <- "ST"
  }
  else if (grepl(pattern = "B", x = orinaFlav$X[i])){
    orinaFlav$Endulzante[i] <- "SU"
  }
  
}

for (i in seq(1, nrow(orinaFlav))){
  if (grepl(pattern = "F", x = orinaFlav$X[i])){
    orinaFlav$Tiempo[i] <- "Final"
  }
}

orinaFlav


# Non hierarchical clustering ----

## K-means

### Number of centers

fviz_nbclust(x = orinaFlav[,c(2,3,4,5,6,7,8)], FUNcluster = kmeans, method = "wss", k.max = 10, 
             diss = get_dist(orinaFlav[,c(2,3,4,5,6,7,8)]), nstart = 50)

### Plotting

set.seed(123)

km_clusters <- kmeans(x = orinaFlav[,c(2,3,4,5,6,7,8)], centers = 3, nstart = 50)

fviz_cluster(object = km_clusters, data = orinaFlav[,c(2,3,4,5,6,7,8)],
             show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Clustering K-means - Regulados positivamente X-A") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))



## K-medoids clustering (PAM)

### Number of clusters

library(cluster)
fviz_nbclust(x =  orinaFlav[,c(2,3,4,5,6,7,8)], FUNcluster = pam, method = "wss", k.max = 10,
             diss = dist( orinaFlav[,c(2,3,4,5,6,7,8)], method = "manhattan"))

set.seed(123)
pam_clusters <- pam(x =  orinaFlav[,c(2,3,4,5,6,7,8)], k = 5, metric = "manhattan")

fviz_cluster(object = pam_clusters, data =  orinaFlav[,c(2,3,4,5,6,7,8)], ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Clustering PAM - Regulados positivamente X-A") +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))

# Hierarchical clustering 

## Hierarchical K-means

# Number of centers

set.seed(123)
hc_euclidea_completo <- hclust(d = dist(x =  orinaFlav[,c(2,3,4,5,6,7,8)], method = "euclidean"),
                               method = "complete")
fviz_dend(x = hc_euclidea_completo, cex = 0.5, main = "Clustering jer?rquico por linkage completo - Regulados positivamente X-A",
          sub = "Distancia eucl?dea", k= 4, rect = TRUE) +
  theme(plot.title =  element_text(hjust = 0.5, size = 15))

# Plotting

hkmeans_cluster <- hkmeans(x =  orinaFlav[,c(2,3,4,5,6,7,8)], 
                           hc.metric = "euclidean",
                           hc.method = "complete", k = 3)

fviz_cluster(object = hkmeans_cluster, pallete = "jco", repel = TRUE) +
  theme_bw() + labs(title = "K-means Jerarquico - Regulados positivamente X-GL")+  theme(plot.title =  element_text(hjust = 0.5, size = 15))

# Hierarchical clustering

mat_dist <- dist(x =  orinaFlav[,c(2,3,4,5,6,7,8)], method = "euclidean")

# Dendrograms with linkage complete y average

hc_euclidea_complete <- hclust(d = mat_dist, method = "complete")
hc_euclidea_average  <- hclust(d = mat_dist, method = "average")
cor(x = mat_dist, cophenetic(hc_euclidea_complete))
cor(x = mat_dist, cophenetic(hc_euclidea_average))

plot(hc_euclidea_average)

plot(hc_euclidea_complete)



