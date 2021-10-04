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

fviz_dend(x = hc_euclidea_complete, k = 4, cex = 0.6) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Linkage complete, K=4")
