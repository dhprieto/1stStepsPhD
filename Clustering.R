# CLustering

library(factoextra)


# Lectura de tablas

# Tablas crónico con datos antropométricos ----

c_O_A.A <- read.csv("data/cronicoOrinaAnt_Antro.csv")

# Retiramos datos ordinales y texto

c_O_A.A$Endulzante <- factor(c_O_A.A$Endulzante, levels = c("SA", "ST", "SU"))
c_O_A.A$Sexo <- factor(c_O_A.A$Sexo, levels = c("HOMBRE", "MUJER"))
c_O_A.A$Tiempo <- factor(c_O_A.A$Tiempo, levels = c("0", "Final"))

set.A <- c_O_A.A[,-c(1,2,4)]

set.A_num <- set.A[,-c(1,7,26)]

# Non hierarchical clustering ----

## K-means ----

### Number of centers

fviz_nbclust(x = set.A_num, FUNcluster = kmeans, method = "wss", k.max = 10, 
             diss = get_dist(set.A_num), nstart = 50)

### Clustering

set.seed(123)

km_clusters <- kmeans(x = set.A_num, centers = 5, nstart = 1000)


### Plotting

fviz_cluster(object = km_clusters, data = set.A_num,
             show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "K-means - Cronico Orina Antocianos + Datos Antropométricos") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

