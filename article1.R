library(dplyr)
library(scales)
library(purrr)
library(ggpubr)
library(factoextra)
library(GGally)
library(plotly)
#library(clustertend)

preprocessTablas <- function(root, nombreTabla) {
  
  # Getting data ready
  rootTabla <- paste0(root, nombreTabla)
  
  tabla <- read.csv(rootTabla)
  
  
  # Make factors of categorical features, deltaing intial-final features
  
  
  tabla$Endulzante <- factor(tabla$Endulzante, levels = c("SA", "ST", "SU"))
  tabla$Sexo <- factor(tabla$Sexo, levels = c("HOMBRE", "MUJER"))
  tabla$Tiempo <- factor(tabla$Tiempo, levels = c("0", "Final"))
  
  tabla$Delta.IRCV <- tabla$IRCV.Final - tabla$IRCV.inicial
  tabla$Delta.Bpmin <- tabla$Bpmin.final - tabla$Bpmin.inicial
  tabla$Delta.Bpmax <- tabla$Bpmax.final - tabla$Bpmax.inicial
  tabla$Delta.Frec <- tabla$Frec.final - tabla$Frec.inicial
  
  # Removing of trivial redundant and useless features 
  
  set.A <- subset(tabla, select =-c(X.1, numVol, X, Peso.inicial, Peso.final, Talla, IMC.Inicial, IMC.Final, 
                                    Grasa.inicial, Grasa.final, IRCV.Final, IRCV.inicial, Bpmin.final, 
                                    Bpmin.inicial, Bpmax.final, Bpmax.inicial, Frec.final, Frec.inicial))
  
  # Only numerical features
  
  set.A_num <- subset(set.A, select=-c(Endulzante, Sexo, Tiempo))
  
  #Rescaling, can use "set.A_rescaled <- scale(set.A_num)" too
  
  set.A_rescaled <- set.A_num %>% mutate_each_(list(~rescale(.) %>% as.vector), 
                                               vars = colnames(set.A_num))
  
  set.A_factors <- cbind(set.A_rescaled, Endulzante = set.A$Endulzante, 
                         Tiempo = set.A$Tiempo, Sexo = set.A$Sexo)
  
  return(list(tablaFactors = set.A, tablaNum = set.A_rescaled))
  
}

checkCluster <- function(tabla){
  
  nombreTabla <- deparse(quote(tabla))
  
  set.A_rescaled <- tabla
  # Validación de método de clustering

  ## Manual 
  
  datos_simulados <- map_df(set.A_rescaled,
                            .f = function(x){runif(n = length(x),
                                                   min = min(x),
                                                   max = max(x))
                            }
  )
  
  
  pca_datos_A      <- prcomp(set.A_rescaled)
  pca_datos_simulados <- prcomp(datos_simulados)
  p1 <- fviz_pca_ind(X = pca_datos_A,
                     geom = "point", title = paste("PCA - Datos ", nombreTabla), 
                     pallete = "jco") +
    theme_bw() + theme(legend.position = "bottom")
  p2 <- fviz_pca_ind(X = pca_datos_simulados, geom = "point",
                     title = paste("PCA - datos simulados ",nombreTabla), pallete = "jco") +
    theme_bw() + theme(legend.position = "bottom")
  
  print(ggarrange(p1, p2), common.legend = TRUE)
  
  dist_datos_A      <- dist(set.A_rescaled, method = "euclidean")
  dist_datos_simulados <- dist(datos_simulados, method = "euclidean")
  
  p7 <- fviz_dist(dist.obj = dist_datos_A, show_labels = FALSE) +
    labs(title = paste("Datos ", nombreTabla)) + theme(legend.position = "bottom")
  p8 <- fviz_dist(dist.obj = dist_datos_simulados, show_labels = FALSE) +
    labs(title = paste("Datos simulados ", nombreTabla)) + theme(legend.position = "bottom")
  
  print(ggarrange(p7, p8))
  
  print(fviz_nbclust(set.A_rescaled, kmeans, method="wss"))
}


clusterVarios <- function (tabla, nclust) {
  
  nombreTabla <- deparse(quote(tabla))
  
  
  ### Aplicamos clustering 
  
  # K-means clustering
  
  km_datos_A <- kmeans(x = tabla, centers = nclust)
  
  p1 <- fviz_cluster(object = km_datos_A, data = tabla, 
                     ellipse.type = "norm", geom = "point", main = paste("Datos ", nombreTabla),
                     stand = FALSE, palette = "jco", show.legend = T)
  
  # Hierarchical clustering
  p2 <- fviz_dend(x = hclust(dist(tabla)), k = nclust, k_colors = "jco",
                show_labels = T, main = paste("Datos ", nombreTabla))
  
  print(p1)
  
  print(p2)
  
  print(fviz_contrib(km_datos_A, choice="var", top = 13))
  
}

pcaVarios<- function(tabla){
  
  nombreTabla <- deparse(quote(tabla))
  
  tabla_Tiempo <- subset(tabla, select=-c(Endulzante, Sexo))
  
  tabla_Tiempo0 <- subset(tabla_Tiempo, Tiempo == "0", select = -Tiempo)
  tabla_TiempoF <- subset(tabla_Tiempo, Tiempo == "Final", select = -Tiempo)
  
  tabla_Sexo <- subset(tabla, select=-c(Endulzante, Tiempo))
  
  tabla_SexoM <- subset(tabla_Sexo, Sexo == "HOMBRE", select = -Sexo)
  tabla_SexoF <- subset(tabla_Sexo, Sexo == "MUJER", select = -Sexo)
  
  tabla_Endulzante <- subset(tabla, select=-c(Sexo, Tiempo))
  
  tabla_EndulzanteSA <- subset(tabla_Endulzante, Endulzante == "SA", select = -Endulzante)
  tabla_EndulzanteSU <- subset(tabla_Endulzante, Endulzante == "SU", select = -Endulzante)
  tabla_EndulzanteST <- subset(tabla_Endulzante, Endulzante == "ST", select = -Endulzante)
  
  return (list(
    tabla_Tiempo,
    tabla_Tiempo0,
    tabla_TiempoF,
    tabla_Sexo, 
    tabla_SexoM,
    tabla_SexoF,
    tabla_Endulzante,
    tabla_EndulzanteSA,
    tabla_EndulzanteSU,
    tabla_EndulzanteST
  ))

  
  # PCA Tiempo
  
  pca_datos_A      <- prcomp(tabla_Tiempo0)
  print(fviz_contrib(pca_datos_A, choice="var", top = 13, title = paste("Importancia
                     variables PCA Tiempo 0 ", nombreTabla)))
  pca_datos_B      <- prcomp(tabla_TiempoF)
  print(fviz_contrib(pca_datos_B, choice="var", top = 13, title = paste("Importancia
                     variables PCA Tiempo Final ", nombreTabla)))
  
  # PCA Sexo
  
  pca_datos_C      <- prcomp(tabla_SexoM)
  print(fviz_contrib(pca_datos_C, choice="var", top = 13, title = paste("Importancia
                     variables PCA Sexo M ", nombreTabla)))
  pca_datos_D      <- prcomp(tabla_SexoF)
  print(fviz_contrib(pca_datos_D, choice="var", top = 13, title = paste("Importancia
                     variables PCA Sexo F ", nombreTabla)))
  
  # PCA Sexo
  
  pca_datos_E      <- prcomp(tabla_EndulzanteSA)
  print(fviz_contrib(pca_datos_E, choice="var", top = 13, title = paste("Importancia variables PCA Endulzante SA ", nombreTabla)))
  pca_datos_F      <- prcomp(tabla_EndulzanteSU)
  print(fviz_contrib(pca_datos_F, choice="var", top = 13, title = paste("Importancia
                                                                        variables PCA Endulzante SU ", nombreTabla)))
  pca_datos_G      <- prcomp(tabla_EndulzanteST)
  print(fviz_contrib(pca_datos_G, choice="var", top = 13))
}

# Orina Flavonoides ----

## Preprocess ----

orinaFlav <- preprocessTablas("data/", "cronicoOrinaFlav_Antro.csv")
orinaFlavRaw <- orinaFlav$tablaFactors
orinaFlavNum <- orinaFlav$tablaNum


orinaFlavNum_0 <- subset(orinaFlavNumTiempo, Tiempo == "0", select = -Tiempo)
orinaFlavNum_F <- subset(orinaFlavNumTiempo, Tiempo == "Final", select = -Tiempo)


## PCA ----

pruebaUwu <- pcaVarios(orinaFlav$tablaFactors)



fviz_contrib(prcomp(pruebaUwu[[5]]), choice="var", top = 13)

pca_datos_A      <- prcomp(orinaFlavNum_0)
print(fviz_contrib(pca_datos_A, choice="var", top = 13))

pca_datos_A      <- prcomp(orinaFlavNum_F)
print(fviz_contrib(pca_datos_A, choice="var", top = 13))

## Clustering ----

### Checking clustering ----

checkCluster(orinaFlavNum_0)
checkCluster(orinaFlavNum_F)

### Performing kmeans ----

clusterVarios(orinaFlavNum_0, 3)
clusterVarios(orinaFlavNum_F, 5)

km_datos_A <- kmeans(x = orinaFlavNum_0, centers = 3)
km_datos_B <- kmeans(x = orinaFlavNum_F, centers = 5)

orinaFlavNum_0$cluster <- as.factor(km_datos_A$cluster)
orinaFlavNum_F$cluster <- as.factor(km_datos_B$cluster)

### Explicando clusters

p <- ggparcoord(data = orinaFlavNum_0, groupColumn = "cluster", scale = "std") + 
               labs(x = "milk constituent", 
                    y = "value (in standard-deviation units)", 
                    title = "Clustering Tiempo 0")


p1 <- ggparcoord(data = orinaFlavNum_F, columns = c(2:14), groupColumn = "cluster", 
                 scale = "std") + 
                 labs(x = "milk constituent", 
                 y = "value (in standard-deviation units)", 
                 title = "Clustering Tiempo Final")


ggplotly(p1)

plot(orinaFlavNum_0$cluster, orinaFlavNum_0$NS)
plot(orinaFlavNum_F$cluster, orinaFlavNum_F$NS)

### Orina Antocianos ---- 

orinaAnt <- preprocessTablas("data/", "cronicoOrinaAnt_Antro.csv")
orinaAntRaw <- orinaAnt$tablaRaw
orinaAntNum <- orinaAnt$tablaNum

orinaAntNumTiempo <- cbind(orinaAntNum, Tiempo = orinaAntRaw$Tiempo)


orinaAntNum_0 <- subset(orinaAntNumTiempo, Tiempo == "0", select = -Tiempo)
orinaAntNum_F <- subset(orinaAntNumTiempo, Tiempo == "Final", select = -Tiempo)

checkCluster(orinaAntNum_0)
checkCluster(orinaAntNum_F)

clusterVarios(orinaFlavNum_0, 5)
clusterVarios(orinaFlavNum_F, 4)



### Plasma Antocianos ---- 

orinaAnt <- preprocessTablas("data/", "cronicoPlasmaAnt_Antro.csv")
orinaAntRaw <- orinaAnt$tablaRaw
orinaAntNum <- orinaAnt$tablaNum

orinaAntNumTiempo <- cbind(orinaAntNum, Tiempo = orinaAntRaw$Tiempo)

orinaAntNum_0 <- subset(orinaAntNumTiempo, Tiempo == "0", select = -Tiempo)
orinaAntNum_F <- subset(orinaAntNumTiempo, Tiempo == "Final", select = -Tiempo)


## PCA ----

pca_datos_A      <- prcomp(orinaAntNum_0)
print(fviz_contrib(pca_datos_A, choice="var", top = 13))

pca_datos_A      <- prcomp(orinaAntNum_F)
print(fviz_contrib(pca_datos_A, choice="var", top = 13))

checkCluster(orinaAntNum_0)
checkCluster(orinaAntNum_F)

clusterVarios(orinaFlavNum_0, 5)
clusterVarios(orinaFlavNum_F, 4)

