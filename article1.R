library(dplyr)
library(scales)
library(purrr)
library(ggpubr)
library(factoextra)
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
  
  return(list(tablaRaw = set.A, tablaNum = set.A_rescaled))
  
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
}


clusterVarios <- function (tabla) {
  
  nombreTabla <- deparse(quote(tabla))
  
  
  ### Aplicamos clustering 
  
  # K-means clustering
  
  km_datos_A <- kmeans(x = tabla, centers = 2)
  
  p1 <- fviz_cluster(object = km_datos_A, data = tabla, 
                     ellipse.type = "norm", geom = "point", main = paste("Datos ", nombreTabla),
                     stand = FALSE, palette = "jco", show.legend = T)
  
  # Hierarchical clustering
  p2 <- fviz_dend(x = hclust(dist(tabla)), k = 2, k_colors = "jco",
                show_labels = T, main = paste("Datos ", nombreTabla))
  
  print(p1)
  
  print(p2)
}


orinaFlav <- preprocessTablas("data/", "cronicoOrinaFlav_Antro.csv")
orinaFlavRaw <- orinaFlav$tablaRaw
orinaFlavNum <- orinaFlav$tablaNum

orinaFlavNumTiempo <- cbind(orinaFlavNum, Tiempo = orinaFlavRaw$Tiempo)

orinaFlavNum_0 <- subset(orinaFlavNumTiempo, Tiempo == "0", select = -Tiempo)
orinaFlavNum_F <- subset(orinaFlavNumTiempo, Tiempo == "Final", select = -Tiempo)

checkCluster(orinaFlavNum_0)
checkCluster(orinaFlavNum_F)

clusterVarios(orinaFlavNum_0)
