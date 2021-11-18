library(dplyr)
library(scales)
library(purrr)
library(ggpubr)
library(factoextra)
library(GGally)
library(plotly)
library(FactoMineR)
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
  
  return(list(tablaFactors = set.A_factors, tablaNum = set.A_rescaled))
  
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
  
  
  
}

pcaVarios<- function(tabla, nombreTabla){
  
  #nombreTabla <- deparse(quote(tabla))
  
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

  
  # PCA Tiempo
  
  pca_datos_A <- PCA(tabla_Tiempo0, scale.unit = F)
  print(fviz_contrib(pca_datos_A, choice="var", title = paste("Importancia variables dim 1 PCA Tiempo 0 ", 
                                                                        nombreTabla)))
  
  print(fviz_contrib(pca_datos_A, choice="var", axes = 2, title = paste("Importancia variables dim 2 PCA Tiempo 0 ", 
                                                                        nombreTabla)))
  pca_datos_B <- PCA(tabla_TiempoF)
  
  print(fviz_contrib(pca_datos_B, choice="var", axes=1, title = paste("Importancia variables dim 1 PCA Tiempo Final "
                                                                        ,nombreTabla)))
  print(fviz_contrib(pca_datos_B, choice="var", axes=2, title = paste("Importancia variables dim 2 PCA Tiempo Final "
                                                                      ,nombreTabla)))
  # PCA Sexo
  
  pca_datos_C <- PCA(tabla_SexoM,scale.unit = F)
  print(fviz_contrib(pca_datos_C, choice="var", title = paste("Importancia variables dim 1 PCA Sexo M ", 
                                                              nombreTabla)))
  
  print(fviz_contrib(pca_datos_C, choice="var", axes = 2, title = paste("Importancia variables dim 2 PCA Sexo M ", 
                                                                        nombreTabla)))
  pca_datos_D <- PCA(tabla_SexoF,scale.unit = F)
  print(fviz_contrib(pca_datos_D, choice="var", title = paste("Importancia variables dim 1 PCA Sexo F ", 
                                                              nombreTabla)))
  
  print(fviz_contrib(pca_datos_D, choice="var", axes = 2, title = paste("Importancia variables dim 2 PCA Sexo F", 
                                                                        nombreTabla)))
  
  
  # PCA Endulzantes
  
  pca_datos_E      <- PCA(tabla_EndulzanteSA, scale.unit = F)
  
  print(fviz_contrib(pca_datos_E, choice="var", title = paste("Importancia variables dim 1 PCA Endulzante SA ", 
                                                              nombreTabla)))
  
  print(fviz_contrib(pca_datos_E, choice="var", axes = 2, title = paste("Importancia variables dim 2 PCA Endulzante SA ", 
                                                                        nombreTabla)))
  
  pca_datos_F      <- PCA(tabla_EndulzanteSU, scale.unit = F)
  
  print(fviz_contrib(pca_datos_F, choice="var", title = paste("Importancia variables dim 1 PCA Endulzante SU ", 
                                                              nombreTabla)))
  
  print(fviz_contrib(pca_datos_F, choice="var", axes = 2, title = paste("Importancia variables dim 2 PCA Endulzante SU ", 
                                                                        nombreTabla)))
  
  pca_datos_G      <- PCA(tabla_EndulzanteST, scale.unit = F)
  print(fviz_contrib(pca_datos_G, choice="var", title = paste("Importancia variables dim 1 PCA Endulzante ST ", 
                                                              nombreTabla)))
  
  print(fviz_contrib(pca_datos_G, choice="var", axes = 2, title = paste("Importancia variables dim 2 PCA Endulzante ST ", 
                                                                        nombreTabla)))
  # PCA TiempoxSexo
  
  tabla_0xM <- subset(tabla, Tiempo == "0" & Sexo == "HOMBRE", select =-c(Tiempo, Sexo, Endulzante))
  tabla_0xF <- subset(tabla, Tiempo == "0" & Sexo == "MUJER", select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxM <- subset(tabla, Tiempo == "Final" & Sexo == "HOMBRE", select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxF <- subset(tabla, Tiempo == "Final" & Sexo == "MUJER", select =-c(Tiempo, Sexo, Endulzante))
  
  pca_0xM <- PCA(tabla_0xM, scale.unit=F)
  print(fviz_contrib(pca_0xM, choice="var", title = paste("ContribVar dim1 Tiempo 0 Sexo M", nombreTabla)))
  
  print(fviz_contrib(pca_0xM, choice="var", axes = 2, title = paste("ContribVar dim2 Tiempo 0 Sexo M", 
                                                                        nombreTabla)))
  pca_0xF <- PCA(tabla_0xF, scale.unit=F)
  
  print(fviz_contrib(pca_0xF, choice="var", title = paste("ContribVar dim1 Tiempo 0 Sexo F", nombreTabla)))
  
  print(fviz_contrib(pca_0xF, choice="var", axes = 2, title = paste("ContribVar dim2 Tiempo 0 Sexo F", 
                                                                    nombreTabla)))
  
  pca_FxM <- PCA(tabla_FxM, scale.unit=F)
  
  print(fviz_contrib(pca_FxM, choice="var", title = paste("ContribVar dim1 Tiempo F Sexo M", nombreTabla)))
  
  print(fviz_contrib(pca_FxM, choice="var", axes = 2, title = paste("ContribVar dim2 Tiempo F Sexo M", 
                                                                    nombreTabla)))
  pca_FxF <- PCA(tabla_FxF, scale.unit=F)
  
  print(fviz_contrib(pca_FxF, choice="var", title = paste("ContribVar dim1 Tiempo F Sexo F", nombreTabla)))
  
  print(fviz_contrib(pca_FxF, choice="var", axes = 2, title = paste("ContribVar dim2 Tiempo F Sexo F", 
                                                                    nombreTabla)))
  print(fviz_contrib(pca_0xM, choice="var", axes = 2, title = paste("ContribVar dim2 Tiempo 0 Sexo M", 
                                                                    nombreTabla)))
  
  # PCA TiempoxEndulzante
  
  # PCA SexoxEndulzante
  
  tabla_MxSAx0 <- subset(tabla, Sexo == "HOMBRE" & Endulzante =="SA" & Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_MxSAxF <- subset(tabla, Sexo == "HOMBRE" & Endulzante =="SA" & Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_FxSAx0 <- subset(tabla, Sexo == "MUJER" & Endulzante =="SA"& Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxSAxF <- subset(tabla, Sexo == "MUJER" & Endulzante =="SA"& Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_MxSTx0 <- subset(tabla, Sexo == "HOMBRE" & Endulzante =="ST" & Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_MxSTxF <- subset(tabla, Sexo == "HOMBRE" & Endulzante =="ST"& Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_FxSTx0 <- subset(tabla, Sexo == "MUJER" & Endulzante =="ST" & Tiempo == "0",
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxSTxF <- subset(tabla, Sexo == "MUJER" & Endulzante =="ST" & Tiempo == "Final",
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_MxSUx0 <- subset(tabla, Sexo == "HOMBRE" & Endulzante =="SU" & Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_MxSUxF <- subset(tabla, Sexo == "HOMBRE" & Endulzante =="SU" & Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_FxSUx0 <- subset(tabla, Sexo == "MUJER" & Endulzante =="SU" & Tiempo == "0",
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxSUxF <- subset(tabla, Sexo == "MUJER" & Endulzante =="SU" & Tiempo == "Final",
                         select =-c(Tiempo, Sexo, Endulzante))
  
  
  pca_MxSAx0 <- PCA(tabla_MxSAx0, scale.unit=F)
  print(fviz_contrib(pca_MxSAx0, choice="var", title = paste("ContribVar dim1 Sexo M Endulzante SA Tiempo 0", 
                                                           nombreTabla)))
  
  print(fviz_contrib(pca_MxSAx0, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo M Endulzante SA Tiempo 0", 
                                                                    nombreTabla)))
  
  pca_MxSAxF <- PCA(tabla_MxSAxF, scale.unit=F)
  print(fviz_contrib(pca_MxSAxF, choice="var", title = paste("ContribVar dim1 Sexo M Endulzante SA Tiempo F", 
                                                             nombreTabla)))
  
  print(fviz_contrib(pca_MxSAxF, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo M Endulzante SA Tiempo F", 
                                                                       nombreTabla)))
  
  pca_FxSAx0 <- PCA(tabla_FxSAx0, scale.unit=F)
  print(fviz_contrib(pca_FxSAx0, choice="var", title = paste("ContribVar dim1 Sexo F Endulzante SA Tiempo 0",
                                                             nombreTabla)))
  
  print(fviz_contrib(pca_FxSAx0, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo F Endulzante SA Tiempo 0", 
                                                                     nombreTabla)))
                                                                     
  pca_FxSAxF <- PCA(tabla_FxSAxF, scale.unit=F)
  
  print(fviz_contrib(pca_FxSAxF, choice="var", title = paste("ContribVar dim1 Sexo F Endulzante SA Tiempo F",
                                                             nombreTabla)))
  
  print(fviz_contrib(pca_FxSAxF, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo F Endulzante SA Tiempo F",
                                                                       nombreTabla)))
  pca_MxSTx0 <- PCA(tabla_MxSTx0, scale.unit=F)
  print(fviz_contrib(pca_MxSTx0, choice="var", title = paste("ContribVar dim1 Sexo M Endulzante ST Tiempo 0", 
                                                             nombreTabla)))
  
  print(fviz_contrib(pca_MxSTx0, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo M Endulzante ST Tiempo 0", 
                                                                     nombreTabla)))
  pca_MxSTxF <- PCA(tabla_MxSTxF, scale.unit=F)
  print(fviz_contrib(pca_MxSTxF, choice="var", title = paste("ContribVar dim1 Sexo M Endulzante ST Tiempo F",
                                                             nombreTabla)))
  
  print(fviz_contrib(pca_MxSTxF, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo M Endulzante ST Tiempo F", 
                                                                     nombreTabla)))
  
  
  pca_FxSTx0 <- PCA(tabla_FxSTx0, scale.unit=F)
  print(fviz_contrib(pca_FxSTx0, choice="var", title = paste("ContribVar dim1 Sexo F Endulzante ST Tiempo 0", nombreTabla)))
  
  print(fviz_contrib(pca_FxSTx0, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo F Endulzante ST Tiempo 0", 
                                                                     nombreTabla)))
  
  
  pca_FxSTxF <- PCA(tabla_FxSTxF, scale.unit=F)
  print(fviz_contrib(pca_FxSTxF, choice="var", title = paste("ContribVar dim1 Sexo F Endulzante ST Tiempo F", nombreTabla)))
  
  print(fviz_contrib(pca_FxSTxF, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo F Endulzante ST Tiempo F", 
                                                                       nombreTabla)))
  
  pca_MxSUx0 <- PCA(tabla_MxSUx0, scale.unit=F)
  print(fviz_contrib(pca_MxSUx0, choice="var", title = paste("ContribVar dim1 Sexo M Endulzante SU Tiempo 0", 
                                                             nombreTabla)))
  
  print(fviz_contrib(pca_MxSUx0, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo M Endulzante SU Tiempo 0", 
                                                                     nombreTabla)))
  
  pca_MxSUxF <- PCA(tabla_MxSUxF, scale.unit=F)
  print(fviz_contrib(pca_MxSUxF, choice="var", title = paste("ContribVar dim1 Sexo M Endulzante SU Tiempo F", 
                                                             nombreTabla)))
  
  print(fviz_contrib(pca_MxSUxF, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo M Endulzante SU Tiempo F", 
                                                                       nombreTabla)))
  pca_FxSUx0 <- PCA(tabla_FxSUx0, scale.unit=F)
  print(fviz_contrib(pca_FxSUx0, choice="var", title = paste("ContribVar dim1 Sexo F Endulzante SU Tiempo 0", 
                                                             nombreTabla)))
  
  print(fviz_contrib(pca_FxSUx0, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo F Endulzante SU Tiempo 0", 
                                                                       nombreTabla)))
  
  
  pca_FxSUxF <- PCA(tabla_FxSUxF, scale.unit=F)
  print(fviz_contrib(pca_FxSUxF, choice="var", title = paste("ContribVar dim1 Sexo F Endulzante SU Tiempo F", 
                                                             nombreTabla)))
  
  print(fviz_contrib(pca_FxSUxF, choice="var", axes = 2, title = paste("ContribVar dim2 Sexo F Endulzante SU Tiempo F", 
                                                                       nombreTabla)))  
}

# Orina Flavonoides ----

## Preprocess ----

orinaFlav <- preprocessTablas("data/", "cronicoOrinaFlav_Antro.csv")
orinaFlavRaw <- orinaFlav$tablaFactors
orinaFlavNum <- orinaFlav$tablaNum

orinaFlavNum_0 <- subset(orinaFlavNumTiempo, Tiempo == "0", select = -Tiempo)
orinaFlavNum_F <- subset(orinaFlavNumTiempo, Tiempo == "Final", select = -Tiempo)


## PCA ----

orinaFlav_Factors <- orinaFlav$tablaFactors

pruebauwu2 <- subset(orinaFlav_Factors, Tiempo == "0" & Sexo == "HOMBRE", select =-c(Tiempo, Sexo, Endulzante))

pcaVarios(orinaFlav_Factors, "orina flavonoides")

fviz_contrib(PCA(pruebaUwu[[5]]), choice="var", top = 13)

pca_datos_A      <- prcomp(orinaFlavNum_0)
print(fviz_contrib(pca_datos_A, choice="var", top = 13))

pca_datos_A <- prcomp(orinaFlavNum_F, scale = T)
pca_datos_Z <- PCA(orinaFlavNum_F, graph = F)

pca_datos_A
pca_datos_Z$var$contrib
  
  
fviz_contrib(pca_datos_A, choice="var")
fviz_contrib(pca_datos_Z, choice="var")

fviz_screeplot(pca_datos_A, addlabes = TRUE)

PVE <- 100*pca_datos_A$sdev^2/sum(pca_datos_A$sdev^2)
PVE

par(mfrow = c(1,2))

plot(PVE, type = "o", 
     ylab = "PVE", 
     xlab = "Componente principal", 
     col = "blue")
plot(cumsum(PVE), type = "o", 
     ylab = "PVE acumulada", 
     xlab = "Componente principal", 
     col = "brown3")

# Helper function 
#::::::::::::::::::::::::::::::::::::::::
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates
#::::::::::::::::::::::::::::::::::::::::
loadings <- pca_datos_A$rotation
sdev <- pca_datos_A$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[, 1:4])

# Compute Cos2
#::::::::::::::::::::::::::::::::::::::::
var.cos2 <- var.coord^2
head(var.cos2[, 1:4])

# Compute contributions
#::::::::::::::::::::::::::::::::::::::::
comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
head(var.contrib[, 1:4])


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


ggplotly(p)

plot(orinaFlavNum_0$cluster, orinaFlavNum_0$NS)
plot(orinaFlavNum_F$cluster, orinaFlavNum_F$NS)

# Orina Antocianos ---- 

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



# Plasma Antocianos ---- 

orinaAnt <- preprocessTablas("data/", "cronicoPlasmaAnt_Antro.csv")
orinaAntRaw <- orinaAnt$tablaRaw
orinaAntNum <- orinaAnt$tablaNum

orinaAntNumTiempo <- cbind(orinaAntNum, Tiempo = orinaAntRaw$Tiempo)

orinaAntNum_0 <- subset(orinaAntNumTiempo, Tiempo == "0", select = -Tiempo)
orinaAntNum_F <- subset(orinaAntNumTiempo, Tiempo == "Final", select = -Tiempo)



