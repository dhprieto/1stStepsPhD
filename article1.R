library(dplyr)
library(scales)
library(purrr)
library(ggpubr)
library(factoextra)
library(GGally)
library(plotly)
library(FactoMineR)
library(cluster)
library(mclust)

preprocessTablas <- function(root, nombreTabla) {
  
  # Getting data ready
  rootTabla <- paste0(root, nombreTabla)
  
  tabla <- read.csv(rootTabla)
  
  
  # Make factors of categorical features, deltaing intial-final features
  
  
  tabla$Endulzante <- factor(tabla$Endulzante, levels = c("SA", "ST", "SU"))
  tabla$Sexo <- factor(tabla$Sexo, levels = c("HOMBRE", "MUJER"))
  tabla$Tiempo <- factor(tabla$Tiempo, levels = c("0", "Final"))
  tabla$numVol <- factor(tabla$numVol)
  
  tabla$Delta.IRCV <- tabla$IRCV.Final - tabla$IRCV.inicial
  tabla$Delta.Bpmin <- tabla$Bpmin.final - tabla$Bpmin.inicial
  tabla$Delta.Bpmax <- tabla$Bpmax.final - tabla$Bpmax.inicial
  tabla$Delta.Frec <- tabla$Frec.final - tabla$Frec.inicial
  
  # Removing of trivial redundant and useless features 
  
  set.A <- subset(tabla, select =-c(X.1, X, Peso.inicial, Peso.final, Talla, IMC.Inicial, IMC.Final, 
                                    Grasa.inicial, Grasa.final, IRCV.Final, IRCV.inicial, Bpmin.final, 
                                    Bpmin.inicial, Bpmax.final, Bpmax.inicial, Frec.final, Frec.inicial))
  
  # Only numerical features
  
  set.A_num <- subset(set.A, select=-c(Endulzante, Sexo, Tiempo, numVol))
  
  #Rescaling, can use "set.A_rescaled <- scale(set.A_num)" too
  
  set.A_rescaled <- set.A_num %>% mutate_each_(list(~rescale(.) %>% as.vector), 
                                               vars = colnames(set.A_num))
  
  set.A_factors <- cbind(set.A_rescaled, Endulzante = set.A$Endulzante, 
                         Tiempo = set.A$Tiempo, Sexo = set.A$Sexo, numVol = set.A$numVol)
  
  tabla_Tiempo <- subset(set.A_factors, select=-c(Endulzante, Sexo))
  
  tabla_Tiempo0 <- subset(tabla_Tiempo, Tiempo == "0", select = -Tiempo)
  tabla_TiempoF <- subset(tabla_Tiempo, Tiempo == "Final", select = -Tiempo)
  
  tabla_Sexo <- subset(set.A_factors, select=-c(Endulzante, Tiempo))
  
  tabla_SexoM <- subset(tabla_Sexo, Sexo == "HOMBRE", select = -Sexo)
  tabla_SexoF <- subset(tabla_Sexo, Sexo == "MUJER", select = -Sexo)
  
  tabla_Endulzante <- subset(set.A_factors, select=-c(Sexo, Tiempo))
  
  tabla_EndulzanteSA <- subset(tabla_Endulzante, Endulzante == "SA", select = -Endulzante)
  tabla_EndulzanteSU <- subset(tabla_Endulzante, Endulzante == "SU", select = -Endulzante)
  tabla_EndulzanteST <- subset(tabla_Endulzante, Endulzante == "ST", select = -Endulzante)
  
  tabla_0xM <- subset(set.A_factors, Tiempo == "0" & Sexo == "HOMBRE", select =-c(Tiempo, Sexo, Endulzante))
  tabla_0xF <- subset(set.A_factors, Tiempo == "0" & Sexo == "MUJER", select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxM <- subset(set.A_factors, Tiempo == "Final" & Sexo == "HOMBRE", select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxF <- subset(set.A_factors, Tiempo == "Final" & Sexo == "MUJER", select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_MxSAx0 <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="SA" & Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_MxSAxF <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="SA" & Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_FxSAx0 <- subset(set.A_factors, Sexo == "MUJER" & Endulzante =="SA"& Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxSAxF <- subset(set.A_factors, Sexo == "MUJER" & Endulzante =="SA"& Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_MxSTx0 <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="ST" & Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_MxSTxF <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="ST"& Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_FxSTx0 <- subset(set.A_factors, Sexo == "MUJER" & Endulzante =="ST" & Tiempo == "0",
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxSTxF <- subset(tabla, Sexo == "MUJER" & Endulzante =="ST" & Tiempo == "Final",
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_MxSUx0 <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="SU" & Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_MxSUxF <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="SU" & Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_FxSUx0 <- subset(set.A_factors, Sexo == "MUJER" & Endulzante =="SU" & Tiempo == "0",
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxSUxF <- subset(set.A_factors, Sexo == "MUJER" & Endulzante =="SU" & Tiempo == "Final",
                         select =-c(Tiempo, Sexo, Endulzante))
  
  return(list(tablaFactors = set.A_factors, tablaNum = set.A_rescaled,
              tabla_Tiempo = tabla_Tiempo, tabla_Tiempo0 = tabla_Tiempo0,
              tabla_TiempoF = tabla_TiempoF,  tabla_Sexo=tabla_Sexo,
              tabla_SexoM=tabla_SexoM, tabla_SexoF=tabla_SexoF,
              tabla_Endulzante=tabla_Endulzante, 
              tabla_EndulzanteSA=tabla_EndulzanteSA,
              tabla_EndulzanteSU=tabla_EndulzanteSU, 
              tabla_EndulzanteST=tabla_EndulzanteST,
              tabla_0xM=tabla_0xM,tabla_0xF=tabla_0xF, 
              tabla_FxM=tabla_FxM, tabla_FxF=tabla_FxF,
              tabla_MxSAx0=tabla_MxSAx0, tabla_MxSAxF=tabla_MxSAxF,
              tabla_FxSAx0=tabla_FxSAx0, tabla_FxSAxF=tabla_FxSAxF, 
              tabla_MxSTx0=tabla_MxSTx0, tabla_MxSTxF=tabla_MxSTxF,
              tabla_FxSTx0=tabla_FxSTx0, tabla_FxSTxF=tabla_FxSTxF, 
              tabla_MxSUx0=tabla_MxSUx0, tabla_MxSUxF=tabla_MxSUxF, 
              tabla_FxSUx0=tabla_FxSUx0, tabla_FxSUxF=tabla_FxSUxF))
  
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
  
  dist_datos_A      <- get_dist(set.A_rescaled, method = "manhattan")
  dist_datos_simulados <- get_dist(datos_simulados, method = "manhattan")
  
  p7 <- fviz_dist(dist.obj = dist_datos_A, show_labels = FALSE) +
    labs(title = paste("Datos ", nombreTabla)) + theme(legend.position = "bottom")
  p8 <- fviz_dist(dist.obj = dist_datos_simulados, show_labels = FALSE) +
    labs(title = paste("Datos simulados ", nombreTabla)) + theme(legend.position = "bottom")
  
  print(ggarrange(p7, p8))
  
  print(fviz_nbclust(set.A_rescaled, kmeans, method="wss"))
  print(fviz_nbclust(x = set.A_rescaled, FUNcluster = pam, method = "wss", k.max = 15,
               diss = get_dist(set.A_rescaled, method = "manhattan")))
}



pcaVarios<- function(tabla, nombreTabla){
  
  #nombreTabla <- deparse(quote(tabla))
  
  tabla_Tiempo <- subset(tabla, select=-c(Endulzante, Sexo, numVol))
  
  tabla_Tiempo0 <- subset(tabla_Tiempo, Tiempo == "0", select = -Tiempo)
  tabla_TiempoF <- subset(tabla_Tiempo, Tiempo == "Final", select = -Tiempo)
  
  tabla_Sexo <- subset(tabla, select=-c(Endulzante, Tiempo, numVol))
  
  tabla_SexoM <- subset(tabla_Sexo, Sexo == "HOMBRE", select = -Sexo)
  tabla_SexoF <- subset(tabla_Sexo, Sexo == "MUJER", select = -Sexo)
  
  tabla_Endulzante <- subset(tabla, select=-c(Sexo, Tiempo, numVol))
  
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
  
  tabla_0xM <- subset(tabla, Tiempo == "0" & Sexo == "HOMBRE", select =-c(Tiempo, Sexo, Endulzante, numVol))
  tabla_0xF <- subset(tabla, Tiempo == "0" & Sexo == "MUJER", select =-c(Tiempo, Sexo, Endulzante, numVol))
  tabla_FxM <- subset(tabla, Tiempo == "Final" & Sexo == "HOMBRE", select =-c(Tiempo, Sexo, Endulzante, numVol))
  tabla_FxF <- subset(tabla, Tiempo == "Final" & Sexo == "MUJER", select =-c(Tiempo, Sexo, Endulzante, numVol))
  
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
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  
  tabla_MxSAxF <- subset(tabla, Sexo == "HOMBRE" & Endulzante =="SA" & Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  
  tabla_FxSAx0 <- subset(tabla, Sexo == "MUJER" & Endulzante =="SA"& Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  tabla_FxSAxF <- subset(tabla, Sexo == "MUJER" & Endulzante =="SA"& Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  
  tabla_MxSTx0 <- subset(tabla, Sexo == "HOMBRE" & Endulzante =="ST" & Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  tabla_MxSTxF <- subset(tabla, Sexo == "HOMBRE" & Endulzante =="ST"& Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  
  tabla_FxSTx0 <- subset(tabla, Sexo == "MUJER" & Endulzante =="ST" & Tiempo == "0",
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  tabla_FxSTxF <- subset(tabla, Sexo == "MUJER" & Endulzante =="ST" & Tiempo == "Final",
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  
  tabla_MxSUx0 <- subset(tabla, Sexo == "HOMBRE" & Endulzante =="SU" & Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  tabla_MxSUxF <- subset(tabla, Sexo == "HOMBRE" & Endulzante =="SU" & Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  
  tabla_FxSUx0 <- subset(tabla, Sexo == "MUJER" & Endulzante =="SU" & Tiempo == "0",
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  tabla_FxSUxF <- subset(tabla, Sexo == "MUJER" & Endulzante =="SU" & Tiempo == "Final",
                         select =-c(Tiempo, Sexo, Endulzante, numVol))
  
  
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

clusterVarios <- function (tabla, nclust) {
  
  nombreTabla <- deparse(quote(tabla))
  
  
  ### Aplicamos clustering 
  
  # K-means clustering
  set.seed(123)
  
  km_datos_A <- kmeans(x = tabla, centers = nclust)
  
  p1 <- fviz_cluster(object = km_datos_A, data = tabla, 
                     ellipse.type = "norm", geom = "point", main = paste("Resultados clustering kmeans ", 
                                                                         nombreTabla),
                     stand = FALSE, palette = "jco", show.legend = T)
  
  # PAM clustering
  
  pam_clusters <- pam(x = tabla, k = nclust, metric = "manhattan")
  
  p2 <- fviz_cluster(object = pam_clusters, data = tabla, ellipse.type = "t",
               repel = TRUE) +
    theme_bw() +
    labs(title = "Resultados clustering PAM") +
    theme(legend.position = "none")
  
  # fuzzy clustering
  
  fuzzy_cluster <- fanny(x = tabla, k = nclust, diss = FALSE, metric = "manhattan", stand = FALSE)
  
  p3 <- fviz_cluster(object = fuzzy_cluster, repel = TRUE, ellipse.type = "norm",
                     pallete = "jco") + theme_bw() + labs(title = "Fuzzy Cluster plot")

  # Hierarchical clustering
  p4 <- fviz_dend(x = hclust(get_dist(tabla, method = "manhattan")), k = nclust, k_colors = "jco",
                show_labels = T, main = paste("Datos ", nombreTabla))
  
  
  model_clustering <- Mclust(tabla)
  
  p5 <- fviz_mclust(object = model_clustering, what = "BIC", pallete = "jco") +
    scale_x_discrete(limits = c(1:10))
  
  p6 <- fviz_mclust(model_clustering, what = "classification", geom = "point",
              pallete = "jco")
  
  
  print(p1)
  
  print(p2)
  
  print(p3)
  
  print(p4)
  
  print(p5)
  
  print(p6)
  
  return(model_clustering)
  
}


############################################################################# uwu

set.seed(123)

# Orina Flavonoides ----

## Preprocess ----

orinaFlav <- preprocessTablas("data/", "cronicoOrinaFlav_Antro.csv")
orinaFlavFactors <- orinaFlav$tablaFactors
orinaFlavNum <- orinaFlav$tablaNum

orinaFlavT_0 <- orinaFlav$tabla_Tiempo0
orinaFlavT_F <- orinaFlav$tabla_TiempoF

## PCA ----

pcaVarios(orinaFlavFactors, "orina flavonoides")

## Clustering ----

### Checking clustering ----

checkCluster(orinaFlavT_0[, colnames(orinaFlavT_0) != "numVol"])
checkCluster(orinaFlavT_F[, colnames(orinaFlavT_) != "numVol"])

### Performing kmeans, PAM and fuzzy ----

clusterVarios()

### Performing model based clustering ----

orinaFlav_Factors_T0 <- subset(orinaFlavFactors, Tiempo == "0")
orinaFlav_Factors_TF <- subset(orinaFlavFactors, Tiempo == "Final")

model_clustering_OFT_0 <- Mclust(orinaFlavT_0[, colnames(orinaFlavT_0) != "numVol"])
model_clustering_OFT_F <- Mclust(orinaFlavT_F[, colnames(orinaFlavT_F) != "numVol"])

### Plotting results of model based clustering 

p1 <- fviz_mclust(object = model_clustering_OFT_0, what = "BIC", pallete = "jco") +
  scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OFT_0, what = "classification", geom = "point",
                  pallete = "jco")

p3 <- fviz_mclust(object = model_clustering_OFT_F, what = "BIC", pallete = "jco") +
  scale_x_discrete(limits = c(1:10))

p4 <- fviz_mclust(model_clustering_OFT_F, what = "classification", geom = "point",
                  pallete = "jco")


ggarrange(p1,p3)
ggarrange(p2,p4)

### Rejoining factors to dataframe

orinaFlavNum_0_clusters <- cbind(orinaFlavT_0, clusters = as.factor(model_clustering_OFT_0$classification),
                                 Endulzante = orinaFlav_Factors_T0$Endulzante, 
                                 Sexo = orinaFlav_Factors_T0$Sexo)
orinaFlavNum_F_clusters <- cbind(orinaFlavT_F, clusters = as.factor(model_clustering_OFT_F$classification),
                                 Endulzante = orinaFlav_Factors_TF$Endulzante, 
                                 Sexo = orinaFlav_Factors_TF$Sexo)

### Scaling factors to plot them. 

orinaFlavNum_0_clusters$Endulzante <- rescale(as.numeric(orinaFlavNum_0_clusters$Endulzante))
orinaFlavNum_0_clusters$Sexo <- rescale(as.numeric(orinaFlavNum_0_clusters$Sexo))

orinaFlavNum_F_clusters$Endulzante <- rescale(as.numeric(orinaFlavNum_F_clusters$Endulzante))
orinaFlavNum_F_clusters$Sexo <- rescale(as.numeric(orinaFlavNum_F_clusters$Sexo))

### Boxplot to explain clusters ----


boxplot(subset(orinaFlavNum_0_clusters, clusters==1, select=-c(clusters,numVol)), 
        main = "Cluster 1 Orina Flav Tiempo 0")
boxplot(subset(orinaFlavNum_0_clusters, clusters==2, select=-c(clusters,numVol)), 
        main = "Cluster 2 Orina Flav Tiempo 0")
boxplot(subset(orinaFlavNum_0_clusters, clusters==3, select=-c(clusters,numVol)), 
        main = "Cluster 3 Orina Flav Tiempo 0")

boxplot(subset(orinaFlavNum_F_clusters, clusters==1, select=-c(clusters,numVol)))
boxplot(subset(orinaFlavNum_F_clusters, clusters==2, select=-c(clusters,numVol)))
boxplot(subset(orinaFlavNum_F_clusters, clusters==3, select=-c(clusters,numVol)))
boxplot(subset(orinaFlavNum_F_clusters, clusters==4, select=-c(clusters,numVol)))


### Explicando clusters

p <- ggparcoord(data = orinaFlavNum_0_clusters[, colnames(orinaFlavT_0) != "numVol"], groupColumn = "clusters", 
                scale = "std", columns = c(1,2,3,4,5,6,7,8,9,10,11,12,14,15)) + 
               labs(x = "variables", 
                    y = "value (in standard-deviation units)", 
                    title = "Clustering Tiempo 0")


p1 <- ggparcoord(data = orinaFlavNum_F_clusters[, colnames(orinaFlavT_F) != "numVol"], groupColumn = "clusters", 
                scale = "std", columns = c(1,2,3,4,5,6,7,8,9,10,11,12,14,15)) + 
                labs(x = "variables", 
                y = "value (in standard-deviation units)", 
                title = "Clustering Tiempo F")

ggarrange(p,p1)

## ANOVA ----

#### ANOVA paired ----

orinaFlavDupl <- orinaFlavFactors[duplicated(orinaFlavFactors$numVol) == TRUE,]

anova_pareado <- aov(formula = NS ~ Sexo + Endulzante + Tiempo + Sexo*Endulzante + Error(numVol/Tiempo),
                     data = orinaFlavDupl)
summary(anova_pareado)

#### ANOVA dos/tres vias ----

anova_no_pareado <- aov(formula = NS ~ Sexo + Endulzante, 
                        data = orinaFlav_Factors)
summary(anova_no_pareado)


###################################################################################
###################################################################################


# Orina Antocianos ---- 


## Preprocess ----

orinaAnt <- preprocessTablas("data/", "cronicoOrinaAnt_Antro.csv")

orinaAntFactors <- orinaAnt$tablaFactors
orinaAntNum <- orinaAnt$tablaNum

orinaAntT_0 <- orinaAnt$tabla_Tiempo0
orinaAntT_F <- orinaAnt$tabla_TiempoF

## PCA ----

pcaVarios(orinaAntFactors, "orina antocianos")

## Clustering ----

### Checking clustering ----

checkCluster(orinaAntT_0[, colnames(orinaAntT_0) != "numVol"])
checkCluster(orinaAntT_F[, colnames(orinaAntT_F) != "numVol"])

### Performing kmeans, PAM and fuzzy ----

clusterVarios()

### Performing model based clustering ----

orinaAnt_Factors_T0 <- subset(orinaAntFactors, Tiempo == "0")
orinaAnt_Factors_TF <- subset(orinaAntFactors, Tiempo == "Final")

model_clustering_OFT_0 <- Mclust(orinaAntT_0[, colnames(orinaAntT_0) != "numVol"])
model_clustering_OFT_F <- Mclust(orinaAntT_F[, colnames(orinaAntT_F) != "numVol"])

### Plotting results of model based clustering 

p1 <- fviz_mclust(object = model_clustering_OFT_0, what = "BIC", pallete = "jco") +
  scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OFT_0, what = "classification", geom = "point",
                  pallete = "jco")

p3 <- fviz_mclust(object = model_clustering_OFT_F, what = "BIC", pallete = "jco") +
  scale_x_discrete(limits = c(1:10))

p4 <- fviz_mclust(model_clustering_OFT_F, what = "classification", geom = "point",
                  pallete = "jco")


ggarrange(p1,p3)
ggarrange(p2,p4)

### Rejoining factors to dataframe

orinaAntNum_0_clusters <- cbind(orinaAntT_0, clusters = as.factor(model_clustering_OFT_0$classification),
                                 Endulzante = orinaAnt_Factors_T0$Endulzante, 
                                 Sexo = orinaAnt_Factors_T0$Sexo)
orinaAntNum_F_clusters <- cbind(orinaAntT_F, clusters = as.factor(model_clustering_OFT_F$classification),
                                 Endulzante = orinaAnt_Factors_TF$Endulzante, 
                                 Sexo = orinaAnt_Factors_TF$Sexo)

### Scaling factors to plot them. 

orinaAntNum_0_clusters$Endulzante <- rescale(as.numeric(orinaAntNum_0_clusters$Endulzante))
orinaAntNum_0_clusters$Sexo <- rescale(as.numeric(orinaAntNum_0_clusters$Sexo))

orinaAntNum_F_clusters$Endulzante <- rescale(as.numeric(orinaAntNum_F_clusters$Endulzante))
orinaAntNum_F_clusters$Sexo <- rescale(as.numeric(orinaAntNum_F_clusters$Sexo))

### Boxplot to explain clusters ----


boxplot(subset(orinaAntNum_0_clusters, clusters==1, select=-c(clusters,numVol)), 
        main = "Cluster 1 Orina Ant Tiempo 0")
boxplot(subset(orinaAntNum_0_clusters, clusters==2, select=-c(clusters,numVol)), 
        main = "Cluster 2 Orina Ant Tiempo 0")
boxplot(subset(orinaAntNum_0_clusters, clusters==3, select=-c(clusters,numVol)), 
        main = "Cluster 3 Orina Ant Tiempo 0")

boxplot(subset(orinaAntNum_F_clusters, clusters==1, select=-c(clusters,numVol)), 
        main = "Cluster 1 Orina Ant Tiempo F")
boxplot(subset(orinaAntNum_F_clusters, clusters==2, select=-c(clusters,numVol)), 
        main = "Cluster 2 Orina Ant Tiempo F")
boxplot(subset(orinaAntNum_F_clusters, clusters==3, select=-c(clusters,numVol)), 
        main = "Cluster 3 Orina Ant Tiempo F")

### Explicando clusters

orinaAntNum_0_clusters <- cbind(orinaAntT_0, clusters = as.factor(model_clustering_OFT_0$classification),
                                Endulzante = orinaAnt_Factors_T0$Endulzante, 
                                Sexo = orinaAnt_Factors_T0$Sexo)
orinaAntNum_F_clusters <- cbind(orinaAntT_F, clusters = as.factor(model_clustering_OFT_F$classification),
                                Endulzante = orinaAnt_Factors_TF$Endulzante, 
                                Sexo = orinaAnt_Factors_TF$Sexo)

p <- ggparcoord(data = orinaAntNum_0_clusters[, colnames(orinaAntT_0) != "numVol"], groupColumn = "clusters", 
                scale = "std", columns = c(1:12,14,15)) + 
                labs(x = "variables", y = "value (in standard-deviation units)", title = "Clustering Tiempo 0")


p1 <- ggparcoord(data = orinaAntNum_F_clusters[, colnames(orinaAntT_F) != "numVol"], groupColumn = "clusters", 
                 scale = "std", columns = c(1:12,14,15)) + 
                 labs(x = "variables", 
                 y = "value (in standard-deviation units)", 
                 title = "Clustering Tiempo F")

ggarrange(p,p1)

## ANOVA ----

#### ANOVA paired ----

orinaAntDupl <- orinaAntFactors[duplicated(orinaAntFactors$numVol) == TRUE,]

anova_pareado <- aov(formula = NS ~ Sexo + Endulzante + Tiempo + Sexo*Endulzante + Error(numVol/Tiempo),
                     data = orinaAntDupl)
summary(anova_pareado)

#### ANOVA dos/tres vias ----

anova_no_pareado <- aov(formula = NS ~ Sexo + Endulzante, 
                        data = orinaAnt_Factors)
summary(anova_no_pareado)


# Plasma antocianos ----

## Preprocess ----

plasmaAnt <- preprocessTablas("data/", "cronicoplasmaAnt_Antro.csv")

plasmaAntFactors <- plasmaAnt$tablaFactors
plasmaAntNum <- plasmaAnt$tablaNum

plasmaAntT_0 <- plasmaAnt$tabla_Tiempo0
plasmaAntT_F <- plasmaAnt$tabla_TiempoF

## PCA ----

pcaVarios(plasmaAntFactors, "Plasma Flavonoides")

## Clustering ----

### Checking clustering ----

checkCluster(plasmaAntT_0[, colnames(plasmaAntT_0) != "numVol"])
checkCluster(plasmaAntT_F[, colnames(plasmaAntT_F) != "numVol"])

### Performing kmeans, PAM and fuzzy ----

clusterVarios()

### Performing model based clustering ----

plasmaAnt_Factors_T0 <- subset(plasmaAntFactors, Tiempo == "0")
plasmaAnt_Factors_TF <- subset(plasmaAntFactors, Tiempo == "Final")

model_clustering_OFT_0 <- Mclust(plasmaAntT_0[, colnames(plasmaAntT_0) != "numVol"])
model_clustering_OFT_F <- Mclust(plasmaAntT_F[, colnames(plasmaAntT_F) != "numVol"])

### Plotting results of model based clustering 

p1 <- fviz_mclust(object = model_clustering_OFT_0, what = "BIC", pallete = "jco") +
  scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OFT_0, what = "classification", geom = "point",
                  pallete = "jco")

p3 <- fviz_mclust(object = model_clustering_OFT_F, what = "BIC", pallete = "jco") +
  scale_x_discrete(limits = c(1:10))

p4 <- fviz_mclust(model_clustering_OFT_F, what = "classification", geom = "point",
                  pallete = "jco")


ggarrange(p1,p3)
ggarrange(p2,p4)

### Rejoining factors to dataframe

plasmaAntNum_0_clusters <- cbind(plasmaAntT_0, clusters = as.factor(model_clustering_OFT_0$classification),
                                Endulzante = plasmaAnt_Factors_T0$Endulzante, 
                                Sexo = plasmaAnt_Factors_T0$Sexo)
plasmaAntNum_F_clusters <- cbind(plasmaAntT_F, clusters = as.factor(model_clustering_OFT_F$classification),
                                Endulzante = plasmaAnt_Factors_TF$Endulzante, 
                                Sexo = plasmaAnt_Factors_TF$Sexo)

### Scaling factors to plot them. 

plasmaAntNum_0_clusters$Endulzante <- rescale(as.numeric(plasmaAntNum_0_clusters$Endulzante))
plasmaAntNum_0_clusters$Sexo <- rescale(as.numeric(plasmaAntNum_0_clusters$Sexo))

plasmaAntNum_F_clusters$Endulzante <- rescale(as.numeric(plasmaAntNum_F_clusters$Endulzante))
plasmaAntNum_F_clusters$Sexo <- rescale(as.numeric(plasmaAntNum_F_clusters$Sexo))

### Boxplot to explain clusters ----

boxplot(subset(plasmaAntNum_0_clusters, clusters==1, select=-c(clusters,numVol)), 
        main = "Cluster 1 Plasma Ant Tiempo 0")
boxplot(subset(plasmaAntNum_0_clusters, clusters==2, select=-c(clusters,numVol)), 
        main = "Cluster 2 Plama Ant Tiempo 0")

boxplot(subset(plasmaAntNum_F_clusters, clusters==1, select=-c(clusters,numVol)), 
        main = "Cluster 1 Plasma Ant Tiempo F")
boxplot(subset(plasmaAntNum_F_clusters, clusters==2, select=-c(clusters,numVol)), 
        main = "Cluster 2 Plasma Ant Tiempo F")

### Explicando clusters

plasmaAntNum_0_clusters <- cbind(plasmaAntT_0, clusters = as.factor(model_clustering_OFT_0$classification),
                                Endulzante = plasmaAnt_Factors_T0$Endulzante, 
                                Sexo = plasmaAnt_Factors_T0$Sexo)
plasmaAntNum_F_clusters <- cbind(plasmaAntT_F, clusters = as.factor(model_clustering_OFT_F$classification),
                                Endulzante = plasmaAnt_Factors_TF$Endulzante, 
                                Sexo = plasmaAnt_Factors_TF$Sexo)

p <- ggparcoord(data = plasmaAntNum_0_clusters[, colnames(plasmaAntT_0) != "numVol"], groupColumn = "clusters", 
                scale = "std", columns = c(1:13,15,16)) + 
  labs(x = "variables", y = "value (in standard-deviation units)", title = "Clustering Tiempo 0")


p1 <- ggparcoord(data = plasmaAntNum_F_clusters[, colnames(plasmaAntT_F) != "numVol"], groupColumn = "clusters", 
                 scale = "std", columns = c(1:13,15,16)) + 
  labs(x = "variables", 
       y = "value (in standard-deviation units)", 
       title = "Clustering Tiempo F")

ggarrange(p,p1)

## ANOVA ----

#### ANOVA paired ----

plasmaAntDupl <- plasmaAntFactors[duplicated(plasmaAntFactors$numVol) == TRUE,]

anova_pareado <- aov(formula = NS ~ Sexo + Endulzante + Tiempo + Sexo*Endulzante + Error(numVol/Tiempo),
                     data = plasmaAntDupl)
summary(anova_pareado)

#### ANOVA dos/tres vias ----

anova_no_pareado <- aov(formula = NS ~ Sexo + Endulzante, 
                        data = plasmaAnt_Factors)
summary(anova_no_pareado)

