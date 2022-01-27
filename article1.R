library(tidyverse)
library(rstatix)
library(scales)
library(ggpubr)
library(factoextra)
library(GGally)
library(plotly)
library(FactoMineR)
library(cluster)
library(mclust)
library(reshape2)
library(gridExtra)
library(QuantPsyc)
library(energy)


preprocessTablas <- function(root, nombreTabla) {
  
  # Getting data ready
  rootTabla <- paste0(root, nombreTabla)
  
  tabla <- read.csv(rootTabla)
  
  
  # Make factors of categorical features
  
  tabla$Endulzante <- factor(tabla$Endulzante, levels = c("SA", "ST", "SU"))
  tabla$Sexo <- factor(tabla$Sexo, levels = c("HOMBRE", "MUJER"))
  tabla$Tiempo <- factor(tabla$Tiempo, levels = c("0", "Final"))
  tabla$numVol <- factor(tabla$numVol)
  
  for (i in seq(1:nrow(tabla))){
    if (tabla$Tiempo[i] == "0"){
      tabla$Peso[i] = tabla$Peso.inicial[i]
      tabla$IMC[i] = tabla$IMC.Inicial[i]
      tabla$Grasa[i] = tabla$Grasa.inicial[i]
      tabla$IRCV[i] = tabla$IRCV.inicial[i]
      tabla$Bpmin[i] = tabla$Bpmin.inicial[i]
      tabla$Bpmax[i] = tabla$Bpmax.inicial[i]
      tabla$Frec[i] = tabla$Frec.inicial[i]
      
    }
    
    else if (tabla$Tiempo[i] == "Final"){
      tabla$Peso[i] = tabla$Peso.final[i]
      tabla$IMC[i] = tabla$IMC.Final[i]
      tabla$Grasa[i] = tabla$Grasa.final[i]
      tabla$IRCV[i] = tabla$IRCV.Final[i]
      tabla$Bpmin[i] = tabla$Bpmin.final[i]
      tabla$Bpmax[i] = tabla$Bpmax.final[i]
      tabla$Frec[i] = tabla$Frec.final[i]
    }
    
    
  }
  
  # Removing of trivial redundant and useless features 
  
  set.A <- subset(tabla, select =-c(X.1, X, Peso.inicial, Peso.final, Delta.Peso, Talla, IMC.Inicial, IMC.Final, 
                                    Delta.IMC, Grasa.inicial, Grasa.final, Delta.Grasa, IRCV.Final, IRCV.inicial, 
                                    Bpmin.final, Bpmin.inicial, Bpmax.final, Bpmax.inicial, Frec.final, Frec.inicial))
  
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

# Orina Flavonoides  ----

## Preprocess ----

orinaFlav <- preprocessTablas("data/", "tablaOrinaFlav.csv")
orinaFlavFactors <- orinaFlav$tablaFactors
orinaFlavNum <- orinaFlav$tablaNum

orinaFlavT_0 <- orinaFlav$tabla_Tiempo0
orinaFlavT_F <- orinaFlav$tabla_TiempoF

## PCA ----

pcaVarios(orinaFlavFactors, "orina flavonoides")

## Clustering ----

### Checking clustering ----

checkCluster(orinaFlavT_0[, colnames(orinaFlavT_0) != "numVol"])
checkCluster(orinaFlavT_F[, colnames(orinaFlavT_F) != "numVol"])

### Performing kmeans, PAM and fuzzy ----

clusterVarios()

### Performing model based clustering ----

## All times

model_clustering_OF <- Mclust(orinaFlav$tablaNum)

p1 <- fviz_mclust(object = model_clustering_OF, what = "BIC", pallete = "jco",  
                  title = "Model Selection Orina Flav") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OF, what = "classification", geom = "point",
                  title = "Clusters Plot Orina Flav", pallete = "jco")

ggarrange(p1,p2)


orinaFlav_clusters <- cbind(orinaFlav$tablaNum, clusters = as.factor(model_clustering_OF$classification),
                                 Endulzante = orinaFlavFactors$Endulzante, 
                                 Sexo = orinaFlavFactors$Sexo,
                                 Tiempo = orinaFlavFactors$Tiempo)

tableSexoOF <- table(orinaFlav_clusters$Sexo,orinaFlav_clusters$clusters)
tableEdulcoranteOF <- table(orinaFlav_clusters$Endulzante,orinaFlav_clusters$clusters)

orinaFlav_clusters$Endulzante <- rescale(as.numeric(orinaFlav_clusters$Endulzante))
orinaFlav_clusters$Sexo <- rescale(as.numeric(orinaFlav_clusters$Sexo))
orinaFlav_clusters$Tiempo <- rescale(as.numeric(orinaFlav_clusters$Tiempo))

longtableOF <- melt(orinaFlav_clusters, id = c("clusters", "Tiempo"))

ggplot(longtableOF, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 14, y = 1.03, label = "Mujer") + 
  annotate("text",x = 14, y = -0.03, label = "Hombre") +
  annotate("text", x = 13, y = 1.03, label = "SU") + 
  annotate("text",x = 13, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexoOF, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 14,xmax=17, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcoranteOF, theme = ttheme_default(base_size = 8)), xmin= 14,xmax=17, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Orina Flavonoids")+
  labs(y = "standarized value", x = "variables/clusters")+
  facet_wrap(~Tiempo)

## Separating by time

orinaFlav_Factors_T0 <- subset(orinaFlavFactors, Tiempo == "0")
orinaFlav_Factors_TF <- subset(orinaFlavFactors, Tiempo == "Final")

model_clustering_OFT_0 <- Mclust(orinaFlavT_0[, colnames(orinaFlavT_0) != "numVol"])
model_clustering_OFT_F <- Mclust(orinaFlavT_F[, colnames(orinaFlavT_F) != "numVol"])

### Plotting results of model based clustering 

p1 <- fviz_mclust(object = model_clustering_OFT_0, what = "BIC", pallete = "jco",  
                  title = "Model Selection Orina Flav T 0") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OFT_0, what = "classification", geom = "point",
                  title = "Clusters Plot Orina Flav T 0", pallete = "jco")

p3 <- fviz_mclust(object = model_clustering_OFT_F, what = "BIC", pallete = "jco", 
                  title = "Model Selection Orina Flav T F") + scale_x_discrete(limits = c(1:10))

p4 <- fviz_mclust(model_clustering_OFT_F, what = "classification", geom = "point", 
                  title = "Clusters plot Orina Flav T F", pallete = "jco")


ggarrange(p1,p3)
ggarrange(p2,p4)

### Rejoining factors to dataframe

orinaFlavNum_0_clusters <- cbind(orinaFlavT_0, clusters = as.factor(model_clustering_OFT_0$classification),
                                 Endulzante = orinaFlav_Factors_T0$Endulzante, 
                                 Sexo = orinaFlav_Factors_T0$Sexo)
orinaFlavNum_F_clusters <- cbind(orinaFlavT_F, clusters = as.factor(model_clustering_OFT_F$classification),
                                 Endulzante = orinaFlav_Factors_TF$Endulzante, 
                                 Sexo = orinaFlav_Factors_TF$Sexo)

### Counting factors to plot them

tableSexo0 <- table(orinaFlavNum_0_clusters$Sexo,orinaFlavNum_0_clusters$clusters)
tableEdulcorante0 <- table(orinaFlavNum_0_clusters$Endulzante,orinaFlavNum_0_clusters$clusters)

tableSexoF <- table(orinaFlavNum_F_clusters$Sexo,orinaFlavNum_F_clusters$clusters)
tableEdulcoranteF <- table(orinaFlavNum_F_clusters$Endulzante,orinaFlavNum_F_clusters$clusters)


### Scaling factors to plot them. SA = 0.0, ST = 0.5, SU = 1; Hombre = 0, Mujer = 1


orinaFlavNum_0_clusters$Endulzante <- rescale(as.numeric(orinaFlavNum_0_clusters$Endulzante))
orinaFlavNum_0_clusters$Sexo <- rescale(as.numeric(orinaFlavNum_0_clusters$Sexo))

orinaFlavNum_F_clusters$Endulzante <- rescale(as.numeric(orinaFlavNum_F_clusters$Endulzante))
orinaFlavNum_F_clusters$Sexo <- rescale(as.numeric(orinaFlavNum_F_clusters$Sexo))

### Boxplot to explain clusters ----

#### Reformatting for boxplot

longtableT_0 <-melt(orinaFlavNum_0_clusters[,colnames(orinaFlavNum_0_clusters)!="numVol"], id = "clusters")

longtableT_F <-melt(orinaFlavNum_F_clusters[,colnames(orinaFlavNum_0_clusters)!="numVol"], id = "clusters")

#### Boxplotting all together

ggplot(longtableT_0, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 14, y = 1.03, label = "Mujer") + 
  annotate("text",x = 14, y = -0.03, label = "Hombre") +
  annotate("text", x = 13, y = 1.03, label = "SU") + 
  annotate("text",x = 13, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexo0, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 13,xmax=17, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcorante0, theme = ttheme_default(base_size = 8)), xmin= 13,xmax=17, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Orina Flavonoids T 0")+
  labs(y = "standarized value", x = "variables/clusters")

ggplot(longtableT_F, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 14, y = 1.03, label = "Mujer") + 
  annotate("text",x = 14, y = -0.03, label = "Hombre") +
  annotate("text", x = 13, y = 1.03, label = "SU") + 
  annotate("text",x = 13, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexoF, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 13,xmax=17, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcoranteF, theme = ttheme_default(base_size = 8)), xmin= 13,xmax=17, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Orina Flavonoids T F")+
  labs(y = "standarized value", x = "variables/clusters")


### Explainig clusters with parallell coordinates plot----

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


## ANOVA paired ----

counts <- data.frame(table(orinaFlavFactors$numVol))

orinaFlavDupl <- orinaFlavFactors[orinaFlavFactors$numVol %in% counts$Var1[counts$Freq > 1],]

### Datos metabólicos ----

anova_pareado_EG <- aov(formula = EG ~ Sexo * Endulzante * Tiempo +
                     Error(numVol/Tiempo),
                     data = orinaFlavDupl)

summary(anova_pareado_EG)


anova_pareado_ES <- aov(formula = ES ~ Sexo * Endulzante * Tiempo +
                          Error(numVol/Tiempo),
                        data = orinaFlavDupl)

summary(anova_pareado_ES)

anova_pareado_HE.G <- aov(formula = HE.G ~Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                        data = orinaFlavDupl)
summary(anova_pareado_HE.G)

anova_pareado_NG <- aov(formula = NG ~ Sexo * Endulzante * Tiempo +
                          Error(numVol/Tiempo),
                        data = orinaFlavDupl)


summary(anova_pareado_NG)

anova_pareado_NS <- aov(formula = NS ~ Sexo * Endulzante * Tiempo +
                          Error(numVol/Tiempo),
                        data = orinaFlavDupl)


summary(anova_pareado_NS)

### Datos antro ----

anova_pareado_Peso <- aov(formula = Peso ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                        data = orinaFlavDupl)


summary(anova_pareado_Peso)

anova_pareado_IMC <- aov(formula = IMC ~ Sexo * Endulzante * Tiempo +
                           Error(numVol/Tiempo),
                          data = orinaFlavDupl)


summary(anova_pareado_IMC)

anova_pareado_Grasa <- aov(formula = Grasa ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                         data = orinaFlavDupl)


summary(anova_pareado_Grasa)

anova_pareado_IRCV <- aov(formula = IRCV ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                            data = orinaFlavDupl)


summary(anova_pareado_IRCV)

anova_pareado_Bpmin <- aov(formula = Bpmin ~ Sexo * Endulzante * Tiempo +
                           Error(numVol/Tiempo),
                           data = orinaFlavDupl)


summary(anova_pareado_Bpmin)

anova_pareado_Bpmax <- aov(formula = Bpmax ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = orinaFlavDupl)


summary(anova_pareado_Bpmax)

anova_pareado_Frec <- aov(formula = Frec ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                           data = orinaFlavDupl)


summary(anova_pareado_Frec)

## Modelos ----



# Orina Antocianos ---- 


## Preprocess ----

orinaAnt <- preprocessTablas("data/", "tablaOrinaAnt.csv")

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

## All times

model_clustering_OA <- Mclust(orinaAnt$tablaNum)

p1 <- fviz_mclust(object = model_clustering_OA, what = "BIC", pallete = "jco",  
                  title = "Model Selection Orina ant") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OA, what = "classification", geom = "point",
                  title = "Clusters Plot Orina ant", pallete = "jco")

ggarrange(p1,p2)


orinaAnt_clusters <- cbind(orinaAnt$tablaNum, clusters = as.factor(model_clustering_OA$classification),
                            Endulzante = orinaAntFactors$Endulzante, 
                            Sexo = orinaAntFactors$Sexo,
                            Tiempo = orinaAntFactors$Tiempo)

tableSexoOA <- table(orinaAnt_clusters$Sexo,orinaAnt_clusters$clusters)
tableEdulcoranteOA <- table(orinaAnt_clusters$Endulzante,orinaAnt_clusters$clusters)

orinaAnt_clusters$Endulzante <- rescale(as.numeric(orinaAnt_clusters$Endulzante))
orinaAnt_clusters$Sexo <- rescale(as.numeric(orinaAnt_clusters$Sexo))
orinaAnt_clusters$Tiempo <- rescale(as.numeric(orinaAnt_clusters$Tiempo))

longtableOA <- melt(orinaAnt_clusters, id = c("clusters", "Tiempo"))

ggplot(longtableOA, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  facet_wrap(~Tiempo)+
  annotate("text", x = 14, y = 1.03, label = "Mujer") + 
  annotate("text",x = 14, y = -0.03, label = "Hombre") +
  annotate("text", x = 13, y = 1.03, label = "SU") + 
  annotate("text",x = 13, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexoOA, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 13,xmax=17, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcoranteOA, theme = ttheme_default(base_size = 8)), xmin= 13,xmax=17, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Orina antocians")+
  labs(y = "standarized value", x = "variables/clusters")

### Selecting by time

orinaAnt_Factors_T0 <- subset(orinaAntFactors, Tiempo == "0")
orinaAnt_Factors_TF <- subset(orinaAntFactors, Tiempo == "Final")

model_clustering_OFT_0 <- Mclust(orinaAntT_0[, colnames(orinaAntT_0) != "numVol"])
model_clustering_OFT_F <- Mclust(orinaAntT_F[, colnames(orinaAntT_F) != "numVol"])

### Plotting results of model based clustering 

p1 <- fviz_mclust(object = model_clustering_OFT_0, what = "BIC", pallete = "jco", 
                  title = "Model Selection Orina Ant T 0") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OFT_0, what = "classification", geom = "point",
                  title = "Clusters Plot Orina Ant T 0", pallete = "jco")

p3 <- fviz_mclust(object = model_clustering_OFT_F, what = "BIC", pallete = "jco",
                  title = "Model Selection Orina Ant T F") + scale_x_discrete(limits = c(1:10))

p4 <- fviz_mclust(model_clustering_OFT_F, what = "classification", geom = "point",
                  title = "Clusters Plot Orina Ant T F", pallete = "jco")


ggarrange(p1,p3)
ggarrange(p2,p4)

### Rejoining factors to dataframe

orinaAntNum_0_clusters <- cbind(orinaAntT_0, clusters = as.factor(model_clustering_OFT_0$classification),
                                 Endulzante = orinaAnt_Factors_T0$Endulzante, 
                                 Sexo = orinaAnt_Factors_T0$Sexo)
orinaAntNum_F_clusters <- cbind(orinaAntT_F, clusters = as.factor(model_clustering_OFT_F$classification),
                                 Endulzante = orinaAnt_Factors_TF$Endulzante, 
                                 Sexo = orinaAnt_Factors_TF$Sexo)

### Counting factors to plot them

tableSexo0 <- table(orinaAntNum_0_clusters$Sexo,orinaAntNum_0_clusters$clusters)
tableEdulcorante0 <- table(orinaAntNum_0_clusters$Endulzante,orinaAntNum_0_clusters$clusters)

tableSexoF <- table(orinaAntNum_F_clusters$Sexo,orinaAntNum_F_clusters$clusters)
tableEdulcoranteF <- table(orinaAntNum_F_clusters$Endulzante,orinaAntNum_F_clusters$clusters)


### Scaling factors to plot them. SA = 0.0, ST = 0.5, SU = 1; Hombre = 0, Mujer = 1


orinaAntNum_0_clusters$Endulzante <- rescale(as.numeric(orinaAntNum_0_clusters$Endulzante))
orinaAntNum_0_clusters$Sexo <- rescale(as.numeric(orinaAntNum_0_clusters$Sexo))

orinaAntNum_F_clusters$Endulzante <- rescale(as.numeric(orinaAntNum_F_clusters$Endulzante))
orinaAntNum_F_clusters$Sexo <- rescale(as.numeric(orinaAntNum_F_clusters$Sexo))

### Boxplot to explain clusters ----

#### Reformatting for boxplot

longtableT_0 <-melt(orinaAntNum_0_clusters[,colnames(orinaAntNum_0_clusters)!="numVol"], id = "clusters")

longtableT_F <-melt(orinaAntNum_F_clusters[,colnames(orinaAntNum_0_clusters)!="numVol"], id = "clusters")

#### Boxplotting all together

ggplot(longtableT_0, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 14, y = 1.03, label = "Mujer") + 
  annotate("text",x = 14, y = -0.03, label = "Hombre") +
  annotate("text", x = 13, y = 1.03, label = "SU") + 
  annotate("text",x = 13, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexo0, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 13,xmax=17, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcorante0, theme = ttheme_default(base_size = 8)), xmin= 13,xmax=17, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Orina Antocians T 0")+
  labs(y = "standarized value", x = "variables/clusters")

ggplot(longtableT_F, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 14, y = 1.03, label = "Mujer") + 
  annotate("text",x = 14, y = -0.03, label = "Hombre") +
  annotate("text", x = 13, y = 1.03, label = "SU") + 
  annotate("text",x = 13, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexoF, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 13,xmax=17, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcoranteF, theme = ttheme_default(base_size = 8)), xmin= 13,xmax=17, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Orina Antocians T F")+
  labs(y = "standarized value", x = "variables/clusters")


### Explainig clusters with parallell coordinates plot----

p <- ggparcoord(data = orinaAntNum_0_clusters[, colnames(orinaAntT_0) != "numVol"], groupColumn = "clusters", 
                scale = "std", columns = c(1,2,3,4,5,6,7,8,9,10,11,12,14,15)) + 
  labs(x = "variables", 
       y = "value (in standard-deviation units)", 
       title = "Clustering Tiempo 0")

p1 <- ggparcoord(data = orinaAntNum_F_clusters[, colnames(orinaAntT_F) != "numVol"], groupColumn = "clusters", 
                 scale = "std", columns = c(1,2,3,4,5,6,7,8,9,10,11,12,14,15)) + 
  labs(x = "variables", 
       y = "value (in standard-deviation units)", 
       title = "Clustering Tiempo F")

ggarrange(p,p1)

## ANOVA paired ----

counts <- data.frame(table(orinaAntFactors$numVol))

orinaAntDupl <- orinaAntFactors[orinaAntFactors$numVol %in% counts$Var1[counts$Freq > 1],]

### Datos metabólicos ----

anova_pareado_CA.Gluc <- aov(formula = CA.Gluc~ Sexo * Endulzante * Tiempo +
                               Error(numVol/Tiempo),
                        data = orinaAntDupl)

summary(anova_pareado_CA.Gluc)

anova_pareado_DHPAA.Gluc <- aov(formula = DHPAA.Gluc ~ Sexo * Endulzante * Tiempo +
                                  Error(numVol/Tiempo),
                        data = orinaAntDupl)

summary(anova_pareado_DHPAA.Gluc)

anova_pareado_TFA.Gluc <- aov(formula = TFA.Gluc ~ Sexo * Endulzante * Tiempo +
                                Error(numVol/Tiempo),
                          data = orinaAntDupl)

summary(anova_pareado_TFA.Gluc)

anova_pareado_TFA.Sulfate <- aov(formula = TFA.Sulfate  ~ Sexo * Endulzante * Tiempo +
                                   Error(numVol/Tiempo),
                                data = orinaAntDupl)

summary(anova_pareado_TFA.Sulfate)

anova_pareado_Ácido.Vanílico..VA. <- aov(formula = Ácido.Vanílico..VA.  ~ Sexo * Endulzante * Tiempo +
                                           Error(numVol/Tiempo),
                                        data = orinaAntDupl)


summary(anova_pareado_Ácido.Vanílico..VA.)

### Datos antro ----

anova_pareado_Peso <- aov(formula = Peso ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = orinaAntDupl)


summary(anova_pareado_Peso)

anova_pareado_IMC <- aov(formula = IMC ~ Sexo * Endulzante * Tiempo +
                           Error(numVol/Tiempo),
                         data = orinaAntDupl)


summary(anova_pareado_IMC)

anova_pareado_Grasa <- aov(formula = Grasa ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = orinaAntDupl)


summary(anova_pareado_Grasa)

anova_pareado_IRCV <- aov(formula = IRCV ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = orinaAntDupl)


summary(anova_pareado_IRCV)

anova_pareado_Bpmin <- aov(formula = Bpmin ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = orinaAntDupl)


summary(anova_pareado_Bpmin)

anova_pareado_Bpmax <- aov(formula = Bpmax ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = orinaAntDupl)


summary(anova_pareado_Bpmax)

anova_pareado_Frec <- aov(formula = Frec ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = orinaAntDupl)


summary(anova_pareado_Frec)
# Plasma antocianos ----

## Preprocess ----

plasmaAnt <- preprocessTablas("data/", "tablaplasmaAnt.csv")

plasmaAntFactors <- plasmaAnt$tablaFactors
plasmaAntNum <- plasmaAnt$tablaNum

plasmaAntT_0 <- plasmaAnt$tabla_Tiempo0
plasmaAntT_F <- plasmaAnt$tabla_TiempoF

## PCA ----

pcaVarios(plasmaAntFactors, "Plasma Antocianos")

## Clustering ----

### Checking clustering ----

checkCluster(plasmaAntT_0[, colnames(plasmaAntT_0) != "numVol"])
checkCluster(plasmaAntT_F[, colnames(plasmaAntT_F) != "numVol"])

### Performing kmeans, PAM and fuzzy ----

clusterVarios()

### Performing model based clustering ----

## All times

model_clustering_PA <- Mclust(plasmaAnt$tablaNum)

p1 <- fviz_mclust(object = model_clustering_PA, what = "BIC", pallete = "jco",  
                  title = "Model Selection plasma ant") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_PA, what = "classification", geom = "point",
                  title = "Clusters Plot plasma ant", pallete = "jco")

ggarrange(p1,p2)


plasmaAnt_clusters <- cbind(plasmaAnt$tablaNum, clusters = as.factor(model_clustering_PA$classification),
                           Endulzante = plasmaAntFactors$Endulzante, 
                           Sexo = plasmaAntFactors$Sexo,
                           Tiempo = plasmaAntFactors$Tiempo)

tableSexoPA <- table(plasmaAnt_clusters$Sexo,plasmaAnt_clusters$clusters)
tableEdulcorantePA <- table(plasmaAnt_clusters$Endulzante,plasmaAnt_clusters$clusters)

plasmaAnt_clusters$Endulzante <- rescale(as.numeric(plasmaAnt_clusters$Endulzante))
plasmaAnt_clusters$Sexo <- rescale(as.numeric(plasmaAnt_clusters$Sexo))
plasmaAnt_clusters$Tiempo <- rescale(as.numeric(plasmaAnt_clusters$Tiempo))

longtablePA <- melt(plasmaAnt_clusters, id = c("clusters", "Tiempo"))

ggplot(longtablePA, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  facet_wrap(~Tiempo)+
  annotate("text", x = 15, y = 1.03, label = "Mujer") + 
  annotate("text",x = 15, y = -0.03, label = "Hombre") +
  annotate("text", x = 14, y = 1.03, label = "SU") + 
  annotate("text",x = 14, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexoPA, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 15,xmax=17, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcorantePA, theme = ttheme_default(base_size = 8)), xmin= 15,xmax=17, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis plasma ant")+
  labs(y = "standarized value", x = "variables/clusters")


### Selecting by time

plasmaAnt_Factors_T0 <- subset(plasmaAntFactors, Tiempo == "0")
plasmaAnt_Factors_TF <- subset(plasmaAntFactors, Tiempo == "Final")

model_clustering_OFT_0 <- Mclust(plasmaAntT_0[, colnames(plasmaAntT_0) != "numVol"])
model_clustering_OFT_F <- Mclust(plasmaAntT_F[, colnames(plasmaAntT_F) != "numVol"])


### Plotting results of model based clustering 

p1 <- fviz_mclust(object = model_clustering_OFT_0, what = "BIC", pallete = "jco", 
                  title = "Model Selection Plasma Ant T 0") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OFT_0, what = "classification", geom = "point",
                  title = "Clusters Plot Plasma Ant T 0", pallete = "jco")

p3 <- fviz_mclust(object = model_clustering_OFT_F, what = "BIC", pallete = "jco",
                  title = "Model Selection Plasma Ant T F") + scale_x_discrete(limits = c(1:10))

p4 <- fviz_mclust(model_clustering_OFT_F, what = "classification", geom = "point",
                  title = "Clusters Plot Plasma Ant T F", pallete = "jco")


ggarrange(p1,p3)
ggarrange(p2,p4)

### Rejoining factors to dataframe

plasmaAntNum_0_clusters <- cbind(plasmaAntT_0, clusters = as.factor(model_clustering_OFT_0$classification),
                                Endulzante = plasmaAnt_Factors_T0$Endulzante, 
                                Sexo = plasmaAnt_Factors_T0$Sexo)
plasmaAntNum_F_clusters <- cbind(plasmaAntT_F, clusters = as.factor(model_clustering_OFT_F$classification),
                                Endulzante = plasmaAnt_Factors_TF$Endulzante, 
                                Sexo = plasmaAnt_Factors_TF$Sexo)

### Counting factors to plot them

tableSexo0 <- table(plasmaAntNum_0_clusters$Sexo,plasmaAntNum_0_clusters$clusters)
tableEdulcorante0 <- table(plasmaAntNum_0_clusters$Endulzante,plasmaAntNum_0_clusters$clusters)

tableSexoF <- table(plasmaAntNum_F_clusters$Sexo,plasmaAntNum_F_clusters$clusters)
tableEdulcoranteF <- table(plasmaAntNum_F_clusters$Endulzante,plasmaAntNum_F_clusters$clusters)


### Scaling factors to plot them. SA = 0.0, ST = 0.5, SU = 1; Hombre = 0, Mujer = 1


plasmaAntNum_0_clusters$Endulzante <- rescale(as.numeric(plasmaAntNum_0_clusters$Endulzante))
plasmaAntNum_0_clusters$Sexo <- rescale(as.numeric(plasmaAntNum_0_clusters$Sexo))

plasmaAntNum_F_clusters$Endulzante <- rescale(as.numeric(plasmaAntNum_F_clusters$Endulzante))
plasmaAntNum_F_clusters$Sexo <- rescale(as.numeric(plasmaAntNum_F_clusters$Sexo))

### Boxplot to explain clusters ----

#### Reformatting for boxplot

longtableT_0 <-melt(plasmaAntNum_0_clusters[,colnames(plasmaAntNum_0_clusters)!="numVol"], id = "clusters")

longtableT_F <-melt(plasmaAntNum_F_clusters[,colnames(plasmaAntNum_0_clusters)!="numVol"], id = "clusters")

#### Boxplotting all together

ggplot(longtableT_0, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 15, y = 1.03, label = "Mujer") + 
  annotate("text",x = 15, y = -0.03, label = "Hombre") +
  annotate("text", x = 14, y = 1.03, label = "SU") + 
  annotate("text",x = 14, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexo0, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 15,xmax=17, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcorante0, theme = ttheme_default(base_size = 8)), xmin= 15,xmax=17, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Plasma Ant T 0")+
  labs(y = "standarized value", x = "variables/clusters")

ggplot(longtableT_F, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 15, y = 1.03, label = "Mujer") + 
  annotate("text",x = 15, y = -0.03, label = "Hombre") +
  annotate("text", x = 14, y = 1.03, label = "SU") + 
  annotate("text",x = 14, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexoF, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 15,xmax=17, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcoranteF, theme = ttheme_default(base_size = 8)), xmin= 15,xmax=17, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Plasma Ant T F")+
  labs(y = "standarized value", x = "variables/clusters")


### Explainig clusters with parallell coordinates plot----

p <- ggparcoord(data = plasmaAntNum_0_clusters[, colnames(plasmaAntT_0) != "numVol"], groupColumn = "clusters", 
                scale = "std", columns = c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)) + 
  labs(x = "variables", 
       y = "value (in standard-deviation units)", 
       title = "Clustering Tiempo 0")

p1 <- ggparcoord(data = plasmaAntNum_F_clusters[, colnames(plasmaAntT_F) != "numVol"], groupColumn = "clusters", 
                 scale = "std", columns = c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)) + 
  labs(x = "variables", 
       y = "value (in standard-deviation units)", 
       title = "Clustering Tiempo F")

ggarrange(p,p1)

## ANOVA paired ----

counts <- data.frame(table(plasmaAntFactors$numVol))

plasmaAntDupl <- plasmaAntFactors[plasmaAntFactors$numVol %in% counts$Var1[counts$Freq > 1],]

### Datos metabólicos ----

anova_pareado_Ácido.Caféico..CA. <- aov(formula = Ácido.Caféico..CA.~ Sexo * Endulzante * Tiempo +
                                          Error(numVol/Tiempo),
                             data = plasmaAntDupl)

summary(anova_pareado_Ácido.Caféico..CA.)

anova_pareado_CA.Gluc <- aov(formula = CA.Gluc ~ Sexo * Endulzante * Tiempo +
                               Error(numVol/Tiempo),
                                data = plasmaAntDupl)

summary(anova_pareado_CA.Gluc)

anova_pareado_X3.4.Ácido.Dihidroxifenilacético..DHPAA. <- aov(formula = X3.4.Ácido.Dihidroxifenilacético..DHPAA. 
                                                              ~ Sexo * Endulzante * Tiempo +
                                                                Error(numVol/Tiempo),
                                                            data = plasmaAntDupl)
summary(anova_pareado_X3.4.Ácido.Dihidroxifenilacético..DHPAA.)

anova_pareado_DHPAA.Gluc <- aov(formula = DHPAA.Gluc~ Sexo * Endulzante * Tiempo +
                                  Error(numVol/Tiempo),
                                 data = plasmaAntDupl)

summary(anova_pareado_DHPAA.Gluc)

anova_pareado_VA.GG <- aov(formula = VA.GG ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                                         data = plasmaAntDupl)


summary(anova_pareado_VA.GG)

anova_pareado_VA.Gluc.sulfate <- aov(formula = VA.Gluc.sulfate ~ Sexo * Endulzante * Tiempo +
                                       Error(numVol/Tiempo),
                           data = plasmaAntDupl)


summary(anova_pareado_VA.Gluc.sulfate)


### Datos antro ----

anova_pareado_Peso <- aov(formula = Peso ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = plasmaAntDupl)


summary(anova_pareado_Peso)

anova_pareado_IMC <- aov(formula = IMC ~ Sexo * Endulzante * Tiempo +
                           Error(numVol/Tiempo),
                         data = plasmaAntDupl)


summary(anova_pareado_IMC)

anova_pareado_Grasa <- aov(formula = Grasa ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = plasmaAntDupl)


summary(anova_pareado_Grasa)

anova_pareado_IRCV <- aov(formula = IRCV ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = plasmaAntDupl)


summary(anova_pareado_IRCV)

anova_pareado_Bpmin <- aov(formula = Bpmin ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = plasmaAntDupl)


summary(anova_pareado_Bpmin)

anova_pareado_Bpmax <- aov(formula = Bpmax ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = plasmaAntDupl)


summary(anova_pareado_Bpmax)

anova_pareado_Frec <- aov(formula = Frec ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = plasmaAntDupl)


summary(anova_pareado_Frec)

# Plasma flavonoides sin ajustar ----

## Preprocess ----

plasmaFlav <- preprocessTablas("data/", "tablaplasmaFlav.csv")

plasmaFlavFactors <- plasmaFlav$tablaFactors
plasmaFlavNum <- plasmaFlav$tablaNum

plasmaFlavT_0 <- plasmaFlav$tabla_Tiempo0
plasmaFlavT_F <- plasmaFlav$tabla_TiempoF

## PCA ----

pcaVarios(plasmaFlavFactors, "Plasma Antocianos")

## Clustering ----

### Checking clustering ----

checkCluster(plasmaFlavT_0[, colnames(plasmaFlavT_0) != "numVol"])
checkCluster(plasmaFlavT_F[, colnames(plasmaFlavT_F) != "numVol"])

### Performing kmeans, PAM and fuzzy ----

#clusterVarios()

### Performing model based clustering ----

## All times

model_clustering_PF <- Mclust(plasmaFlav$tablaNum)

p1 <- fviz_mclust(object = model_clustering_PF, what = "BIC", pallete = "jco",  
                  title = "Model Selection Plasma flav") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_PF, what = "classification", geom = "point",
                  title = "Clusters Plot Plasma flav", pallete = "jco")

ggarrange(p1,p2)


plasmaFlav_clusters <- cbind(plasmaFlav$tablaNum, clusters = as.factor(model_clustering_PF$classification),
                           Endulzante = plasmaFlavFactors$Endulzante, 
                           Sexo = plasmaFlavFactors$Sexo,
                           Tiempo = plasmaFlavFactors$Tiempo)

tableSexoPF <- table(plasmaFlav_clusters$Sexo,plasmaFlav_clusters$clusters)
tableEdulcorantePF <- table(plasmaFlav_clusters$Endulzante,plasmaFlav_clusters$clusters)

plasmaFlav_clusters$Endulzante <- rescale(as.numeric(plasmaFlav_clusters$Endulzante))
plasmaFlav_clusters$Sexo <- rescale(as.numeric(plasmaFlav_clusters$Sexo))
plasmaFlav_clusters$Tiempo <- rescale(as.numeric(plasmaFlav_clusters$Tiempo))

longtablePF <- melt(plasmaFlav_clusters, id = c("clusters", "Tiempo"))

ggplot(longtablePF, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  facet_wrap(~Tiempo)+
  annotate("text", x = 9, y = 1.03, label = "Mujer") + 
  annotate("text",x = 9, y = -0.03, label = "Hombre") +
  annotate("text", x = 8, y = 1.03, label = "SU") + 
  annotate("text",x = 8, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexoPF, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 9,xmax=11, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcorantePF, theme = ttheme_default(base_size = 8)), xmin= 9,xmax=11, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Plasma flav")+
  labs(y = "standarized value", x = "variables/clusters")


### Selecting by time

plasmaFlav_Factors_T0 <- subset(plasmaFlavFactors, Tiempo == "0")
plasmaFlav_Factors_TF <- subset(plasmaFlavFactors, Tiempo == "Final")

model_clustering_OFT_0 <- Mclust(plasmaFlavT_0[, colnames(plasmaFlavT_0) != "numVol"])
model_clustering_OFT_F <- Mclust(plasmaFlavT_F[, colnames(plasmaFlavT_F) != "numVol"])


### Plotting results of model based clustering 

p1 <- fviz_mclust(object = model_clustering_OFT_0, what = "BIC", pallete = "jco", 
                  title = "Model Selection Plasma Flav T 0") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OFT_0, what = "classification", geom = "point",
                  title = "Clusters Plot Plasma Flav T 0", pallete = "jco")

p3 <- fviz_mclust(object = model_clustering_OFT_F, what = "BIC", pallete = "jco",
                  title = "Model Selection Plasma Flav T F") + scale_x_discrete(limits = c(1:10))

p4 <- fviz_mclust(model_clustering_OFT_F, what = "classification", geom = "point",
                  title = "Clusters Plot Plasma Flav T F", pallete = "jco")


ggarrange(p1,p3)
ggarrange(p2,p4)

### Rejoining factors to dataframe

plasmaFlavNum_0_clusters <- cbind(plasmaFlavT_0, clusters = as.factor(model_clustering_OFT_0$classification),
                                 Endulzante = plasmaFlav_Factors_T0$Endulzante, 
                                 Sexo = plasmaFlav_Factors_T0$Sexo)
plasmaFlavNum_F_clusters <- cbind(plasmaFlavT_F, clusters = as.factor(model_clustering_OFT_F$classification),
                                 Endulzante = plasmaFlav_Factors_TF$Endulzante, 
                                 Sexo = plasmaFlav_Factors_TF$Sexo)

### Counting factors to plot them

tableSexo0 <- table(plasmaFlavNum_0_clusters$Sexo,plasmaFlavNum_0_clusters$clusters)
tableEdulcorante0 <- table(plasmaFlavNum_0_clusters$Endulzante,plasmaFlavNum_0_clusters$clusters)

tableSexoF <- table(plasmaFlavNum_F_clusters$Sexo,plasmaFlavNum_F_clusters$clusters)
tableEdulcoranteF <- table(plasmaFlavNum_F_clusters$Endulzante,plasmaFlavNum_F_clusters$clusters)


### Scaling factors to plot them. SA = 0.0, ST = 0.5, SU = 1; Hombre = 0, Mujer = 1


plasmaFlavNum_0_clusters$Endulzante <- rescale(as.numeric(plasmaFlavNum_0_clusters$Endulzante))
plasmaFlavNum_0_clusters$Sexo <- rescale(as.numeric(plasmaFlavNum_0_clusters$Sexo))

plasmaFlavNum_F_clusters$Endulzante <- rescale(as.numeric(plasmaFlavNum_F_clusters$Endulzante))
plasmaFlavNum_F_clusters$Sexo <- rescale(as.numeric(plasmaFlavNum_F_clusters$Sexo))

### Boxplot to explain clusters ----

#### Reformatting for boxplot

longtableT_0 <-melt(plasmaFlavNum_0_clusters[,colnames(plasmaFlavNum_0_clusters)!="numVol"], id = "clusters")

longtableT_F <-melt(plasmaFlavNum_F_clusters[,colnames(plasmaFlavNum_0_clusters)!="numVol"], id = "clusters")

#### Boxplotting all together

ggplot(longtableT_0, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 9, y = 1.03, label = "Mujer") + 
  annotate("text",x = 9, y = -0.03, label = "Hombre") +
  annotate("text", x = 8, y = 1.03, label = "SU") + 
  annotate("text",x = 8, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexo0, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 9,xmax=11, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcorante0, theme = ttheme_default(base_size = 8)), xmin= 9,xmax=11, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Plasma Flavonoids T 0")+
  labs(y = "standarized value", x = "variables/clusters")

ggplot(longtableT_F, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 9, y = 1.03, label = "Mujer") + 
  annotate("text",x = 9, y = -0.03, label = "Hombre") +
  annotate("text", x = 9, y = 1.03, label = "SU") + 
  annotate("text",x = 8, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexoF, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 9,xmax=11, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcoranteF, theme = ttheme_default(base_size = 8)), xmin= 9,xmax=11, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Plasma Flavonoids T F")+
  labs(y = "standarized value", x = "variables/clusters")


### Explainig clusters with parallell coordinates plot----

p <- ggparcoord(data = plasmaFlavNum_0_clusters[, colnames(plasmaFlavT_0) != "numVol"], groupColumn = "clusters", 
                scale = "std", columns = c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)) + 
  labs(x = "variables", 
       y = "value (in standard-deviation units)", 
       title = "Clustering Tiempo 0")

p1 <- ggparcoord(data = plasmaFlavNum_F_clusters[, colnames(plasmaFlavT_F) != "numVol"], groupColumn = "clusters", 
                 scale = "std", columns = c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)) + 
  labs(x = "variables", 
       y = "value (in standard-deviation units)", 
       title = "Clustering Tiempo F")

ggarrange(p,p1)

## ANOVA paired ----

counts <- data.frame(table(plasmaFlavFactors$numVol))

plasmaFlavDupl <- plasmaFlavFactors[plasmaFlavFactors$numVol %in% counts$Var1[counts$Freq > 1],]

### Datos metabólicos ----

### Datos antro ----
anova_pareado_Peso <- aov(formula = Peso ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = plasmaFlavDupl)


summary(anova_pareado_Peso)

anova_pareado_IMC <- aov(formula = IMC ~ Sexo * Endulzante * Tiempo +
                           Error(numVol/Tiempo),
                         data = plasmaFlavDupl)


summary(anova_pareado_IMC)

anova_pareado_Grasa <- aov(formula = Grasa ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = plasmaFlavDupl)


summary(anova_pareado_Grasa)

anova_pareado_IRCV <- aov(formula = IRCV ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = plasmaFlavDupl)


summary(anova_pareado_IRCV)

anova_pareado_Bpmin <- aov(formula = Bpmin ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = plasmaFlavDupl)


summary(anova_pareado_Bpmin)

anova_pareado_Bpmax <- aov(formula = Bpmax ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = plasmaFlavDupl)


summary(anova_pareado_Bpmax)

anova_pareado_Frec <- aov(formula = Frec ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = orinaFlavDupl)


summary(anova_pareado_Frec)


# Plasma AJUSTADO flavonoides ----

## Preprocess ----

plasmaFlav_adjusted <- preprocessTablas("data/", "tablaplasmaFlav_adjusted.csv")

plasmaFlav_adjustedFactors <- plasmaFlav_adjusted$tablaFactors
plasmaFlav_adjustedNum <- plasmaFlav_adjusted$tablaNum

plasmaFlav_adjustedT_0 <- plasmaFlav_adjusted$tabla_Tiempo0
plasmaFlav_adjustedT_F <- plasmaFlav_adjusted$tabla_TiempoF

## PCA ----

pcaVarios(plasmaFlav_adjustedFactors, "Plasma Antocianos")

## Clustering ----

### Checking clustering ----

checkCluster(plasmaFlav_adjustedT_0[, colnames(plasmaFlav_adjustedT_0) != "numVol"])
checkCluster(plasmaFlav_adjustedT_F[, colnames(plasmaFlav_adjustedT_F) != "numVol"])

### Performing kmeans, PAM and fuzzy ----

#clusterVarios()

### Performing model based clustering ----

## All times

model_clustering_PF <- Mclust(plasmaFlav_adjusted$tablaNum)

p1 <- fviz_mclust(object = model_clustering_PF, what = "BIC", pallete = "jco",  
                  title = "Model Selection Plasma ADJ flav") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_PF, what = "classification", geom = "point",
                  title = "Clusters Plot Plasma ADJ flav", pallete = "jco")

ggarrange(p1,p2)


plasmaFlav_adjusted_clusters <- cbind(plasmaFlav_adjusted$tablaNum, clusters = as.factor(model_clustering_PF$classification),
                             Endulzante = plasmaFlav_adjustedFactors$Endulzante, 
                             Sexo = plasmaFlav_adjustedFactors$Sexo,
                             Tiempo = plasmaFlav_adjustedFactors$Tiempo)

tableSexoPF <- table(plasmaFlav_adjusted_clusters$Sexo,plasmaFlav_adjusted_clusters$clusters)
tableEdulcorantePF <- table(plasmaFlav_adjusted_clusters$Endulzante,plasmaFlav_adjusted_clusters$clusters)

plasmaFlav_adjusted_clusters$Endulzante <- rescale(as.numeric(plasmaFlav_adjusted_clusters$Endulzante))
plasmaFlav_adjusted_clusters$Sexo <- rescale(as.numeric(plasmaFlav_adjusted_clusters$Sexo))
plasmaFlav_adjusted_clusters$Tiempo <- rescale(as.numeric(plasmaFlav_adjusted_clusters$Tiempo))

longtablePF <- melt(plasmaFlav_adjusted_clusters, id = c("clusters", "Tiempo"))

ggplot(longtablePF, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  facet_wrap(~Tiempo)+
  annotate("text", x = 11, y = 1.03, label = "Mujer") + 
  annotate("text",x = 11, y = -0.03, label = "Hombre") +
  annotate("text", x = 10, y = 1.03, label = "SU") + 
  annotate("text",x = 10, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexoPF, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcorantePF, theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Plasma ADJ flav")+
  labs(y = "standarized value", x = "variables/clusters")


### Selecting by time

plasmaFlav_adjusted_Factors_T0 <- subset(plasmaFlav_adjustedFactors, Tiempo == "0")
plasmaFlav_adjusted_Factors_TF <- subset(plasmaFlav_adjustedFactors, Tiempo == "Final")

model_clustering_OFT_0 <- Mclust(plasmaFlav_adjustedT_0[, colnames(plasmaFlav_adjustedT_0) != "numVol"])
model_clustering_OFT_F <- Mclust(plasmaFlav_adjustedT_F[, colnames(plasmaFlav_adjustedT_F) != "numVol"])


### Plotting results of model based clustering 

p1 <- fviz_mclust(object = model_clustering_OFT_0, what = "BIC", pallete = "jco", 
                  title = "Model Selection Plasma Flav T 0") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OFT_0, what = "classification", geom = "point",
                  title = "Clusters Plot Plasma ADJ Flav T 0", pallete = "jco")

p3 <- fviz_mclust(object = model_clustering_OFT_F, what = "BIC", pallete = "jco",
                  title = "Model Selection Plasma Flav T F") + scale_x_discrete(limits = c(1:10))

p4 <- fviz_mclust(model_clustering_OFT_F, what = "classification", geom = "point",
                  title = "Clusters Plot Plasma ADJ Flav T F", pallete = "jco")


ggarrange(p1,p3)
ggarrange(p2,p4)

### Rejoining factors to dataframe

plasmaFlav_adjustedNum_0_clusters <- cbind(plasmaFlav_adjustedT_0, clusters = as.factor(model_clustering_OFT_0$classification),
                                  Endulzante = plasmaFlav_adjusted_Factors_T0$Endulzante, 
                                  Sexo = plasmaFlav_adjusted_Factors_T0$Sexo)
plasmaFlav_adjustedNum_F_clusters <- cbind(plasmaFlav_adjustedT_F, clusters = as.factor(model_clustering_OFT_F$classification),
                                  Endulzante = plasmaFlav_adjusted_Factors_TF$Endulzante, 
                                  Sexo = plasmaFlav_adjusted_Factors_TF$Sexo)

### Counting factors to plot them

tableSexo0 <- table(plasmaFlav_adjustedNum_0_clusters$Sexo,plasmaFlav_adjustedNum_0_clusters$clusters)
tableEdulcorante0 <- table(plasmaFlav_adjustedNum_0_clusters$Endulzante,plasmaFlav_adjustedNum_0_clusters$clusters)

tableSexoF <- table(plasmaFlav_adjustedNum_F_clusters$Sexo,plasmaFlav_adjustedNum_F_clusters$clusters)
tableEdulcoranteF <- table(plasmaFlav_adjustedNum_F_clusters$Endulzante,plasmaFlav_adjustedNum_F_clusters$clusters)


### Scaling factors to plot them. SA = 0.0, ST = 0.5, SU = 1; Hombre = 0, Mujer = 1


plasmaFlav_adjustedNum_0_clusters$Endulzante <- rescale(as.numeric(plasmaFlav_adjustedNum_0_clusters$Endulzante))
plasmaFlav_adjustedNum_0_clusters$Sexo <- rescale(as.numeric(plasmaFlav_adjustedNum_0_clusters$Sexo))

plasmaFlav_adjustedNum_F_clusters$Endulzante <- rescale(as.numeric(plasmaFlav_adjustedNum_F_clusters$Endulzante))
plasmaFlav_adjustedNum_F_clusters$Sexo <- rescale(as.numeric(plasmaFlav_adjustedNum_F_clusters$Sexo))

### Boxplot to explain clusters ----

#### Reformatting for boxplot

longtableT_0 <-melt(plasmaFlav_adjustedNum_0_clusters[,colnames(plasmaFlav_adjustedNum_0_clusters)!="numVol"], id = "clusters")

longtableT_F <-melt(plasmaFlav_adjustedNum_F_clusters[,colnames(plasmaFlav_adjustedNum_0_clusters)!="numVol"], id = "clusters")

#### Boxplotting all together

ggplot(longtableT_0, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 11, y = 1.03, label = "Mujer") + 
  annotate("text",x = 11, y = -0.03, label = "Hombre") +
  annotate("text", x = 10, y = 1.03, label = "SU") + 
  annotate("text",x = 10, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexo0, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcorante0, theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Plasma ADJ Flavonoids T 0")+
  labs(y = "standarized value", x = "variables/clusters")

ggplot(longtableT_F, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 11, y = 1.03, label = "Mujer") + 
  annotate("text",x = 11, y = -0.03, label = "Hombre") +
  annotate("text", x = 10, y = 1.03, label = "SU") + 
  annotate("text",x = 10, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexoF, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcoranteF, theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Plasma ADJ Flavonoids T F")+
  labs(y = "standarized value", x = "variables/clusters")


### Explainig clusters with parallell coordinates plot----

p <- ggparcoord(data = plasmaFlav_adjustedNum_0_clusters[, colnames(plasmaFlav_adjustedT_0) != "numVol"], groupColumn = "clusters", 
                scale = "std", columns = c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)) + 
  labs(x = "variables", 
       y = "value (in standard-deviation units)", 
       title = "Clustering Tiempo 0")

p1 <- ggparcoord(data = plasmaFlav_adjustedNum_F_clusters[, colnames(plasmaFlav_adjustedT_F) != "numVol"], groupColumn = "clusters", 
                 scale = "std", columns = c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)) + 
  labs(x = "variables", 
       y = "value (in standard-deviation units)", 
       title = "Clustering Tiempo F")

ggarrange(p,p1)

## ANOVA paired ----

counts <- data.frame(table(plasmaFlav_adjustedFactors$numVol))

plasmaFlav_adjustedDupl <- plasmaFlav_adjustedFactors[plasmaFlav_adjustedFactors$numVol %in% counts$Var1[counts$Freq > 1],]

### Datos metabólicos ----

anova_pareado_Eriodictiol..E. <- aov(formula = Eriodictiol..E. ~ Sexo * Endulzante * Tiempo +
                                       Error(numVol/Tiempo),
                                     data = plasmaFlav_adjustedDupl)


summary(anova_pareado_Eriodictiol..E.)


anova_pareado_ES <- aov(formula = ES ~ Sexo * Endulzante * Tiempo +
                          Error(numVol/Tiempo),
                        data = plasmaFlav_adjustedDupl)


summary(anova_pareado_ES)

### Datos antro ----

anova_pareado_Peso <- aov(formula = Peso ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = plasmaFlav_adjustedDupl)


summary(anova_pareado_Peso)

anova_pareado_IMC <- aov(formula = IMC ~ Sexo * Endulzante * Tiempo +
                           Error(numVol/Tiempo),
                         data = plasmaFlav_adjustedDupl)


summary(anova_pareado_IMC)

anova_pareado_Grasa <- aov(formula = Grasa ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = plasmaFlav_adjustedDupl)


summary(anova_pareado_Grasa)

anova_pareado_IRCV <- aov(formula = IRCV ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = plasmaFlav_adjustedDupl)


summary(anova_pareado_IRCV)

anova_pareado_Bpmin <- aov(formula = Bpmin ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = plasmaFlav_adjustedDupl)


summary(anova_pareado_Bpmin)

anova_pareado_Bpmax <- aov(formula = Bpmax ~ Sexo * Endulzante * Tiempo +
                             Error(numVol/Tiempo),
                           data = plasmaFlav_adjustedDupl)


summary(anova_pareado_Bpmax)

anova_pareado_Frec <- aov(formula = Frec ~ Sexo * Endulzante * Tiempo +
                            Error(numVol/Tiempo),
                          data = plasmaFlav_adjustedDupl)


summary(anova_pareado_Frec)

# PRUEBAS ----

### Solo metabolicos ----

#### Cluster ----

clusterNPlot <- function(listaTablas){

tablaNumMet <- listaTablas$tablaNum %>%
  dplyr::select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec))

tablaFactorsAll <-listaTablas$tablaFactors

model_clustering_OF <- Mclust(tablaNumMet)

p1 <- fviz_mclust(object = model_clustering_OF, what = "BIC", pallete = "jco",  
                  title = "Model Selection Orina Flav") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OF, what = "classification", geom = "point",
                  title = "Clusters Plot Orina Flav", pallete = "jco")

ggarrange(p1,p2)


tabla_clusters <- tablaNumMet %>% tibble::add_column(Peso = listaTablas$tablaNum$Peso, 
                                                      IMC = listaTablas$tablaNum$IMC, 
                                                      Grasa = listaTablas$tablaNum$Grasa, 
                                                      IRCV = listaTablas$tablaNum$IRCV, 
                                                      Bpmin = listaTablas$tablaNum$Bpmin, 
                                                      Bpmax = listaTablas$tablaNum$Bpmax, 
                                                      Frec = listaTablas$tablaNum$Frec,
                                                      clusters = model_clustering_OF$classification,
                                                      Endulzante = rescale(as.numeric(tablaFactorsAll$Endulzante)), 
                                                      Sexo = rescale(as.numeric(tablaFactorsAll$Sexo)),
                                                      Tiempo = tablaFactorsAll$Tiempo) %>%
                  select(everything(),Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, Endulzante, Sexo, Tiempo, clusters)



tableSexo <- table(tabla_clusters$Sexo, tabla_clusters$clusters)#tabla_clusters %>% count(Sexo, clusters)  
tableEdulcorante <- table(tabla_clusters$Endulzante, tabla_clusters$clusters) #tabla_clusters %>% count(Endulzante, clusters)

tabla_clusters$Endulzante <- rescale(as.numeric(tabla_clusters$Endulzante))
tabla_clusters$Sexo <- rescale(as.numeric(tabla_clusters$Sexo))

longtableOF <- melt(tabla_clusters, id = c("clusters", "Tiempo"))

longtableOF <- tabla_clusters %>% gather(variable, values, -clusters, -Tiempo, )

ggplot(longtableOF, aes(factor(variable, level = unique(longtableOF$variable)),as.numeric(values), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = which(unique(longtableOF$variable)=="Sexo"), y = 1.03, label = "Mujer") + 
  annotate("text",x = which(unique(longtableOF$variable)=="Sexo"), y = -0.03, label = "Hombre") +
  annotate("text", x = which(unique(longtableOF$variable)=="Endulzante"), y = 1.03, label = "SU") + 
  annotate("text",x = which(unique(longtableOF$variable)=="Endulzante"), y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexo, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcorante, rows=c("SA", "ST","SU"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0, ymax=0.25)+
  ggtitle(paste("Boxplot Cluster Analysis ", deparse(substitute(listaTablas))))+
  labs(y = "standarized value", x = "variables/clusters")+
  facet_wrap(~Tiempo)

}

library(QuantPsyc)
library(energy)


checkMultVar <- function(listaTablas) {
    

    tablaNumMet1 <- listaTablas$tablaNum %>% 
    dplyr::select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec))

multivarTest <- mult.norm(tablaNumMet1)$mult.test
print(multivarTest)
print (mvnorm.etest(tablaNumMet1, R=1000))

}

checkMultVar(orinaAnt)
clusterNPlot(orinaAnt)

checkMultVar(orinaFlav)
clusterNPlot(orinaFlav)

checkMultVar(plasmaAnt)
clusterNPlot(plasmaAnt)

plasmaFlav_adjusted <- preprocessTablas("data/", "tablaplasmaFlav_adjusted.csv")

clusterNPlot(plasmaFlav_adjusted)




#### Anova ----

datosuwu <- orinaFlavDupl %>%
  select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec))



datosuwu_aov <- aov(formula = EG ~ Sexo*Endulzante*Tiempo + Error(numVol/Tiempo), data = datosuwu)

anova_pareado_ES <- aov(formula = EG ~ Sexo*Endulzante*Tiempo + Error(numVol/Tiempo), data = orinaFlavDupl)

summary(anova_pareado_ES)

summary(datosuwu_aov)

### TODO JUNTO ----

orinaFlav <- preprocessTablas("data/", "tablaOrinaFlav.csv")
orinaAnt <- preprocessTablas("data/", "tablaorinaAnt.csv")
plasmaAnt <- preprocessTablas("data/", "tablaplasmaAnt.csv")
plasmaFlav <- preprocessTablas("data/", "tablaplasmaFlav.csv")

tabla1 <- orinaFlav$tablaFactors %>% 
          select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec))

tabla2 <- orinaAnt$tablaFactors %>% 
          select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, Endulzante, 
                    Sexo, Tiempo))

tabla3 <- plasmaAnt$tablaFactors %>% 
          select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, Endulzante, 
                    Sexo, Tiempo))

tabla_antro <- plasmaFlav$tablaFactors %>% 
               select(c(numVol, Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec)) 

tabla_merge <- na.omit(full_join( full_join( full_join(
               tabla_antro %>% group_by(numVol) %>% mutate(id = row_number()),
               tabla1 %>% group_by(numVol) %>% mutate(id = row_number()), by = c("numVol", "id")), 
               tabla2 %>% group_by(numVol) %>% mutate(id = row_number()), by = c("numVol", "id")),
               tabla3 %>% group_by(numVol) %>% mutate(id = row_number()), by = c("numVol", "id")) )
                                                    

model_clustering_OF <- Mclust(ungroup(tabla_merge) %>% select(-c(numVol, Endulzante, Sexo, Tiempo, id, Peso, IMC,
                                                                 Grasa, IRCV, Bpmin, Bpmax, Frec)))

p1 <- fviz_mclust(object = model_clustering_OF, what = "BIC", pallete = "jco",  
                  title = "Model Selection Orina Flav") + scale_x_discrete(limits = c(1:10))

p2 <- fviz_mclust(model_clustering_OF, what = "classification", geom = "point",
                  title = "Clusters Plot Orina Flav", pallete = "jco")

ggarrange(p1,p2)


tabla_merge_clusters <- cbind(ungroup(tabla_merge) %>% select(-c(numVol, id)), clusters = as.factor(model_clustering_OF$classification))

tableSexoOF <- table(tabla_merge_clusters$Sexo,tabla_merge_clusters$clusters)
tableEdulcoranteOF <- table(tabla_merge_clusters$Endulzante,tabla_merge_clusters$clusters)

tabla_merge_clusters$Endulzante <- rescale(as.numeric(tabla_merge_clusters$Endulzante))
tabla_merge_clusters$Sexo <- rescale(as.numeric(tabla_merge_clusters$Sexo))
tabla_merge_clusters$Tiempo <- rescale(as.numeric(tabla_merge_clusters$Tiempo))

longtableOF <- melt(tabla_merge_clusters, id = c("clusters", "Tiempo"))

ggplot(longtableOF, aes(variable,as.numeric(value), fill=factor(clusters))) +
  geom_boxplot()+
  annotate("text", x = 14, y = 1.03, label = "Mujer") + 
  annotate("text",x = 14, y = -0.03, label = "Hombre") +
  annotate("text", x = 13, y = 1.03, label = "SU") + 
  annotate("text",x = 13, y = -0.03, label = "SA")+
  annotation_custom(grob = tableGrob(tableSexoOF, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 22,xmax=24, ymin=0.75, ymax=1)+
  annotation_custom(grob = tableGrob(tableEdulcoranteOF, theme = ttheme_default(base_size = 8)), xmin= 22,xmax=24, ymin=0, ymax=0.25)+
  ggtitle("Boxplot Cluster Analysis Orina Flavonoids")+
  labs(y = "standarized value", x = "variables/clusters")+
  facet_wrap(~Tiempo)


#### Anovas ----


counts <- data.frame(table(tabla_merge$numVol))

tabla_mergeDupl <- tabla_merge[tabla_merge$numVol %in% counts$Var1[counts$Freq > 1],]

### Datos metabólicos ----

anova_pareado_EG <- aov(formula = EG ~ Sexo * Endulzante * Tiempo +
                          Error(numVol/Tiempo),
                        data = tabla_mergeDupl)

summary(anova_pareado_EG)


anova_pareado_ES <- aov(formula = ES ~ Sexo * Endulzante * Tiempo +
                          Error(numVol/Tiempo),
                        data = tabla_mergeDupl)

summary(anova_pareado_ES)


for(i in colnames(tabla_mergeDupl)) {
  
  if (i == "numVol"){
   print("uwu") 
  } 
  else{
  print(as.matrix(i))  
  anova_paired <- aov(formula = as.matrix(i) ~ Sexo * Endulzante * Tiempo +
                        Error(numVol/Tiempo),
                      data = tabla_mergeDupl)
  }

}


anova_results <- purrr::map(tabla_mergeDupl[,2:9], ~aov(.x ~  tabla_mergeDupl$Sexo * tabla_mergeDupl$Endulzante 
                                                        * tabla_mergeDupl$Tiempo + 
                                                          Error(tabla_mergeDupl$numVol/tabla_mergeDupl$Tiempo)))
anova_results[[1]]



aov_results <- lapply(tabla_mergeDupl, function(x) aov(as.numeric(x) ~ Sexo * Endulzante * Tiempo +
                                                               Error(numVol/Tiempo),
                                                               data = tabla_mergeDupl))


pruebaUwu <- summary(aov_results[[2]])

pruebaUwu$`Error: numVol:Tiempo`[[1]][1,]




library(stargazer)

stargazer(anova(aov_results[[1]]$`numVol:Tiempo`), type = "html")

  
  for (j in seq(1, length(aov_results))) {
    print(paste("Variable analizada: ", names(aov_results)[j]))
    
    resultado <- summary(aov_results[[j]])$`Error: numVol:Tiempo`
    
    for (i in seq(1,4)){
      
      if (resultado[[1]][,5][i] < 0.05){
        print(resultado[[1]][i,])
      }
      
    }  
    
  }
  

  
names(aov_results)[1]

lapply(aov_results, print)

str(tabla_mergeDupl)

# [1] "numVol"                                   "Peso"                                     "IMC"                                     
# [4] "Grasa"                                    "IRCV"                                     "Bpmin"                                   
# [7] "Bpmax"                                    "Frec"                                     "id"                                      
# [10] "EG"                                       "ES"                                       "HE.G"                                    
# [13] "NG"                                       "NS"                                       "Endulzante"                              
# [16] "Tiempo"                                   "Sexo"                                     "CA.Gluc.x"                               
# [19] "DHPAA.Gluc.x"                             "TFA.Gluc"                                 "TFA.Sulfate"                             
# [22] "Ácido.Vanílico..VA."                      "Ácido.Caféico..CA."                       "CA.Gluc.y"                               
# [25] "X3.4.Ácido.Dihidroxifenilacético..DHPAA." "DHPAA.Gluc.y"                             "VA.GG"                                   
# [28] "VA.Gluc.sulfate"


### Conservar numVol ----

datosOwo <- orinaFlav$tablaFactors

for (i in range(1,nrow(tabla))){
  if (tabla$clusters_0[i] != tabla$clusters_F[i]){
      tabla_ =+ 
  }
}

### Anova OTRO METODO paired ----

set.seed(123)

datos <- orinaFlav$tablaFactors %>% dplyr::select (numVol, Sexo, Endulzante, Tiempo, EG)

datos %>% sample_n_by(Sexo, Endulzante, size=1)

# long format needed?

datos %>% 
  group_by(Sexo, Endulzante, Tiempo) %>%
  get_summary_stats(EG, type = "mean_sd")

bxp <- ggboxplot(
  datos, x = "Endulzante", y = "EG",
  color = "Tiempo", palette = "jco",
  facet.by = "Sexo", short.panel.labs = FALSE
)

bxp
bxp_nogg <- boxplot(datos$Endulzante, datos$EG)
datos <- datos[-which(datos$EG %in% bxp_nogg$out),]

datos %>%
  group_by(Sexo, Endulzante, Tiempo) %>%
  identify_outliers(EG)

# Normality

datos %>%
  group_by(Sexo, Endulzante, Tiempo) %>%
  shapiro_test(EG)

res.aov <- anova_test(
  data = datos, dv = EG, wid = numVol,
  within = c(Endulzante, Tiempo)
  )

### 

checkNorm <- function (listaTablas, var) {

datos <- listaTablas$tablaFactors# %>% dplyr::select (numVol, Sexo, Endulzante, Tiempo, EG)

Q <- quantile(datos[,var], probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(datos[,var])

datos <- subset(datos, 
                datos[,var] > (Q[1] - 0.5*iqr) & 
                  datos[,var] < (Q[2] + 0.5*iqr))


#datos <- datos [-which(datos$ES > 0.2),]

print(
  ggplot(data = datos, aes(x = datos[,var])) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(datos[,var]),
                            sd = sd(datos[,var]))) +
  ggtitle("Histograma con curva normal teórica") +
  theme_bw()
  )

print(
qqnorm(datos[,var], pch = 19, col = "gray50")
)
qqline(datos[,var])

print(shapiro.test(datos[,var]))

print(ks.test(datos[,var], "pnorm", mean(datos[,var]), sd(datos[,var])))

print(nortest::lillie.test(datos[,var]))

return()

}

orinaFlavNorm <- checkNorm(orinaFlav, 'ES')

orinaFlavNormES <- orinaFlavNorm %>% select(numVol, Tiempo, Endulzante, Sexo, ES)



res_norm <- nortest::lillie.test(orinaFlav$tablaFactors[, "ES"])

res_norm$p.value

res_norm <- ks.test(orinaFlav$tablaFactors[, "ES"], "pnorm", 
                    mean(orinaFlav$tablaFactors[, "ES"]),
                    sd(orinaFlav$tablaFactors[, "ES"]))

re_norm <- function(datos, var){
  
  Q <- quantile(datos[,var], probs=c(.25, .75), na.rm = FALSE)
  
  iqr <- IQR(datos[,var])
  
  resultado$p.value <- 0
  valIQR <- 1.5
  
  while (resultado$p.value < 0.05) {
    
    
  datosNorm <- subset(datos, datos[,var] > (Q[1] - valIQR*iqr) & 
                         datos[,var] < (Q[2] + valIQR*iqr))
  
  # resultado <- ks.test(datosNorm[, var], "pnorm",
  #                      mean(datosNorm[, var]),
  #                      sd(datosNorm[, var]))
  # 
  
  resultado <- nortest::lillie.test(datos[,var])
  
  print(nrow(datosNorm))
  print(valIQR)
  valIQR <- valIQR - 0.05
  
    }
  
  return(list(pval = resultado$p.value, dataset =datosNorm))
}

uwu <- re_norm(orinaFlav$tablaFactors, "ES")

hist(orinaFlav$tablaFactors[,"IRCV"])

plot(orinaFlav$tablaFactors)

batchNorm <- function(tabla){
  
  for (i in seq(ncol(tabla))) {
    
    print(names(tabla)[i])
    
    if (is.numeric(tabla[,i])){
        
      resultado <- re_norm(datos = tabla, var = names(tabla)[i])
      
      print(names(tabla[i]))
      
      }
  print(nrow(resultado$dataset))
  print(ncol(resultado$dataset))  
  } 
  return(resultado)
}


uwu4 <- batchNorm(orinaFlav$tablaFactors)

re_norm(orinaFlav$tablaFactors, )


checkMultVar(uwu4$dataset)



checkMultVar <- function(listaTablas) {
  
  
  tablaNumMet1 <- listaTablas %>% 
   dplyr::select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, Endulzante,
                    Tiempo, Sexo, numVol))
  
  multivarTest <- QuantPsyc::mult.norm(tablaNumMet1)$mult.test
  print(multivarTest)
  print (energy::mvnorm.etest(tablaNumMet1, R=1000))
  
}

checkMultVar(orinaAnt)
clusterNPlot(uwu4$dataset)

checkMultVar(orinaFlav)
clusterNPlot(orinaFlav)

checkMultVar(plasmaAnt)
clusterNPlot(plasmaAnt)












aov_todo <- function (tablaFactors) {

counts <- data.frame(table(tablaFactors$numVol))

tabla_Dupl <- tablaFactors[tablaFactors$numVol %in% counts$Var1[counts$Freq > 1],]


aov_results <- lapply(tabla_Dupl, function(x) aov(as.numeric(x) ~ Sexo *
                                                         Endulzante * Tiempo +
                                                         Error(numVol/Tiempo),
                                                        data = tabla_Dupl))

for (j in seq(1, length(aov_results))) {
  message(paste("Variable analizada: ", names(aov_results)[j]))
  
  resultado <- summary(aov_results[[j]])$`Error: numVol:Tiempo`
  
  for (i in seq(1,4)){
    
    if (resultado[[1]][,5][i] < 0.1){
      print(resultado[[1]][i,])
    }
    
  }  
  
}

}

aov_todo (orinaFlavNorm)




### ----

orinaFlavNorm <- checkNorm(orinaFlav, 'ES')

orinaFlavNormES <- orinaFlavNorm %>% select(numVol, Tiempo, Endulzante, Sexo, ES)

# bxp <- ggboxplot(
#   orinaFlavNormES, x = "Endulzante", y = "ES",
#   color = "Tiempo", palette = "jco",
#   facet.by = "Sexo", short.panel.labs = FALSE
# )
# bxp
# 
# orinaFlavNormES %>%
#   group_by(Sexo, Endulzante, Tiempo) %>%
#   identify_outliers(ES)
# 
# orinaFlavNormES %>%
#   group_by(Sexo, Endulzante, Tiempo) %>%
#   shapiro_test(ES)
# 
# res.aov <- anova_test(
#   data = orinaFlavNormES, dv= ES, wid = numVol, 
#   within = c(Endulzante, Sexo, Tiempo))

tabla_Dupl %>%
  group_by(Endulzante, Tiempo) %>%
  identify_outliers(ES)

tabla_Dupl %>%
  group_by(Endulzante, Tiempo) %>%
  shapiro_test(ES)

counts <- data.frame(table(orinaFlavNormES$numVol))

tabla_Dupl <- orinaFlavNormES[orinaFlavNormES$numVol %in% counts$Var1[counts$Freq > 1],]

### ANOVA BIEN HECHO ----

res.aov <- anova_test(
  data = tabla_Dupl, formula = ES ~ Sexo * Endulzante * Tiempo +
    Error(numVol/Tiempo))
res.aov <- anova_test(
  data = orinaFlav$tablaFactors, dv=ES, wid=numVol, between = c(Sexo, Endulzante), 
  within= Tiempo, detailed = T)

anova_tablexD <- get_anova_table(res.aov, correction = "auto")

aov_results2 <- lapply(tabla_Dupl, function(x) anova_test(tabla_Dupl,
                                                          as.numeric(x) ~ Sexo * 
                                                          Endulzante * Tiempo +
                                                          Error(numVol/Tiempo)
                                                          ))
aov_results <- lapply(tabla_Dupl, function(x) aov(as.numeric(x) ~ Sexo * Endulzante * Tiempo +
                                                         Error(numVol/Tiempo),
                                                       data = tabla_Dupl))


aov_test <- function(tabla, variable){
  
  tablaVar <- tabla %>% dplyr::select(numVol, Endulzante, Sexo, Tiempo, variable)
  
  tablaVar <- tablaVar[!tablaVar[,variable] %in% boxplot.stats(tablaVar[,variable])$out,]
  
  res.aov <- anova_test(data = tablaVar, dv=variable, wid=numVol, 
                        between = c(Sexo, Endulzante), within= Tiempo)
  
  tablaAnova <- get_anova_table(res.aov, correction = "auto")
  
  print(tablaAnova)

}

aov_test(orinaFlav$tablaFactors, "ES")


tablaVar <- orinaFlav$tablaFactors %>% dplyr::select(numVol, Endulzante, Sexo, Tiempo, ES)

tablaVar <- tablaVar[!tablaVar$ES %in% boxplot.stats(tablaVar$ES)$out,]

length(tablaVar$numVol)


counts <- data.frame(table(tablaVar$numVol))

tablaVarDupl <- tablaVar[tablaVar$numVol %in% counts$Var1[counts$Freq > 1],]







tablaVar <- orinaFlav$tablaFactors %>% select(numVol, Endulzante, Sexo, Tiempo, "ES")

str(orinaFlav$tablaFactors)

for (i in seq(1,ncol(orinaFlav$tablaFactors))){
  
  if (is.numeric(orinaFlav$tablaFactors[,i])){
  
    aov_test(orinaFlav$tablaFactors,
           names(orinaFlav$tablaFactors)[i])
  }
  
  
}


anova_test()
