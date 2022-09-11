source("scripts/preprocess.R")

install.packages(c("modeest", "psych"))

library(modeest)
library(psych)

listaTablas <- preprocessTables("data/", "tablaOrinaAnt.csv")

tablaFactores <- removeOutliers(listaTablas$tablaSinEsc)

estadisticosDescriptivos <- function (tabla) {

  #Encabezados de cada estadístico como un vector
  nombres <- c("Mínimo", "Q1", "Media", "Media recortada", "Mediana", "Moda",
               "Varianza", "Desviación Estándar", "Q3", "Máximo", "Simetría", "Curtosis")
  
  descr <- data.frame(matrix(ncol = length(nombres), nrow = 0))
  colnames(descr) <- nombres
  
  for (i in colnames(tabla)){
  
  if (is.numeric(tabla[, i]) & ){
    
      min <- min(tabla[, i], na.rm = TRUE)
      q1 <- quantile(tabla[, i], probs = 0.25, na.rm = TRUE)
      media <- mean.default(tabla[, i], na.rm = TRUE)
      media_rec <- mean.default(tabla[, i], trim = 0.025, na.rm = TRUE)
      mediana <- median.default(tabla[, i], na.rm = TRUE)
      moda <- mfv(tabla[, i])
      var <- var(tabla[, i], na.rm = TRUE)
      desvest <- sd(tabla[, i], na.rm = TRUE)
      q3 <- quantile(tabla[, i], probs = 0.75, na.rm = TRUE)
      max <- max(tabla[, i], na.rm = TRUE)
      s <- skew(tabla[, i])
      c <- kurtosi(tabla[, i])
  

  #Valores de estadísticos como vector
  descriptivos <- as.numeric(c(min, q1, media, media_rec, mediana, moda,
                                     var, desvest, q3, max, s, c))

  descr <- rbind(descr,descriptivos)

  }
    
    
  }

  return (descr)


}

pruebaUwu <- estadisticosDescriptivos(tablaFactores)

View(pruebaUwu)

skimr::skim(tablaFactores)

plasmaAnt <- read.csv("data/mainPlasmaAnt.csv") %>% select(-c(Time, Sex, Sweetener, numVol, X))

mainPlasmaDesc <- skimr::skim(plasmaAnt) %>% 
  add_column(skewness = apply(plasmaAnt, MARGIN = 2, skewness)) 

