source("scripts/preprocess.R")

#install.packages(c("modeest", "psych"))

library(modeest)
library(psych)

listaTablas <- preprocessTables("data/", "tablaOrinaAnt.csv")

tablaFactores <- removeOutliers(listaTablas$tablaSinEsc)

estadisticosDescriptivos <- function (tabla, variableNum) {

  if (is.numeric(tabla[, variableNum])){
    
      min <- min(tabla[, variableNum], na.rm = TRUE)
      q1 <- quantile(tabla[, variableNum], probs = 0.25, na.rm = TRUE)
      media <- mean.default(tabla[, variableNum], na.rm = TRUE)
      media_rec <- mean.default(tabla[, variableNum], trim = 0.025, na.rm = TRUE)
      mediana <- median.default(tabla[, variableNum], na.rm = TRUE)
      moda <- mfv(tabla[, variableNum])
      var <- var(tabla[, variableNum], na.rm = TRUE)
      desvest <- sd(tabla[, variableNum], na.rm = TRUE)
      q3 <- quantile(tabla[, variableNum], probs = 0.75, na.rm = TRUE)
      max <- max(tabla[, variableNum], na.rm = TRUE)
      s <- skew(tabla[, variableNum])
      c <- kurtosi(tabla[, variableNum])
  

  #Valores de estadísticos como vector
  descriptivos <- as.numeric(c(min, q1, media, media_rec, mediana, moda,
                                     var, desvest, q3, max, s, c))

  #Encabezados de cada estadístico como un vector
  nombres <- c("Mínimo", "Q1", "Media", "Media recortada", "Mediana", "Moda",
             "Varianza", "Desviación Estándar", "Q3", "Máximo", "Simetría", "Curtosis")


  descr2 <- as.data.frame(rbind(nombres, descriptivos))

  }

  return (descr2)


}

pruebaUwu <- estadisticosDescriptivos(tablaFactores, "Peso")
View(pruebaUwu)

skimr::skim(tablaFactores)
