source("scripts/preprocess.R")

tabla1 <- reading("data/cronicoOrinaAntLimpio.csv", nasPercentageCol = 0.3, nasRow = T)
write.csv(tabla1, "data/prueba.csv")
tabla1pp <- preprocessTables("data/", "prueba.csv")

View(tabla1pp$tablaSinEsc)

# ----

tabla1 <- read.csv("data/mainPlasmaAnt.csv")

head(tabla1)

tabla1_sinout <- removeOutliers(tabla1)

tabla2 <- read.csv("data/limpiosOutliers/plasmaAntNoOutliers.csv")

# ----


library(tidyverse)
library(rstatix)
library(scales)

orinaFlav <- read.csv("../data/limpiosOutliers/orinaFlavNoOutliers.csv")[-1]
orinaAnt <- read.csv("../data/limpiosOutliers/orinaAntNoOutliers.csv")[-1]
plasmaAnt <- read.csv("../data/limpiosOutliers/plasmaFlavNoOutliers.csv")[-1]
plasmaFlav <- read.csv("../data/limpiosOutliers/plasmaFlavNoOutliers.csv")[-1]

aov_test <- function(tabla, variable){
  tablaVar <- tabla %>% select(numVol, Endulzante, Sexo, Tiempo, variable)
  
  res.aov <- anova_test(data = tablaVar, dv=variable, wid=numVol, 
                        between = c(Sexo, Endulzante), within= Tiempo)
  
  tablaAnova <- get_anova_table(res.aov, correction = "auto")
  
  print(tablaAnova)
}

aov_loop <- function(tabla){
  
  message(paste("Tabla analizada: ", deparse(substitute(tabla))))
  for (i in seq(1,ncol(tabla))){
    
    if (is.numeric(tabla[,i]) & names(tabla)[i] != "numVol"){
      message(paste("Variable analizada: ", names(tabla)[i]))
      aov_test(tabla,
               names(tabla)[i])
    }
    
    
  }
}

aov_loop(orinaFlav)
aov_loop(orinaAnt)
aov_loop(plasmaAnt)
aov_loop(plasmaFlav)


