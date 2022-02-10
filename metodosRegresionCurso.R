library(dummies)
library(tidyverse)
library(scales)
library(caret)
library(FNN)
source("scripts/reading.R")

set.seed(123)

# métodos regresión


# Lectura tablas ----

listaTablas <- preprocessTables("data/", "tablaPlasmaAnt.csv")$tablaSinEsc

for (i in colnames(listaTablas)) {
  
  if (is.numeric(listaTablas[,i])){
    
    listaTablas <- listaTablas[!listaTablas[, i] %in% boxplot.stats(listaTablas[,i])$out,]
  }
} 

endulzanteDum <- dummy(listaTablas$Endulzante, sep = "_")
sexoDm <- dummy(listaTablas$Sexo, sep = "_")
tiempoDm <- dummy(listaTablas$Tiempo, sep = "_")

datos <- cbind(listaTablas, endulzanteDum, sexoDm, tiempoDm)

for (i in colnames(datos)) {
  
  if (is.numeric(datos[,i]) && i != "Peso"){
    
    datos[,i] <- rescale(datos[,i])
  }
} 

datosNum <- datos %>% select(-c(Sexo, Endulzante, Tiempo, numVol)) 
datosNumMet <- datos %>% 
  dplyr::select(-c(IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                   Endulzante, Sexo, numVol, Tiempo))

# KNN ----

t.id <- createDataPartition(datosNumMet$Peso, p=0.6, list = F)
tr <- datosNumMet[t.id, ]
temp <- datosNumMet[-t.id, ]
v.id <- createDataPartition(temp$Peso, p=0.5, list = F)
val <- temp[v.id,]
test <- temp[-v.id, ]

# Pruebas con diferentes valores de K 

reg1 <- knn.reg(tr[,-7], val[,-7], tr$Peso, k=1, algorithm = "brute")

sqrt(mean(reg1$pred - val$Peso)^2)

reg2 <- knn.reg(tr[,-7], val[,-7], tr$Peso, k=2, algorithm = "brute")

sqrt(mean(reg2$pred - val$Peso)^2)

reg3 <- knn.reg(tr[,-7], val[,-7], tr$Peso, k=3, algorithm = "brute")

sqrt(mean(reg3$pred - val$Peso)^2)

reg4 <- knn.reg(tr[,-8], val[,-8], tr$Peso, k=4, algorithm = "brute")

sqrt(mean(reg4$pred - val$Peso)^2)

reg5 <- knn.reg(tr[,-8], val[,-8], tr$Peso, k=5, algorithm = "brute")

sqrt(mean(reg5$pred - val$Peso)^2)

# Vemos que para k = 4 obtenemos los mejores resultados si no reescalamos la variable respuesta

reg.test <- knn.reg(tr[,-7], test[,-7], tr$Peso, k=4, algorithm = "brute")

sqrt(mean(reg.test$pred - test$Peso)^2)


# KNN sin partición de validación ----

t.id <- createDataPartition(datos$Peso, p=0.7, list = F)
tr <- datos[t.id, ]
val <- datos[-t.id, ]

reg4 <- knn.reg(tr[,-8], NULL, tr$Peso, k=4, algorithm = "brute")

sqrt(mean(reg4$pred^2))


# Regresión lineal ----

library(caret)

set.seed(123)

# Transformamos a factor 

datos <- listaTablas %>%
         dplyr::select(-c(IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                        Endulzante, Sexo, numVol, Tiempo))

for (i in colnames(datos)) {
  
  if (is.numeric(datos[,i]) && i != "Peso"){ 
    
    datos[,i] <- rescale(datos[,i])
  }
} 

t.id <- createDataPartition(datos$Peso, p=0.7, list = F)

reg1 <- lm(Peso ~ ., data = datos[t.id, ])

summary(reg1)

boxplot(reg1$residuals)

pred1 <- predict(reg1 , datos[-t.id, ] )

sqrt(mean(pred1 - datos[-t.id, ]$Peso)^2)


# Árboles de regresión ----

# Bosques aleatorios ----