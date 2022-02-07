library(dummies)
library(tidyverse)
library(scales)
library(caret)
library(FNN)
source("scripts/reading.R")


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


# KNN ----

# Reescalamos variables, usamos rescale por que tenemos las dummies en 0-1

library(dplyr)

set.A_rescaled <- set.A %>% mutate_each_(list(~rescale(.) %>% as.vector), 
                                         vars = colnames(set.A)[which != "Peso"])

#################### mutate_each_ deprecated, look for across()

set.seed(123)

t.id <- createDataPartition(set.A_rescaled$Delta.Peso, p=0.6, list = F)
tr <- set.A_rescaled[t.id, ]
temp <- set.A_rescaled[-t.id, ]
v.id <- createDataPartition(temp$Delta.Peso, p=0.5, list = F)
val <- temp[v.id,]
test <- temp[-v.id, ]

# Pruebas con diferentes valores de K 


reg1 <- knn.reg(tr[,-8], val[,-8], tr$Delta.Peso, k=1, algorithm = "brute")

sqrt(mean(reg1$pred - val$Delta.Peso)^2)

reg2 <- knn.reg(tr[,-8], val[,-8], tr$Delta.Peso, k=2, algorithm = "brute")

sqrt(mean(reg2$pred - val$Delta.Peso)^2)

reg3 <- knn.reg(tr[,-8], val[,-8], tr$Delta.Peso, k=3, algorithm = "brute")

sqrt(mean(reg3$pred - val$Delta.Peso)^2)

reg4 <- knn.reg(tr[,-8], val[,-8], tr$Delta.Peso, k=4, algorithm = "brute")

sqrt(mean(reg4$pred - val$Delta.Peso)^2)

reg5 <- knn.reg(tr[,-8], val[,-8], tr$Delta.Peso, k=5, algorithm = "brute")

sqrt(mean(reg5$pred - val$Delta.Peso)^2)


# Vemos que para k = 4 obtenemos los mejores resultados si no reescalamos la variable respuesta

reg.test <- knn.reg(tr[,-8], test[,-8], tr$Delta.Peso, k=4, algorithm = "brute")

sqrt(mean(reg.test$pred - test$Delta.Peso)^2)


# KNN sin partición de validación ----

t.id <- createDataPartition(set.A_rescaled$Delta.Peso, p=0.7, list = F)
tr <- set.A_rescaled[t.id, ]
val <- set.A_rescaled[-t.id, ]

reg4 <- knn.reg(tr[,-8], NULL, tr$Delta.Peso, k=4, algorithm = "brute")

sqrt(mean(reg4$pred^2))


# Regresión lineal ----

library(caret)

set.seed(2021)

# Transformamos a factor 

c_O_A.A$Endulzante <- factor(c_O_A.A$Endulzante, levels = c("SA", "ST", "SU"))
c_O_A.A$Sexo <- factor(c_O_A.A$Sexo, levels = c("HOMBRE", "MUJER"))
c_O_A.A$Tiempo <- factor(c_O_A.A$Tiempo, levels = c("0", "Final"))

set.A <- c_O_A.A[,-c(1,2,4)]

set.A_rescaled <- set.A %>% mutate_each_(list(~rescale(.) %>% as.vector), 
                                         vars = colnames(set.A)[-c(1,7,10,26)])


t.id <- createDataPartition(set.A$Delta.Peso, p=0.7, list = F)

reg1 <- lm(Delta.Peso ~ ., data = set.A[t.id, ])

summary(reg1)

boxplot(reg1$residuals)

pred1 <- predict(reg1 , set.A_rescaled[-t.id,] )

sqrt(mean(pred1 - set.A_rescaled[-t.id,]$Delta.Peso)^2)


# Hay que reescalar o no?

# Árboles de regresión ----

# Bosques aleatorios ----