# LDA

source("scripts/preprocess.R")
library(MASS)
library(caret)

## Reading tables ----

tabla1 <- preprocessTables("data/", "tablaOrinaAnt.csv")  

tabla1.1 <- removeOutliers(tabla1$tablaSinEsc %>% 
                             dplyr::select(-c(numVol, Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec,
                                              Endulzante, Tiempo))) 
tabla1.1 <- escaladoTablas(tabla1.1)

t.id <- createDataPartition(tabla1.1$Sexo, p=0.7, list = F)

mod <- lda(tabla1.1[t.id,-6], tabla1.1[t.id,6])
#mod <- lda(Sexo ~., data = tabla1.1[t.id,])

tabla1.1[t.id, "Pred"] <- predict(mod, tabla1.1[t.id,-6])

table(tabla1.1[t.id, "Sexo"], tabla1.1[t.id, "Pred"], dnn = c("Actual", "Predichos"))

tabla1.1[-t.id, "Pred"] <- predict(mod, tabla1.1[-t.id, 1:4])$Sexo 
table(tabla1.1[-t.id, "Sexo"], tabla1.1[-t.id, "Pred"], dnn = c("Actual", "Predichos"))
