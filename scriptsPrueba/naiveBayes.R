source("scripts/preprocess.R")
library(e1071)
library(caret)

## Reading tables ----

tabla1 <- preprocessTables("data/", "tablaOrinaAnt.csv")  

tabla1.1 <- removeOutliers(tabla1$tablaSinEsc %>% 
                             select(-c(numVol, Peso, Grasa, IRCV, Bpmin, Bpmax, Frec))) 
tabla1.1 <- escaladoTablas(tabla1.1)

set.seed(123)

t.ids <- createDataPartition(tabla1.1$Sexo, p = 0.67, list = F)
mod <- naiveBayes(Sexo ~ ., data = tabla1.1[t.ids,])
mod

pred <- predict(mod, tabla1.1[-t.ids,])
tab <- table(tabla1.1[-t.ids,]$Sexo, pred, dnn = c("Actual", "Predicha"))

confusionMatrix(tab)
