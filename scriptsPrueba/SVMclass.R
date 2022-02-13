source("scripts/preprocess.R")
library(e1071)
library(caret)

## Reading tables ----

tabla1 <- preprocessTables("data/", "tablaOrinaAnt.csv")  

tabla1.1 <- removeOutliers(tabla1$tablaSinEsc %>% 
                             select(-c(numVol, Peso, Grasa, IRCV, Bpmin, Bpmax, Frec))) 
tabla1.1 <- escaladoTablas(tabla1.1)

set.seed(123)

t.ids <- createDataPartition(tabla1.1$Sexo, p = 0.7, list = F)

mod <- svm(Sexo ~ ., data = tabla1.1[t.ids, ], 
           class.weights = c("HOMBRE"=0.5, "MUJER"=0.5),
           cost=1000)

table(tabla1.1[t.ids,"Sexo"], fitted(mod), dnn = c("Actual", "Predicho"))

pred <- predict(mod, tabla1.1[-t.ids,])

table(tabla1.1[-t.ids, "Sexo"], pred, dnn = c("Actual", "Predicho"))

plot(mod, data = tabla1.1[t.ids,], skew ~ variance)
plot(mod, data = tabla1.1[-t.ids,], skew ~ variance)


tuned <- tune.svm(Sexo ~ ., data = tabla1.1[t.ids,], 
                  gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)
