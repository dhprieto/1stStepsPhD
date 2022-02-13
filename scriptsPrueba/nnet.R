source("scripts/preprocess.R")
library(caret)
library(nnet)
## Clasification script

## Reading tables ----

set.seed(123)
tabla1 <- preprocessTables("data/", "tablaOrinaAnt.csv")  

tabla1.1 <- removeOutliers(tabla1$tablaSinEsc %>% 
                             select(-c(numVol, Peso, Grasa, IRCV,IMC, Bpmin, Bpmax, Frec))) 

tabla1.1 <- escaladoTablas(tabla1.1)


t.id <- createDataPartition(tabla1.1$Sexo
                            , p= 0.7, list = F)

mod <- nnet(Sexo ~ ., data = tabla1.1[t.id,], 
            size = 3, maxit = 10000, decay = .001, rang = 0.05,
            na.action = na.omit, skip = T)

#rang * max(|variables|) ~ 1

pred <- predict(mod, newdata = tabla1.1[-t.id,], type = "class")


table(tabla1.1[-t.id,]$Sexo, pred,dnn = c("Actual", "Predichos") )


library(ROCR)

pred2 <- predict(mod, newdata = tabla1.1[-t.id,], type = "raw")
perf <- performance(prediction(pred2, tabla1.1[-t.id,"Sexo"]), 
                    "tpr", "fpr")
plot(perf)
