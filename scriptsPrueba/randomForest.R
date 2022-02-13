source("scripts/preprocess.R")

## Clasification script

## Reading tables ----


tabla1 <- preprocessTables("data/", "tablaOrinaAnt.csv")  

tabla1.1 <- removeOutliers(tabla1$tablaSinEsc %>% 
                             select(-c(numVol, Peso, Grasa, IRCV, IMC, Bpmin, Bpmax, Frec))) 

tabla1.1 <- escaladoTablas(tabla1.1)

library(caret)
library(randomForest)

set.seed(123)

training.ids <- createDataPartition(tabla1.1$Sexo, p = 0.7, list = F)

mod <- randomForest(Sexo ~ ., data = tabla1.1[training.ids,],
                    ntree = 5000,
                    keep.forest = TRUE)

pred <- predict(mod, tabla1.1[-training.ids,], type = "class")

table(tabla1.1[-training.ids,"Sexo"], pred, dnn= c("Actual", "Predicho"))

library(ROCR)

probs <- predict(mod, tabla1.1[-training.ids,], type = "prob")
head(probs)

pred <- prediction(probs[,2], tabla1.1[-training.ids,"Sexo"])
perf <- performance(pred, "tpr", "fpr")
plot(perf)

### Confussion matrix ----

table <- table(tabla1.1[-training.ids,]$Sexo,  pred.pruned, 
               dnn =  c("Actual", "Predecido"))
table

prop.table(table)

round(prop.table(table, 1)*100, 2)

round(prop.table(table, 2)*100, 2)

barplot(table, legend = TRUE, 
        xlab = "Nota predecida por el modelo")

mosaicplot(table, main = "Eficiencia del modelo")

summary(table) 
