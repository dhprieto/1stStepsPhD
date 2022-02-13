
source("scripts/preprocess.R")

## Clasification script

## Reading tables ----


tabla1 <- preprocessTables("data/", "tablaOrinaAnt.csv")  

tabla1.1 <- removeOutliers(tabla1$tablaSinEsc %>% 
                             select(-c(numVol, Peso, Grasa, IRCV, Bpmin, Bpmax, Frec))) 

tabla1.1 <- escaladoTablas(tabla1.1)

### Classification trees ----

library(caret)
library(rpart)
library(rpart.plot)

set.seed(123)


training.ids <- createDataPartition(tabla1.1$Sexo, p = 0.7, list = F)

#class ~ . <-> class ~ variance + skew + curtosis + entropy

mod <- rpart(Sexo ~ . , 
             data = tabla1.1[training.ids,],
             method = "class", 
             control = rpart.control(minsplit = 5, cp = 0.01))

mod

# Representación del árbol

prp(mod, type = 2, extra = 104, nn = TRUE, 
    fallen.leaves = TRUE, faclen = 4, varlen = 8,
    shadow.col = "gray") 

pred.mod <- predict(mod, tabla1.1[-training.ids,], type="class")

table(tabla1.1[-training.ids,]$Sexo, pred.mod, 
      dnn = c("Actual", "Predicho"))


mod$cptable 

# Componentes principales hago la suma del minimo xerror + xstd para elegir CP

mod.pruned <- prune(mod, mod$cptable[4, "CP"])

prp(mod.pruned, type = 2, extra = 104, nn = TRUE,
    fallen.leaves = TRUE, faclen = 4, varlen = 8,
    shadow.col = "gray")


pred.pruned <- predict(mod.pruned, tabla1.1[-training.ids,], type="class")

table(tabla1.1[-training.ids,]$Sexo, pred.pruned, 
      dnn = c("Actual", "Predicho"))

pred.pruned2 <- predict(mod.pruned, tabla1.1[-training.ids,], type = "prob")



head(pred.pruned)
head(pred.pruned2)

library(ROCR)

pred <- prediction(pred.pruned2[,2], tabla1.1[-training.ids, "Sexo"])
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
