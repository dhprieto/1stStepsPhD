library(class)
library(caret)
source("scripts/preprocess.R")


tabla1 <- preprocessTables("data/", "tablaOrinaAnt.csv")  

tabla1.1 <- removeOutliers(tabla1$tablaSinEsc %>% 
                             select(-c(numVol, Peso, Grasa, IRCV, IMC, Bpmin, 
                                       Bpmax, Frec, Endulzante, Tiempo))) 

tabla1.1 <- escaladoTablas(tabla1.1) 

set.seed(123)

t.ids <- createDataPartition(tabla1.1$Sexo, p=0.5, list = F)
train <- tabla1.1[t.ids, ]
temp <- tabla1.1[-t.ids, ]
v.ids <- createDataPartition(temp$Sexo, p=0.5, list = F)
val <- temp[v.ids,]
test <- temp[-v.ids,]


pred1 <- knn(train = train[,-6], 
             test = val[,-6], 
             cl = as.factor(train[,6]), 
             k = 5)
errmat1 <- table(val$Sexo, pred1, dnn = c("Actual", "Predichos"))
errmat1

pred2 <- knn(train = train[,-6], 
             test = val[,-6], 
             cl = as.factor(train[,6]), k =1)

errmat2 <- table(test$Sexo, pred2, dnn = c("Actual", "Predichos"))
errmat2



knn.automate <- function(tr_predictors, val_predictors, tr_target,
                         val_target, start_k, end_k){
  for (k in start_k:end_k) {
    pred <- knn(tr_predictors, val_predictors, tr_target, k)
    tab <- table(val_target, pred, dnn = c("Actual", "Predichos") )
    cat(paste("Matriz de confusión para k = ",k,"\n"))
    cat("==============================\n")
    print(tab)
    cat("------------------------------\n")
  }
}


knn.automate(train[,-6], 
             val[,-6], 
             train[,6],
             val[,6],1,8)


trcntrl <- trainControl(method="repeatedcv", number = 10, repeats = 3)

caret_knn_fit <- train(Sexo ~ ., data = train,
                       method = "knn", trControl = trcntrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)

caret_knn_fit
