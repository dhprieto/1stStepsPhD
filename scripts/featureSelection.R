library(caret)
library(AppliedPredictiveModeling)
source("scripts/preprocess.R")

set.seed(123)

# lectura

orinaFlav <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaOrinaFlav.csv")$tablaFactors))
orinaAnt <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaorinaAnt.csv")$tablaFactors))
plasmaAnt <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaplasmaAnt.csv")$tablaFactors))
plasmaFlav <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaplasmaFlav_adjusted.csv")$tablaFactors))

anthro <- c("Peso", "IMC", "Grasa", "IRCV", 
            "Bpmin", "Bpmax", "Frec")

## corr ----

correlatedFeatures <- function (tabla) {
  
  
  anthro <- c("Peso", "IMC", "Grasa", "IRCV", 
              "Bpmin", "Bpmax", "Frec")
  
  
  tablaNum <- tabla %>% select(-c(all_of(anthro), Sexo, Endulzante, numVol, Tiempo))
  
  corrMatrix <- cor(tablaNum)
  print(corrMatrix)
  highcorr <- findCorrelation(corrMatrix, 0.5)
  
  print(highcorr)
  
    
}
  

## rankby importance ----

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Tiempo~., data=orinaFlav%>%select(-all_of(anthro), -numVol), method="lvq", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

rankImportanceClass <- function (tabla){
  
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  
  anthro <- c("Peso", "IMC", "Grasa", "IRCV", 
              "Bpmin", "Bpmax", "Frec")
  
  
  
  for (i in c("Sexo", "Tiempo", "Endulzante")){
    print (paste("Factor: ", i ))
    model <- train(formula(paste0(i,"~.")), data=tabla%>%select(-all_of(anthro), -numVol), 
                   method="lvq", trControl=control)
    # estimate variable importance
    importance <- varImp(model, scale=FALSE)
    # summarize importance
    print("------------------------")
    print(importance)
    print("------------------------")
    
    # plot importance
    print(plot(importance, main = paste("Factor analizado: ", i)))
    
    
    
  } 
  
}

rankImportanceClass(plasmaFlav)


# rfe ----

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(plasmaFlav, tablaFactors$Sexo, sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))




## KNN ----

# feature visualization

transparentTheme(trans = .9)
featurePlot(x = orinaFlavNum, 
            y = orinaFlav$Sexo,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(2,1), 
            auto.key = list(columns = 3))


featurePlot(x = orinaFlavNum, 
            y = orinaFlav$Sexo, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 2))
