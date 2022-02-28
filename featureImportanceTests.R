library(caret)
library(AppliedPredictiveModeling)
source("scripts/preprocess.R")

listaTablas <- preprocessTables("data/", "tablaOrinaAnt.csv")

tablaNum <- listaTablas$tablaNum
tablaFactors <- listaTablas$tablaFactors

tablaFactorsMet <- tablaFactors %>%  
  dplyr::select(-c(numVol, Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec))

tablaNumMet <- tablaNum %>% 
  dplyr::select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec))

tablaNumMet

# KNN ----

# feature visualization

transparentTheme(trans = .9)
featurePlot(x = tablaNumMet, 
            y = tablaFactors$Sexo,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(2,1), 
            auto.key = list(columns = 3))


featurePlot(x = tablaNumMet, 
            y = tablaFactors$Sexo, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 2))


# feature selection ----

set.seed(101)

# calculate correlation matrix
correlationMatrix <- cor(tablaNumMet)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)



# Feature importance ----

set.seed(101)

# load the library

library(caret)

# prepare training scheme

control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model

model <- train(Endulzante~., data=tablaFactorsMet, method="lvq", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)




# rfe ----

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(tablaFactorsMet, tablaFactors$Sexo, sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
