library(caret)
library(AppliedPredictiveModeling)
# Getting data ready ----

c_O_A.A <- read.csv("data/cronicoOrinaAnt_Antro.csv")

# Make factors of categorical features, deltaing intial-final features

c_O_A.A$Endulzante <- factor(c_O_A.A$Endulzante, levels = c("SA", "ST", "SU"))
c_O_A.A$Sexo <- factor(c_O_A.A$Sexo, levels = c("HOMBRE", "MUJER"))
c_O_A.A$Tiempo <- factor(c_O_A.A$Tiempo, levels = c("0", "Final"))

c_O_A.A$Delta.IRCV <- c_O_A.A$IRCV.Final - c_O_A.A$IRCV.inicial
c_O_A.A$Delta.Bpmin <- c_O_A.A$Bpmin.final - c_O_A.A$Bpmin.inicial
c_O_A.A$Delta.Bpmax <- c_O_A.A$Bpmax.final - c_O_A.A$Bpmax.inicial
c_O_A.A$Delta.Frec <- c_O_A.A$Frec.final - c_O_A.A$Frec.inicial

# Removing of trivial redundant and useless features 

set.A <- subset(c_O_A.A, select =-c(X.1, numVol, X, Peso.inicial, Peso.final, Talla, IMC.Inicial, IMC.Final, 
                                    Grasa.inicial, Grasa.final, IRCV.Final, IRCV.inicial, Bpmin.final, 
                                    Bpmin.inicial, Bpmax.final, Bpmax.inicial, Frec.final, Frec.inicial))

# Only numerical features

set.A_num <- subset(set.A, select=-c(Endulzante, Sexo, Tiempo))


# feature visualization

transparentTheme(trans = .9)
featurePlot(x = set.A_num, 
            y = set.A$Sexo,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(2,1), 
            auto.key = list(columns = 3))


featurePlot(x = set.A_num, 
            y = set.A$Sexo, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 2))


# feature selection ----

set.seed(101)

# calculate correlation matrix
correlationMatrix <- cor(set.A_num)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)



# Feature importance ----

set.seed(101)

# load the library

library(caret)

# prepare training scheme

control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model

model <- train(Sexo~., data=set.A, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)




# rfe ----

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(scale(set.A_num[, 1:5]), set.A$Sexo, sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))



# Anova ----



anova <- aov(Peso.final ~ ., data = set.A)
summary(anova)
par(mfrow=c(1,2))
plot(anova, which=1:4)
par(mfrow = c(1,1))
lsr::etaSquared(anova)



