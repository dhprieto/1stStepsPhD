# Métodos no paramétricos

# Regression Tree ----

# Lectura tablas ----

c_O_A.A <- readRDS("data/cronicoOrinaAnt_Antro.csv")

# Retiramos datos ordinales y texto

set.A <- c_O_A.A[,-c(1,2,3,9)]

library(rpart)

t1st <- rpart(Peso.inicial ~ ., set.A)

plot(jitter(predict(t1st)), residuals(t1st), xlab="Fitted", ylab="Residuals")
abline(h=0)
qqnorm(residuals(t1st))
qqline(residuals(t1st))

x0 <- apply(set.A[,-6],2,median)

predict(t1st, data.frame(t(x0)))

# Random forest ----

library("randomForest")

fmod <- randomForest (Peso.final ~ ., set.A)
plot(fmod, main="")

# Redes neuronales ----


library("nnet")

set.seed(123)

nnmdl <- nnet(formula = Peso.final ~ ., data = set.A, size=2, linout=T)

bestrss <- 10000

for(i in 1:100){
  nnmdl <- nnet(formula = Peso.final ~ ., data = set.A, size=2, 
                linout=T, trace=F)
  cat(i,nnmdl$value,"\n")
  if(nnmdl$value < bestrss){
    bestnn <- nnmdl
    bestrss <- nnmdl$value
  }}

bestnn$value

summary(nnmdl)
