### Clustering pacientes + modelo

library(factoextra)

orinaFlav <- read.csv("data/cronicoOrinaFlavLimpio.csv", sep = ";", dec = ",")

orinaFlav[is.na(orinaFlav)]<-0

# tabla <- stack(as.data.frame(orinaFlav[,-1]))
# tabla$Condiciones <- rep(orinaFlav$X, 8) 
orinaFlav$Endulzante <- rep("SA", nrow(orinaFlav))
orinaFlav$Tiempo <- rep("0", nrow(orinaFlav))

# A = Estevia B = sucralosa C = sacarosa

for (i in seq(1, nrow(orinaFlav))){
    
  if (grepl(pattern = "A", x = orinaFlav$X[i])){
    orinaFlav$Endulzante[i] <- "ST"
  }
  else if (grepl(pattern = "B", x = orinaFlav$X[i])){
    orinaFlav$Endulzante[i] <- "SU"
  }
  
}

for (i in seq(1, nrow(orinaFlav))){
  if (grepl(pattern = "F", x = orinaFlav$X[i])){
    orinaFlav$Tiempo[i] <- "Final"
  }
}

orinaFlav


# Non hierarchical clustering ----

## K-means

### Number of centers

fviz_nbclust(x = orinaFlav[,c(2,3,4,5,6,7,8)], FUNcluster = kmeans, method = "wss", k.max = 10, 
             diss = get_dist(orinaFlav[,c(2,3,4,5,6,7,8)]), nstart = 50)

### Plotting

set.seed(123)

km_clusters <- kmeans(x = orinaFlav[,c(2,3,4,5,6,7,8)], centers = 3, nstart = 50)

fviz_cluster(object = km_clusters, data = orinaFlav[,c(2,3,4,5,6,7,8)],
             show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "K-means - Cronico Orina Flavanonas") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


orinaFlav <- cbind(orinaFlav, km_clusters$cluster)

orinaFlav[order(orinaFlav$`km_clusters$cluster`),]

grupo1 <- orinaFlav[orinaFlav$`km_clusters$cluster` == 1,]
grupo2 <- orinaFlav[orinaFlav$`km_clusters$cluster` == 2,]
grupo3 <- orinaFlav[orinaFlav$`km_clusters$cluster` == 3,]


# Análisis de los clusters

summary(aov(ES~ Endulzante+Tiempo, data = grupo1))

install.packages("stargazer")


model1 <- lm(formula = ES ~ Endulzante, data = grupo1)
model2 <- lm(formula = ES ~ Tiempo, data = grupo1)
model3 <- lm(formula = ES ~ Endulzante*Tiempo, data = grupo1)



anova(model1, model2, model3)
library(stargazer)

analisismodel <- function(metabolito) {
  
  # f1 <- as.formula(paste(metabolito, paste("Endulzante")))
  model1 <- lm(as.formula(paste(metabolito,"Endulzante", sep = "~"))
                               , data = grupo1)
  model2 <- lm(as.formula(paste(metabolito,"Tiempo", sep = "~"))
               , data = grupo1)
  model3 <- lm(as.formula(paste(metabolito,"Endulzante*Tiempo", sep = "~"))
               , data = grupo1)
  
  model4 <- lm(as.formula(paste(metabolito,"Endulzante+Tiempo", sep = "~"))
               , data = grupo1)
  
  stargazer(model1, model2, model3, model4,type="html",
            title="Comparación de modelos", out = 
              paste("resultadoAnovas", metabolito, ".html"))
  anova(model1, model2, model3,model4)
  
  }

analisismodel ("NG")




library("nnet")

set.seed(123)

nnmdl <- nnet(formula = ES ~ Endulzante*Tiempo, data = grupo1, size=2, linout=T)

bestrss <- 10000

for(i in 1:100){
  nnmdl <- nnet(formula = ES ~ Endulzante*Tiempo, data = grupo1, size=2, 
                linout=T, trace=F)
  cat(i,nnmdl$value,"\n")
  if(nnmdl$value < bestrss){
    bestnn <- nnmdl
    bestrss <- nnmdl$value
  }}
bestnn$value

summary(nnmdl)
