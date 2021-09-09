# Análisis PCA de datos crónico antropométrico

library(FactoMineR)
library(factoextra)

# Lectura tablas ----

c_O_A.A <- readRDS("data/cronicoOrinaAnt_Antro.csv")

# Retiramos datos ordinales y texto

set.A <- c_O_A.A[,-c(1,2,3,9)]

# PCA ----

pca.A <- PCA(set.A,  scale.unit = T, ncp = 214, graph = F)
pca.A2 <- prcomp(set.A, scale = TRUE)


# Lista de componentes del objeto pca.a

print(pca.A)

# Observación de los eigenvalues (autovalores?), que explican la varianza

pca.A$eig


# Representación gráfica ----

fviz_pca_ind(pca.A, geom.ind = "point", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), 
             pointsize = 1.5)
colnames(set.A)


# Seleccion de variables ----

fviz_contrib(pca.A, choice = "var", axes = 2, top = 120)

# Proporción de la varianza explicada ----

PVE <- 100*pca.A2$sdev^2/sum(pca.A2$sdev^2)
PVE
