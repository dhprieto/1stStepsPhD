library(dplyr)
library(scales)
library(purrr)
library(ggpubr)
library(factoextra)
library(clustertend)

preprocessTablas1 <- function(root, nombreTabla) {
  
  # Getting data ready
  rootTabla <- paste0(root, nombreTabla)
  
  tabla <- read.csv(rootTabla)
  
  
  # Make factors of categorical features, deltaing intial-final features
  
  
  tabla$Endulzante <- factor(tabla$Endulzante, levels = c("SA", "ST", "SU"))
  tabla$Sexo <- factor(tabla$Sexo, levels = c("HOMBRE", "MUJER"))
  tabla$Tiempo <- factor(tabla$Tiempo, levels = c("0", "Final"))
  
  tabla$Delta.IRCV <- tabla$IRCV.Final - tabla$IRCV.inicial
  tabla$Delta.Bpmin <- tabla$Bpmin.final - tabla$Bpmin.inicial
  tabla$Delta.Bpmax <- tabla$Bpmax.final - tabla$Bpmax.inicial
  tabla$Delta.Frec <- tabla$Frec.final - tabla$Frec.inicial

  # Removing of trivial redundant and useless features 
  
  set.A <- subset(tabla, select =-c(X.1, numVol, X, Peso.inicial, Peso.final, Talla, IMC.Inicial, IMC.Final, 
                                      Grasa.inicial, Grasa.final, IRCV.Final, IRCV.inicial, Bpmin.final, 
                                      Bpmin.inicial, Bpmax.final, Bpmax.inicial, Frec.final, Frec.inicial))
  
  
  # Only numerical features
  
  set.A_num <- subset(set.A, select=-c(Endulzante, Sexo, Tiempo))
  
  
  #Rescaling, can use "set.A_rescaled <- scale(set.A_num)" too
  set.A_rescaled <- set.A_num %>% mutate_each_(list(~rescale(.) %>% as.vector), 
                                               vars = colnames(set.A_num))
  
  # Validación de método de clustering
  
  ## Manual 
  
  datos_simulados <- map_df(set.A_rescaled,
                            .f = function(x){runif(n = length(x),
                                                   min = min(x),
                                                   max = max(x))
                            }
  )
  
  
  pca_datos_A      <- prcomp(set.A_rescaled)
  pca_datos_simulados <- prcomp(datos_simulados)
  p1 <- fviz_pca_ind(X = pca_datos_A, habillage = set.A$Sexo,
                     geom = "point", title = paste("PCA - datos sexo", nombreTabla), 
                     pallete = "jco") +
    theme_bw() + theme(legend.position = "bottom")
  p2 <- fviz_pca_ind(X = pca_datos_simulados, geom = "point",
                     title = paste("PCA - datos simulados",nombreTabla), pallete = "jco") +
    theme_bw() + theme(legend.position = "bottom")
  
  print(ggarrange(p1, p2))#, common.legend = TRUE)
  
  print(fviz_contrib(pca_datos_A, choice="var", top = 13))
  
  ### Aplicamos clustering 
  
  # K-means clustering
  
  km_datos_A <- kmeans(x = set.A_rescaled, centers = 2)
  p3 <- fviz_cluster(object = km_datos_A, data = set.A_rescaled, 
                     ellipse.type = "norm", geom = "point", main = paste("Datos sexo", nombreTabla),
                     stand = FALSE, palette = "jco", show.legend = F) +
    geom_point(aes(shape = set.A$Sexo), show.legend = TRUE)
  
  km_datos_simulados <- kmeans(x = datos_simulados, centers = 2)
  p4 <- fviz_cluster(object = km_datos_simulados, data = datos_simulados, habillage = set.A$Sexo,
                     ellipse.type = "norm", geom = "point",
                     main = paste("Datos simulados", nombreTabla), stand = FALSE, palette = "jco") +
    theme_bw() + theme(legend.position = "none")
  
  # Hierarchical clustering
  p5 <- fviz_dend(x = hclust(dist(set.A_rescaled)), k = 2, k_colors = "jco",
                  show_labels = FALSE, main = paste("Datos sexo", nombreTabla))
  p6 <- fviz_dend(x = hclust(dist(datos_simulados)), k = 2, k_colors = "jco",
                  show_labels = FALSE, main = paste("Datos simulados", nombreTabla))
  
  print(ggarrange(p3, p4))
  
  print(ggarrange(p5, p6))
  
  ### Estadístico de Hopkins ----
  
  set.seed(101)
  
  hopkins(set.A_rescaled, n = nrow(set.A_rescaled)-1)
  hopkins(datos_simulados, n = nrow(datos_simulados)-1)
  
  ### VAT (Comparación visual) ----
  
  dist_datos_A      <- dist(set.A_rescaled, method = "euclidean")
  dist_datos_simulados <- dist(datos_simulados, method = "euclidean")
  
  p7 <- fviz_dist(dist.obj = dist_datos_A, show_labels = FALSE) +
    labs(title = paste("Datos sexo", nombreTabla)) + theme(legend.position = "bottom")
  p8 <- fviz_dist(dist.obj = dist_datos_simulados, show_labels = FALSE) +
    labs(title = paste("Datos simulados", nombreTabla)) + theme(legend.position = "bottom")
  
  print(ggarrange(p7, p8))
}

resultadoC_O_A.A <- preprocessTablas1("data/","cronicoOrinaAnt_Antro.csv")
resultadoA_O_F.A <- preprocessTablas1("data/","cronicoOrinaFlav_Antro.csv")

preprocessTablas1("data/","cronicoPlasmaAnt_Antro.csv")
preprocessTablas1("data/","cronicoPlasmaFlav_Antro.csv")


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


#Rescaling, can use "set.A_rescaled <- scale(set.A_num)" too
set.A_rescaled <- set.A_num %>% mutate_each_(list(~rescale(.) %>% as.vector), 
                           vars = colnames(set.A_num))


# anovas traviesas:

library(nortest)

lillie.test(x = )

set.A_rescaled_sexo <- cbind(set.A_rescaled, Sexo=set.A$Sexo)

anova_sexo <- aov(Sexo ~. , data = set.A_rescaled_sexo)

summary(anova_sexo)



# Validación de método de clustering ----

## Manual ----

datos_simulados <- map_df(set.A_rescaled,
                          .f = function(x){runif(n = length(x),
                                                 min = min(x),
                                                 max = max(x))
                          }
)


pca_datos_A      <- prcomp(set.A_rescaled)
pca_datos_simulados <- prcomp(datos_simulados)
p1 <- fviz_pca_ind(X = pca_datos_A, habillage = set.A$Sexo,
                   geom = "point", title = "PCA - datos sexo",
                   pallete = "jco") +
  theme_bw() + theme(legend.position = "bottom")
p2 <- fviz_pca_ind(X = pca_datos_simulados, geom = "point",
                   title = "PCA - datos simulados", pallete = "jco") +
  theme_bw() + theme(legend.position = "bottom")

ggarrange(p1, p2)#, common.legend = TRUE)

fviz_contrib(pca_datos_A, choice="var", top = 13)

### Aplicamos clustering ----

# K-means clustering

km_datos_A <- kmeans(x = set.A_rescaled, centers = 2)
p1 <- fviz_cluster(object = km_datos_A, data = set.A_rescaled, 
                   ellipse.type = "norm", geom = "point", main = "Datos sexo",
                   stand = FALSE, palette = "jco", show.legend = F) +
                   geom_point(aes(shape = set.A$Sexo), show.legend = TRUE)

km_datos_simulados <- kmeans(x = datos_simulados, centers = 2)
p2 <- fviz_cluster(object = km_datos_simulados, data = datos_simulados, habillage = set.A$Sexo,
                   ellipse.type = "norm", geom = "point",
                   main = "Datos simulados", stand = FALSE, palette = "jco") +
  theme_bw() + theme(legend.position = "none")

# Hierarchical clustering
p3 <- fviz_dend(x = hclust(dist(set.A_rescaled)), k = 2, k_colors = "jco",
                show_labels = FALSE, main = "Datos sexo")
p4 <- fviz_dend(x = hclust(dist(datos_simulados)), k = 2, k_colors = "jco",
                show_labels = FALSE, main = "Datos simulados")

ggarrange(p1, p2)

ggarrange(p3, p4)

### Estadístico de Hopkins ----

set.seed(101)

hopkins(set.A_rescaled, n = nrow(set.A_rescaled)-1)
hopkins(datos_simulados, n = nrow(datos_simulados)-1)

### VAT (Comparación visual) ----

dist_datos_A      <- dist(set.A_rescaled, method = "euclidean")
dist_datos_simulados <- dist(datos_simulados, method = "euclidean")

p1 <- fviz_dist(dist.obj = dist_datos_A, show_labels = FALSE) +
  labs(title = "Datos sexo") + theme(legend.position = "bottom")
p2 <- fviz_dist(dist.obj = dist_datos_simulados, show_labels = FALSE) +
  labs(title = "Datos simulados") + theme(legend.position = "bottom")

ggarrange(p1, p2)