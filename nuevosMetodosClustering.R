library(factoextra)
library(ggpubr)
library(grid)
library(gridExtra)
library(NbClust)
library(clValid)
library(mclust)
library(kohonen)

source("scripts/preprocess.R")

# lectura

orinaFlav <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaOrinaFlav.csv")$tablaFactors))
orinaAnt <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaorinaAnt.csv")$tablaFactors))
plasmaAnt <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaplasmaAnt.csv")$tablaFactors))
plasmaFlav <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaplasmaFlav_adjusted.csv")$tablaFactors))

anthro <- c("Peso", "IMC", "Grasa", "IRCV", 
            "Bpmin", "Bpmax", "Frec")

# orinaFlav ----

NbClust(data = orinaFlav %>% filter(Tiempo == "0") 
        %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec,
                      Endulzante, Sexo, numVol, Tiempo)), 
        distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = "kmeans", index = "all", alphaBeale = 0.1)

# 8

comparacionOF <- clValid(
  obj        = orinaFlav %>% filter(Tiempo == "0") %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                                                     Endulzante, Sexo, numVol, Tiempo)),
  nClust     = 8,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionOF) # hierarchical/pam/kmeans

clustersOF_T0 <- pam(orinaFlav %>% filter(Tiempo == "0") %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                                      Endulzante, Sexo, numVol, Tiempo)), 8)

p11 <- fviz_cluster(clustersOF_T0, ellipse.type = "convex", repel = T)+
  labs(title = "Proyección PCA orinaFlav Tiempo 0", subtitle = "PAM Clustering")+
  theme_bw()+
  theme(legend.position = "bottom")



p1 <- clusterSinTiempo(orinaFlav %>% filter(Tiempo == "0")
                       %>% select (-anthro, -Tiempo, Sexo, Endulzante), 
                       clustersOF_T0$clustering)

p3 <- clusterSinTiempo(orinaFlav %>% filter(Tiempo == "0")
                       %>% select (all_of(anthro), -Tiempo,numVol, Sexo, Endulzante), 
                       clustersOF_T0$clustering)


# T == Final

NbClust(data = orinaFlav %>% filter(Tiempo == "Final") 
        %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec,
                      Endulzante, Sexo, numVol, Tiempo)), 
        distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = "kmeans", index = "all", alphaBeale = 0.1)
# 6


comparacionOF <- clValid(
  obj        = orinaFlav %>% filter(Tiempo == "Final") %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                Endulzante, Sexo, numVol, Tiempo)),
  nClust     = 6,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionOF) # kmeans

clustersOF_TF <- kmeans(orinaFlav %>% filter(Tiempo == "Final") %>% 
                 select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, 
                 Frec, Endulzante, Sexo, numVol, Tiempo)), centers = 6, nstart = 50)


p12 <- fviz_cluster(clustersOF_TF, data = orinaFlav %>% filter (Tiempo == "Final") %>% 
                      select(-c(Endulzante, Sexo, numVol, Tiempo)), ellipse.type = "convex")+
  labs(title = "Proyección PCA orinaFlav Tiempo Final", subtitle = "Kmeans Clustering")+
  theme_bw()+
  theme(legend.position = "bottom")


p2 <- clusterSinTiempo(orinaFlav %>% filter(Tiempo == "Final")
                       %>% select (-anthro, -Tiempo, Sexo, Endulzante), 
                       clustersOF_TF$cluster)

p4 <- clusterSinTiempo(orinaFlav %>% filter(Tiempo == "Final")
                       %>% select (all_of(anthro), -Tiempo,numVol, Sexo, Endulzante), 
                       clustersOF_TF$cluster)

ggarrange(p11,p12)
ggarrange(p3,p4)

comparacionOA <- clValid(
  obj        = orinaAnt %>% filter(Tiempo == "0")%>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                      Endulzante, Sexo, numVol, Tiempo)),
  nClust     = 3,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionOA) # hierarchical

# Optimal Scores:
#   
#   Score   Method       Clusters
# APN           0.0471 hierarchical 3       
# AD            0.5133 som          3       
# ADM           0.0913 hierarchical 3       
# FOM           0.2025 clara        3       
# Connectivity 13.0802 hierarchical 3       
# Dunn          0.1997 hierarchical 3       
# Silhouette    0.2766 hierarchical 3 


comparacionPF <- clValid(
  obj        = plasmaFlav %>% filter(Tiempo == "Final") %>% select(-c(IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                        Endulzante, Sexo, numVol, Tiempo)),
  nClust     = 2:6,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionPF) # 0 hierarchical 6 Final 4 clara/pam

# Optimal Scores:
#   
#   Score  Method       Clusters
# APN          0.2737 diana        3       
# AD           0.4165 pam          3       
# ADM          0.1359 hierarchical 3       
# FOM          0.2198 kmeans       3       
# Connectivity 7.3341 hierarchical 3       
# Dunn         0.2288 hierarchical 3       
# Silhouette   0.2770 diana        3      

comparacionPA <- clValid(
  obj        = plasmaAnt %>% filter(Tiempo == "Final")%>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                       Endulzante, Sexo, numVol, Tiempo)),
  nClust     = 2:6,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionPA) # hierarchical
 
# Optimal Scores:
#   
#   Score   Method       Clusters
# APN           0.1071 hierarchical 3       
# AD            0.6536 kmeans       3       
# ADM           0.1036 hierarchical 3       
# FOM           0.2177 diana        3       
# Connectivity 38.3520 hierarchical 3       
# Dunn          0.2400 hierarchical 3       
# Silhouette    0.2369 hierarchical 3       


# plasmaAnt ----

matriz_distancias <- dist(x = plasmaAnt %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                      Endulzante, Sexo, numVol, Tiempo)) , method = "euclidean")
hc_completo <- hclust(d = matriz_distancias, method = "complete")
hc_average  <- hclust(d = matriz_distancias, method = "average")
hc_single   <- hclust(d = matriz_distancias, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersPA <- cutree(hc_completo, k=3)

clusterNuevoPlot(plasmaAnt, clustersPA)

# plasmaFlav ----

matriz_distancias <- dist(x = plasmaFlav %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                                      Endulzante, Sexo, numVol, Tiempo)) , method = "euclidean")
hc_completo <- hclust(d = matriz_distancias, method = "complete")
hc_average  <- hclust(d = matriz_distancias, method = "average")
hc_single   <- hclust(d = matriz_distancias, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersPF <- cutree(hc_average, k=3)

clusterNuevoPlot(plasmaFlav, clustersPF)

# orinaFlav ----

matriz_distancias <- dist(x = orinaFlav %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                                       Endulzante, Sexo, numVol, Tiempo)) , method = "euclidean")
hc_completo <- hclust(d = matriz_distancias, method = "complete")
hc_average  <- hclust(d = matriz_distancias, method = "average")
hc_single   <- hclust(d = matriz_distancias, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersOF <- cutree(hc_completo, k=3)

clusterNuevoPlot(orinaFlav, clustersOF)

# orinaAnt ----

matriz_distancias <- dist(x = orinaAnt %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                                      Endulzante, Sexo, numVol, Tiempo)) , method = "euclidean")
hc_completo <- hclust(d = matriz_distancias, method = "complete")
hc_average  <- hclust(d = matriz_distancias, method = "average")
hc_single   <- hclust(d = matriz_distancias, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersOA <- cutree(hc_completo, k=3)

clusterNuevoPlot(orinaAnt, clustersOA)


### funciones ----

clusterNuevoPlot <- function(tabla, clusters){

  tabla_clusters <- tabla %>% tibble::add_column(clusters) %>% select(-numVol)
  
  tableSexo <- table(tabla_clusters$Sexo, tabla_clusters$clusters)#tabla_clusters %>% count(Sexo, clusters)  
  tableEdulcorante <- table(tabla_clusters$Endulzante, tabla_clusters$clusters) #tabla_clusters %>% count(Endulzante, clusters)
  
  tabla_clusters$Endulzante <- rescale(as.numeric(tabla_clusters$Endulzante))
  tabla_clusters$Sexo <- rescale(as.numeric(tabla_clusters$Sexo))
  
  longtableOF <- reshape2::melt(tabla_clusters, id = c("clusters", "Tiempo"))
  
  longtableOF <- tabla_clusters %>% gather(variable, values, -clusters, -Tiempo, )
  
  ggplot(longtableOF, aes(factor(variable, level = unique(longtableOF$variable)),as.numeric(values), fill=factor(clusters))) +
    geom_boxplot()+
    annotate("text", x = which(unique(longtableOF$variable)=="Sexo"), y = 1.03, label = "Mujer") + 
    annotate("text",x = which(unique(longtableOF$variable)=="Sexo"), y = -0.03, label = "Hombre") +
    annotate("text", x = which(unique(longtableOF$variable)=="Endulzante"), y = 1.03, label = "SU") + 
    annotate("text",x = which(unique(longtableOF$variable)=="Endulzante"), y = -0.03, label = "SA")+
    annotation_custom(grob = tableGrob(tableSexo, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0.75, ymax=1)+
    annotation_custom(grob = tableGrob(tableEdulcorante, rows=c("SA", "ST","SU"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0, ymax=0.25)+
    ggtitle(paste("Boxplot Cluster Analysis ", deparse(substitute(tabla))))+
    theme(axis.text=element_text(angle = 45, size=4),
          axis.title=element_text(size=14,face="bold"))+
    labs(y = "standarized value", x = "variables/clusters")+
    facet_wrap(~Tiempo)
  
}

# colors: ST-RED, SU-GREEN, SA-BLACK

### pruebas ----

##### orina Ant----

# T == 0

matriz_distanciasT_0 <- dist(x = orinaAnt %>%
                               filter(Tiempo == "0") %>% 
                               select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                                     Endulzante, Sexo, numVol, Tiempo)) , 
                             method = "euclidean")
hc_completo <- hclust(d = matriz_distanciasT_0, method = "complete")
hc_average  <- hclust(d = matriz_distanciasT_0, method = "average")
hc_single   <- hclust(d = matriz_distanciasT_0, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersOA_T0 <- cutree(hc_completo, k=3)

p1 <- clusterSinTiempo(orinaAnt %>% filter (Tiempo == "0"), clustersOA_T0)


# T == Final

matriz_distanciasT_F <- dist(x = orinaAnt %>%
                               filter(Tiempo == "Final") %>% 
                               select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                         Endulzante, Sexo, numVol, Tiempo)) , 
                             method = "euclidean")
hc_completo <- hclust(d = matriz_distanciasT_F, method = "complete")
hc_average  <- hclust(d = matriz_distanciasT_F, method = "average")
hc_single   <- hclust(d = matriz_distanciasT_F, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersOA_TF <- cutree(hc_completo, k=3)

p2 <- clusterSinTiempo(orinaAnt %>% filter (Tiempo == "Final"), clustersOA_TF)


ggarrange(p1,p2)





clusterSinTiempo <- function(tabla, clusters){
  
  tabla_clusters <- tabla %>% tibble::add_column(clusters) %>% select(-numVol)
  
  tableSexo <- table(tabla_clusters$Sexo, tabla_clusters$clusters)#tabla_clusters %>% count(Sexo, clusters)  
  tableEdulcorante <- table(tabla_clusters$Endulzante, tabla_clusters$clusters) #tabla_clusters %>% count(Endulzante, clusters)
  
  tabla_clusters$Endulzante <- rescale(as.numeric(tabla_clusters$Endulzante))
  tabla_clusters$Sexo <- rescale(as.numeric(tabla_clusters$Sexo))
  
  longtableOF <- reshape2::melt(tabla_clusters, id = c("clusters"))
  
  longtableOF <- tabla_clusters %>% gather(variable, values, -clusters)
  
  p <- ggplot(longtableOF, aes(factor(variable, level = unique(longtableOF$variable)),as.numeric(values), fill=factor(clusters))) +
    geom_boxplot()+
    annotate("text", x = which(unique(longtableOF$variable)=="Sexo"), y = 1.03, label = "Mujer") + 
    annotate("text",x = which(unique(longtableOF$variable)=="Sexo"), y = -0.03, label = "Hombre") +
    annotate("text", x = which(unique(longtableOF$variable)=="Endulzante"), y = 1.03, label = "SU") + 
    annotate("text",x = which(unique(longtableOF$variable)=="Endulzante"), y = -0.03, label = "SA")+
    annotation_custom(grob = tableGrob(tableSexo, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0.75, ymax=1)+
    annotation_custom(grob = tableGrob(tableEdulcorante, rows=c("SA", "ST","SU"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0, ymax=0.25)+
    ggtitle(paste("Boxplot Cluster Analysis ", deparse(substitute(tabla))))+
    theme(axis.text=element_text(angle = 45, size=4),
          axis.title=element_text(size=14,face="bold"))+
    labs(y = "standarized value", x = "variables/clusters")
    
  return(p)
}



##### orina Flav----

# T == 0

matriz_distanciasT_0 <- dist(x = orinaFlav %>%
                               filter(Tiempo == "0") %>% 
                               select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                         Endulzante, Sexo, numVol, Tiempo)) , 
                             method = "euclidean")
hc_completo <- hclust(d = matriz_distanciasT_0, method = "complete")
hc_average  <- hclust(d = matriz_distanciasT_0, method = "average")
hc_single   <- hclust(d = matriz_distanciasT_0, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersOA_T0 <- cutree(hc_completo, k=3)

p1 <- clusterSinTiempo(orinaFlav %>% filter (Tiempo == "0"), clustersOA_T0)

# T == Final

matriz_distanciasT_F <- dist(x = orinaFlav %>%
                               filter(Tiempo == "Final") %>% 
                               select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                         Endulzante, Sexo, numVol, Tiempo)) , 
                             method = "euclidean")
hc_completo <- hclust(d = matriz_distanciasT_F, method = "complete")
hc_average  <- hclust(d = matriz_distanciasT_F, method = "average")
hc_single   <- hclust(d = matriz_distanciasT_F, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersOA_TF <- cutree(hc_average, k=3)

p2 <- clusterSinTiempo(orinaFlav %>% filter (Tiempo == "Final"), clustersOA_TF)

ggarrange(p1,p2)

##### plasma Flav----

# T == 0

matriz_distanciasT_0 <- dist(x = plasmaFlav %>%
                               filter(Tiempo == "0") %>% 
                               select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                         Endulzante, Sexo, numVol, Tiempo)) , 
                             method = "euclidean")
hc_completo <- hclust(d = matriz_distanciasT_0, method = "complete")
hc_average  <- hclust(d = matriz_distanciasT_0, method = "average")
hc_single   <- hclust(d = matriz_distanciasT_0, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersPF_T0 <- cutree(hc_completo, k=3)

p1 <- clusterSinTiempo(plasmaFlav %>% filter (Tiempo == "0"), clustersPF_T0)

# T == Final

matriz_distanciasT_F <- dist(x = plasmaFlav %>%
                               filter(Tiempo == "Final") %>% 
                               select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                         Endulzante, Sexo, numVol, Tiempo)) , 
                             method = "euclidean")
hc_completo <- hclust(d = matriz_distanciasT_F, method = "complete")
hc_average  <- hclust(d = matriz_distanciasT_F, method = "average")
hc_single   <- hclust(d = matriz_distanciasT_F, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersPF_TF <- cutree(hc_completo, k=3)

p2 <- clusterSinTiempo(plasmaFlav %>% filter (Tiempo == "Final"), clustersPF_TF)

ggarrange(p1,p2)


##### plasma Ant----

# T == 0

matriz_distanciasT_0 <- dist(x = plasmaAnt %>%
                               filter(Tiempo == "0") %>% 
                               select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                         Endulzante, Sexo, numVol, Tiempo)) , 
                             method = "euclidean")
hc_completo <- hclust(d = matriz_distanciasT_0, method = "complete")
hc_average  <- hclust(d = matriz_distanciasT_0, method = "average")
hc_single   <- hclust(d = matriz_distanciasT_0, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersPA_T0 <- cutree(hc_completo, k=3)

p1 <- clusterSinTiempo(plasmaAnt %>% filter (Tiempo == "0"), clustersPA_T0)

# T == Final

matriz_distanciasT_F <- dist(x = plasmaAnt %>%
                               filter(Tiempo == "Final") %>% 
                               select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, 
                                         Endulzante, Sexo, numVol, Tiempo)) , 
                             method = "euclidean")
hc_completo <- hclust(d = matriz_distanciasT_F, method = "complete")
hc_average  <- hclust(d = matriz_distanciasT_F, method = "average")
hc_single   <- hclust(d = matriz_distanciasT_F, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

clustersPA_TF <- cutree(hc_average, k=3)

p2 <- clusterSinTiempo(plasmaAnt %>% filter (Tiempo == "Final"), clustersPA_TF)

ggarrange(p1,p2)
