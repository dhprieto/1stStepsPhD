source("scripts/preprocess.R")
library(mclust)
library(factoextra)
library(ggpubr)
library(reshape2)
library(gridExtra)
library(clValid)
# lectura
orinaFlav <- read.csv("data/mainUrineFlav.csv")[-1]
orinaAnt <- read.csv("data/mainUrineAnt.csv")[-1]
plasmaAnt <- read.csv("data/mainPlasmaAnt.csv")[-1]
plasmaFlav <- read.csv("data/mainPlasmaFlav.csv")[-1]

# 
anthro <- c("Peso", "IMC", "Grasa", "IRCV", 
            "Bpmin", "Bpmax", "Frec")

# flavplot ----

flavplot <- orinaFlav %>% select(-c(anthro, Sweetener, Sex, numVol, Time)) %>% select(HE.G, NS, NG)

model_clustering_OF <- Mclust(flavplot)

p1 <- fviz_mclust(object = model_clustering_OF, what = "BIC", pallete = "jco",  
                  title = "Model Selection") + scale_x_discrete(limits = c(1:10))
p2 <- fviz_mclust(model_clustering_OF, what = "classification", geom = "point",
                  title = "Clusters Plot Orina Flav", pallete = "jco")
ggarrange(p1,p2)

plot(p1)

flavplot_T0 <- orinaFlav %>% filter(Time == "Initial") %>% 
  select(-c(anthro, Sweetener, Sex, numVol, Time)) %>% select(HE.G, NS, NG)

flavplot_TF <- orinaFlav %>% filter(Time == "Final") %>% 
  select(-c(anthro, Sweetener, Sex, numVol, Time)) %>% select(HE.G, NS, NG)

modelClusteringFlav_T0 <- Mclust(flavplot_T0)

p1 <- fviz_mclust(object = modelClusteringFlav_T0, what = "BIC", pallete = "jco",  
                  title = "Model Selection Orina Flav Initial Time") + scale_x_discrete(limits = c(1:10))
p2_0 <- fviz_mclust(modelClusteringFlav_T0, what = "classification", geom = "point",
                  title = "A. Flavanones Initial Time", pallete = "jco")
ggarrange(p1,p2)

modelClusteringFlav_TF <- Mclust(flavplot_TF, G = 4)

p1 <- fviz_mclust(object = modelClusteringFlav_TF, what = "BIC", pallete = "jco",  
                  title = "Model Selection Orina Flav Final Time") + scale_x_discrete(limits = c(1:10))
p2_F <- fviz_mclust(modelClusteringFlav_TF, what = "classification", geom = "point",
                  title = "B. Flavanones Final Time", pallete = "jco")
ggarrange(p2_0,p2_F)


# antplot ----

comparacionPA_T0 <- clValid(
  obj        = plasmaAnt %>% filter(Time == "Initial") %>% select(-c(anthro, Sweetener, Sex, numVol, Time)) %>% 
                                                                    select(CA, VA.GG, DHPAA, DHPAA.G)
,
  nClust     = 2:6,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionPA_T0) 


comparacionPA_TF <- clValid(
  obj        = plasmaAnt %>% filter(Time == "Final") %>% select(-c(anthro, Sweetener, Sex, numVol, Time)) %>% 
    select(CA, VA.GG, DHPAA, DHPAA.G)
  ,
  nClust     = 2:6,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparacionPA_TF) 

### model-based ----

antplot <- plasmaAnt %>% select(-c(anthro, Sweetener, Sex, numVol, Time)) %>% select(CA, VA.GG, DHPAA, DHPAA.G)

modelClustering <- Mclust(antplot)

p1 <- fviz_mclust(object = modelClustering, what = "BIC", pallete = "jco",  
                  title = "Model Selection Plasma Ant ") + scale_x_discrete(limits = c(1:10))
p2 <- fviz_mclust(modelClustering, what = "classification", geom = "point",
                  title = "Clusters Plot Plasma Ant ", pallete = "jco")
ggarrange(p1,p2)


antplot_T0 <- plasmaAnt %>% filter(Time == "Initial") %>%
  select(-c(anthro, Sweetener, Sex, numVol, Time)) %>% select(CA, VA.GG, DHPAA, DHPAA.G)

antplot_TF <- plasmaAnt %>% filter(Time == "Final") %>%
  select(-c(anthro, Sweetener, Sex, numVol, Time)) %>% select(CA, VA.GG, DHPAA, DHPAA.G)

modelClusteringAnt_T0 <- Mclust(antplot_T0, G = 5)

p1 <- fviz_mclust(object = modelClusteringAnt_T0, what = "BIC", pallete = "jco",  
                  title = "Model Selection Plasma Ant Initial Time") + scale_x_discrete(limits = c(1:10))
p2_0 <- fviz_mclust(modelClusteringAnt_T0, what = "classification", geom = "point",
                  title = "A.Anthocyanins Initial Time", pallete = "jco")
plot(p1)

ggarrange(p1,p2)


modelClusteringAnt_TF <- Mclust(antplot_TF, G = 4)

p1 <- fviz_mclust(object = modelClusteringAnt_TF, what = "BIC", pallete = "jco",  
                  title = "Model Selection Plasma Ant Final Time") + scale_x_discrete(limits = c(1:10))
p2_F <- fviz_mclust(modelClusteringAnt_TF, what = "classification", geom = "point",
                  title = "B. Anthocyanins Final Time", pallete = "jco")
ggarrange(p2_0,p2_F)




modelClusteringAnt_T0$classification
modelClusteringAnt_TF$classification

modelClusteringFlav_T0$classification
modelClusteringFlav_TF$classification

# plots ----

# Ant_T0

tabla_clustersAnt_T0 <-  plasmaAnt %>% filter(Time == "Initial") %>%
                         select(-c(anthro, numVol, Time)) %>% 
                         select(CA, VA.GG, DHPAA, DHPAA.G, Sweetener, Sex) %>%
                         add_column(clusters = modelClusteringAnt_T0$classification)

tableSexo_T0 <- table(tabla_clustersAnt_T0$Sex, tabla_clustersAnt_T0$clusters)#tabla_clusters %>% count(Sexo, clusters)  

tabla_clustersAnt_T0 <- tabla_clustersAnt_T0 %>% select(-c(Sweetener, Sex))

longtableAnt_T0 <- melt(tabla_clustersAnt_T0, id = c("clusters"))

p1 <- ggplot(longtableAnt_T0, aes(factor(variable, level = unique(longtableAnt_T0$variable)),as.numeric(value), 
                            fill=factor(clusters))) +
  geom_boxplot()+
  annotation_custom(grob = tableGrob(tableSexo_T0, rows = c("M", "W"), theme = ttheme_default(base_size = 8)), 
                    xmin= 4,xmax=5.5, ymin=0.75, ymax=1.25)+
 
  ggtitle("A. Anthocyanins at Initial Time")+
  labs(y = "standarized value",x = "variables", fill = "Cluster")

# Ant_TF

tabla_clustersAnt_TF <-  plasmaAnt %>% filter(Time == "Final") %>%
  select(-c(anthro, numVol, Time)) %>% 
  select(CA, VA.GG, DHPAA, DHPAA.G, Sweetener, Sex) %>%
  add_column(clusters = modelClusteringAnt_TF$classification)

tableSexo_TF <- table(tabla_clustersAnt_TF$Sex, tabla_clustersAnt_TF$clusters)#tabla_clusters %>% count(Sexo, clusters)  
tableEdulcorante_TF <- table(tabla_clustersAnt_TF$Sweetener, tabla_clustersAnt_TF$clusters) #tabla_clusters %>% count(Endulzante, clusters)

tabla_clustersAnt_TF <- tabla_clustersAnt_TF %>% select(-c(Sweetener, Sex))

longtableAnt_TF <- melt(tabla_clustersAnt_TF, id = c("clusters"))

p2 <- ggplot(longtableAnt_TF, aes(factor(variable, level = unique(longtableAnt_TF$variable)),as.numeric(value), 
                                  fill=factor(clusters))) +
  geom_boxplot()+
  annotation_custom(grob = tableGrob(tableSexo_TF, rows = c("M", "W"), theme = ttheme_default(base_size = 8)), 
                    xmin= 4,xmax=5.5, ymin=0.75, ymax=1.25)+
  annotation_custom(grob = tableGrob(tableEdulcorante_TF, rows=c("SA", "ST","SU"), theme = ttheme_default(base_size = 8)), 
                    xmin= 4,xmax=5.5, ymin=-0.25, ymax=0.25)+
  ggtitle("B. Anthocyanins at Final Time")+
  labs(y = "standarized value",  x = "variables", fill = "Cluster")


ggarrange(p1,p2)


# Flav_T0

tabla_clustersFlav_T0 <-  orinaFlav %>% filter(Time == "Initial") %>%
  select(-c(anthro, numVol, Time)) %>% 
  select(HE.G, NG, NS, Sweetener, Sex) %>%
  add_column(clusters = modelClusteringFlav_T0$classification)

tableSexo_T0 <- table(tabla_clustersFlav_T0$Sex, tabla_clustersFlav_T0$clusters)#tabla_clusters %>% count(Sexo, clusters)  

tabla_clustersFlav_T0 <- tabla_clustersFlav_T0 %>% select(-c(Sweetener, Sex))

longtableFlav_T0 <- melt(tabla_clustersFlav_T0, id = c("clusters"))

p1 <- ggplot(longtableFlav_T0, aes(factor(variable, level = unique(longtableFlav_T0$variable)),as.numeric(value), 
                                  fill=factor(clusters))) +
  geom_boxplot()+
  annotation_custom(grob = tableGrob(tableSexo_T0, rows = c("M", "W"), theme = ttheme_default(base_size = 8)), 
                    xmin= 4-1,xmax=5.5-1, ymin=0.75, ymax=1.25)+
  ggtitle("A. Flavanones at Initial Time")+
  labs(y = "standarized value", x = "variables", fill = "Cluster")

# Flav_TF

tabla_clustersFlav_TF <- orinaFlav %>% filter(Time == "Final") %>%
  select(-c(anthro, numVol, Time)) %>% 
  select(HE.G, NG, NS, Sweetener, Sex) %>%
  add_column(clusters = modelClusteringFlav_TF$classification)

tableSexo_TF <- table(tabla_clustersFlav_TF$Sex, tabla_clustersFlav_TF$clusters)#tabla_clusters %>% count(Sexo, clusters)  
tableEdulcorFlave_TF <- table(tabla_clustersFlav_TF$Sweetener, tabla_clustersFlav_TF$clusters) #tabla_clusters %>% count(EndulzFlave, clusters)

tabla_clustersFlav_TF <- tabla_clustersFlav_TF %>% select(-c(Sweetener, Sex))

longtableFlav_TF <- melt(tabla_clustersFlav_TF, id = c("clusters"))

p2 <- ggplot(longtableFlav_TF, aes(factor(variable, level = unique(longtableFlav_TF$variable)),as.numeric(value), 
                                  fill=factor(clusters))) +
  geom_boxplot()+
  annotation_custom(grob = tableGrob(tableSexo_TF, rows = c("M", "W"), theme = ttheme_default(base_size = 8)), 
                    xmin= 3,xmax=4.5, ymin=0.75, ymax=1.25)+
  annotation_custom(grob = tableGrob(tableEdulcorFlave_TF, rows=c("SA", "ST","SU"), theme = ttheme_default(base_size = 8)), 
                    xmin= 3,xmax=4.5, ymin=-0.25, ymax=0.25)+
  ggtitle("B. Flavanones at Final Time")+
  labs(y = "standarized value", x = "variables", fill = "Cluster")


ggarrange(p1,p2)

