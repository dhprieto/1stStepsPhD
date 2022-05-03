source("scripts/preprocess.R")
library(mclust)
library(factoextra)
library(ggpubr)
library(reshape2)

# lectura

orinaFlav <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaOrinaFlav.csv")$tablaFactors))
orinaAnt <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaorinaAnt.csv")$tablaFactors))
plasmaAnt <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaplasmaAnt.csv")$tablaFactors))
plasmaFlav <- escaladoTablas(removeOutliers(preprocessTables("data/", "tablaplasmaFlav_adjusted.csv")$tablaFactors))

# rename

colnames(plasmaFlav) <- c("E", "ES",
                         anthro, "Sweetener", "Time",
                        "Sex", "numVol")

write.csv(plasmaFlav, "data/mainPlasmaFlav.csv")

anthro <- c("Peso", "IMC", "Grasa", "IRCV", 
            "Bpmin", "Bpmax", "Frec")

clusterNPlot <- function(listaTablas, anthropometric){
  
  tablaNumMet <- listaTablas  %>% 
    dplyr::select(-c(anthro, Sweetener, Sex, numVol, Time))
  
  model_clustering_OF <- Mclust(tablaNumMet)
  
  p1 <- fviz_mclust(object = model_clustering_OF, what = "BIC", pallete = "jco",  
                    title = "Model Selection Orina Flav") + scale_x_discrete(limits = c(1:10))
  
  p2 <- fviz_mclust(model_clustering_OF, what = "classification", geom = "point",
                    title = "Clusters Plot Orina Flav", pallete = "jco")
  
  ggarrange(p1,p2)
  
  if(anthropometric){
  
  tabla_clusters <- tablaNumMet %>% tibble::add_column(Peso = listaTablas$Peso, 
                                                       IMC = listaTablas$IMC, 
                                                       Grasa = listaTablas$Grasa, 
                                                       IRCV = listaTablas$IRCV, 
                                                       Bpmin = listaTablas$Bpmin, 
                                                       Bpmax = listaTablas$Bpmax, 
                                                       Frec = listaTablas$Frec,
                                                       clusters = model_clustering_OF$classification,
                                                       Sweetener = rescale(as.numeric(listaTablas$Sweetener)), 
                                                       Sex = rescale(as.numeric(listaTablas$Sex)),
                                                       Time = listaTablas$Time) %>%
    select(everything(),Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, Sweetener, Sex, Time, clusters)
  
  
  
  tableSex <- table(tabla_clusters$Sex, tabla_clusters$clusters)#tabla_clusters %>% count(Sex, clusters)  
  tableEdulcorante <- table(tabla_clusters$Sweetener, tabla_clusters$clusters) #tabla_clusters %>% count(Sweetener, clusters)
  
  tabla_clusters$Sweetener <- rescale(as.numeric(tabla_clusters$Sweetener))
  tabla_clusters$Sex <- rescale(as.numeric(tabla_clusters$Sex))
  
  longtableOF <- melt(tabla_clusters, id = c("clusters", "Time"))
  
  longtableOF <- tabla_clusters %>% gather(variable, values, -clusters, -Time, )
  
  ggplot(longtableOF, aes(factor(variable, level = unique(longtableOF$variable)),as.numeric(values), fill=factor(clusters))) +
    geom_boxplot()+
    annotate("text", x = which(unique(longtableOF$variable)=="Sex"), y = 1.03, label = "Women") + 
    annotate("text",x = which(unique(longtableOF$variable)=="Sex"), y = -0.03, label = "Men") +
    annotate("text", x = which(unique(longtableOF$variable)=="Sweetner"), y = 1.03, label = "SU") + 
    annotate("text",x = which(unique(longtableOF$variable)=="Sweetner"), y = -0.03, label = "SA")+
    annotation_custom(grob = tableGrob(tableSex, rows = c("M", "F"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0.75, ymax=1)+
    annotation_custom(grob = tableGrob(tableEdulcorante, rows=c("SA", "ST","SU"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0, ymax=0.25)+
    ggtitle(paste("Boxplot Cluster Analysis ", deparse(substitute(listaTablas))))+
    labs(y = "standarized value", x = "variables/clusters")+
    facet_wrap(~Time)
  }
  
  else {
    tabla_clusters <- tablaNumMet %>% tibble::add_column(clusters = model_clustering_OF$classification,
                                                         Sweetener = rescale(as.numeric(listaTablas$Sweetener)), 
                                                         Sex = rescale(as.numeric(listaTablas$Sex)),
                                                         Time = listaTablas$Time) %>%
      select(everything(), Sweetener, Sex, Time, clusters)
    
    
    
    tableSex <- table(tabla_clusters$Sex, tabla_clusters$clusters)#tabla_clusters %>% count(Sex, clusters)  
    tableEdulcorante <- table(tabla_clusters$Sweetener, tabla_clusters$clusters) #tabla_clusters %>% count(Sweetener, clusters)
    
    tabla_clusters$Sweetener <- rescale(as.numeric(tabla_clusters$Sweetener))
    tabla_clusters$Sex <- rescale(as.numeric(tabla_clusters$Sex))
    
    longtableOF <- melt(tabla_clusters, id = c("clusters", "Time"))
    
    longtableOF <- tabla_clusters %>% gather(variable, values, -clusters, -Time, )
    
    ggplot(longtableOF, aes(factor(variable, level = unique(longtableOF$variable)),as.numeric(values), fill=factor(clusters))) +
      geom_boxplot()+
      annotate("text", x = which(unique(longtableOF$variable)=="Sex"), y = 1.03, label = "Women") + 
      annotate("text",x = which(unique(longtableOF$variable)=="Sex"), y = -0.03, label = "Men") +
      annotate("text", x = which(unique(longtableOF$variable)=="Sweetner"), y = 1.03, label = "SU") + 
      annotate("text",x = which(unique(longtableOF$variable)=="Sweetner"), y = -0.03, label = "SA")+
      annotation_custom(grob = tableGrob(tableSex, rows = c("M", "F"), theme = ttheme_default(base_size = 8)), xmin= 5,xmax=7, ymin=0.75, ymax=1)+
      annotation_custom(grob = tableGrob(tableEdulcorante, rows=c("SA", "ST","SU"), theme = ttheme_default(base_size = 8)), xmin= 5,xmax=7, ymin=0, ymax=0.25)+
      ggtitle(paste("Boxplot Cluster Analysis ", deparse(substitute(listaTablas))))+
      labs(y = "standarized value", x = "variables/clusters")+
      facet_wrap(~Time)
  }
}

clusterNPlot(orinaAnt, anthropometric = F)

counts <- data.frame(table(orinaAnt$numVol))

orinaAntDupl <- orinaAnt[orinaAnt$numVol %in% counts$Var1[counts$Freq > 1],]


# prueba con media

orinaAntDuplArr <- orinaAntDupl %>% select(-anthro) %>% arrange(numVol)


orinaAntDupl$numVol[1] == orinaAntDupl$numVol[1+1]

rowMeans(data.frame(orinaAntDuplArr[i,],orinaAntDuplArr[i+1,])) 

while (i < 81){
for(i in seq(1,nrow(orinaAntDuplArr))){
  
  
  
  if (orinaAntDuplArr$numVol[i] == orinaAntDuplArr$numVol[i+1]){
   nuVector <- rowMeans(data.frame(as.numeric(orinaAntDuplArr[i,]), as.numeric(orinaAntDuplArr[i+1,])))
   print(nuVector)  
   }
  
}
}

install.packages("TSclust")

library(TSclust)

orinaAntDupl_0 <- orinaAntDupl %>% filter(Time == "0") %>% select((-c(Time, Sweetener, Sex,numVol)))
orinaAntDupl_F <- orinaAntDupl %>% filter(Time == "Final") %>% select(-c(Time, Sweetener, Sex, numVol))

diss.DTWARP(t(orinaAntDupl_0), t(orinaAntDupl_F))


library(dtwclust)

listOrinaAnt <- split(orinaAntDuplArr %>% select((-c(Time, Sweetener, Sex,numVol))), f = orinaAntDuplArr$numVol, drop = T)

tsclust(listOrinaAnt, k = 4)

