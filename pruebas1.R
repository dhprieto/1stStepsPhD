library(tidyverse)
library(mclust)
library(factoextra)
library(ggpubr)
library(scales)

tabla1 <- read.csv("data/tablaOrinaAnt.csv")

preprocessTables1 <- function(root, nombreTabla) {
  
  # Getting data ready
  rootTabla <- paste0(root, nombreTabla)
  
  tabla <- read.csv(rootTabla)
  
  
  # Make factors of categorical features
  
  tabla$Endulzante <- factor(tabla$Endulzante, levels = c("SA", "ST", "SU"))
  tabla$Sexo <- factor(tabla$Sexo, levels = c("HOMBRE", "MUJER"))
  tabla$Tiempo <- factor(tabla$Tiempo, levels = c("0", "Final"))
  tabla$numVol <- factor(tabla$numVol)
  
  for (i in seq(1:nrow(tabla))){
    if (tabla$Tiempo[i] == "0"){
      tabla$Peso[i] = tabla$Peso.inicial[i]
      tabla$IMC[i] = tabla$IMC.Inicial[i]
      tabla$Grasa[i] = tabla$Grasa.inicial[i]
      tabla$IRCV[i] = tabla$IRCV.inicial[i]
      tabla$Bpmin[i] = tabla$Bpmin.inicial[i]
      tabla$Bpmax[i] = tabla$Bpmax.inicial[i]
      tabla$Frec[i] = tabla$Frec.inicial[i]
      
    }
    
    else if (tabla$Tiempo[i] == "Final"){
      tabla$Peso[i] = tabla$Peso.final[i]
      tabla$IMC[i] = tabla$IMC.Final[i]
      tabla$Grasa[i] = tabla$Grasa.final[i]
      tabla$IRCV[i] = tabla$IRCV.Final[i]
      tabla$Bpmin[i] = tabla$Bpmin.final[i]
      tabla$Bpmax[i] = tabla$Bpmax.final[i]
      tabla$Frec[i] = tabla$Frec.final[i]
    }
    
    
  }
  
  # Removing of trivial redundant and useless features 
  
  set.A <- subset(tabla, select =-c(X.1, X, Peso.inicial, Peso.final, Delta.Peso, Talla, IMC.Inicial, IMC.Final, 
                                    Delta.IMC, Grasa.inicial, Grasa.final, Delta.Grasa, IRCV.Final, IRCV.inicial, 
                                    Bpmin.final, Bpmin.inicial, Bpmax.final, Bpmax.inicial, Frec.final, Frec.inicial))
  
  # Only numerical features
  
  #set.A_num <- subset(set.A, select=-c(Endulzante, Sexo, Tiempo, numVol))
  
}

listaTablas1 <- preprocessTables1(root = "data/", nombreTabla = "tablaOrinaAnt.csv")

write.csv(x = listaTablas1, "data/OrinaAntLimpio.csv")

scale(listaTablas1)

hist(listaTablas1$CA.Gluc)

hist(rescale(listaTablas1$Peso))

theme_set(theme_classic())

# Plot

g <- ggplot(listaTablas1, aes(CA.Gluc))
g + geom_bar(aes(), width=0.5) 

table(listaTablas1$CA.Gluc)


datosuwu <- data.frame(uno = listaTablas1$CA.Gluc, dos = listaTablas1$DHPAA.Gluc)

hist(datosuwu$uno)
hist(re_norm(datosuwu, "uno")$uno)
uwu <- re_norm(datosuwu, "uno")

str(datosuwu2)

datosuwu2 <- as.data.frame(apply(datosuwu, MARGIN = 2, function(x) {x[!x %in% boxplot.stats(x)$out]}))

re_norm <- function(datos, var){
  
  Q <- quantile(datos[,var], probs=c(.25, .75), na.rm = FALSE)
  
  iqr <- IQR(datos[,var])
  
  valIQR <- 1.5

    
  datosNorm <- subset(datos, datos[,var] > (Q[1] - valIQR*iqr) & 
                          datos[,var] < (Q[2] + valIQR*iqr))
    
    # resultado <- ks.test(datosNorm[, var], "pnorm",
    #                      mean(datosNorm[, var]),
    #                      sd(datosNorm[, var]))
    # 

  
}



clusterNPlot1(as.data.frame(scale(listaTablas1)))

clusterNPlot1 <- function(listaTablas){
  
  tablaNumMet <- listaTablas %>%
    dplyr::select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec))
  
  tablaFactorsAll <-listaTablas$tablaFactors
  
  model_clustering_OF <- Mclust(tablaNumMet)
  
  p1 <- fviz_mclust(object = model_clustering_OF, what = "BIC", pallete = "jco",  
                    title = "Model Selection Orina Flav") + scale_x_discrete(limits = c(1:10))
  
  p2 <- fviz_mclust(model_clustering_OF, what = "classification", geom = "point",
                    title = "Clusters Plot Orina Flav", pallete = "jco")
  
  ggarrange(p1,p2)
  
  
  tabla_clusters <- tablaNumMet %>% tibble::add_column(Peso = listaTablas$tablaNum$Peso, 
                                                       IMC = listaTablas$tablaNum$IMC, 
                                                       Grasa = listaTablas$tablaNum$Grasa, 
                                                       IRCV = listaTablas$tablaNum$IRCV, 
                                                       Bpmin = listaTablas$tablaNum$Bpmin, 
                                                       Bpmax = listaTablas$tablaNum$Bpmax, 
                                                       Frec = listaTablas$tablaNum$Frec,
                                                       clusters = model_clustering_OF$classification,
                                                       Endulzante = rescale(as.numeric(tablaFactorsAll$Endulzante)), 
                                                     Sexo = rescale(as.numeric(tablaFactorsAll$Sexo)),
                                                       Tiempo = tablaFactorsAll$Tiempo) %>%
    select(everything(),Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec, Endulzante, Sexo, Tiempo, clusters)
  
  
  
  tableSexo <- table(tabla_clusters$Sexo, tabla_clusters$clusters)#tabla_clusters %>% count(Sexo, clusters)  
  tableEdulcorante <- table(tabla_clusters$Endulzante, tabla_clusters$clusters) #tabla_clusters %>% count(Endulzante, clusters)
  
  tabla_clusters$Endulzante <- rescale(as.numeric(tabla_clusters$Endulzante))
  tabla_clusters$Sexo <- rescale(as.numeric(tabla_clusters$Sexo))
  
  longtableOF <- melt(tabla_clusters, id = c("clusters", "Tiempo"))
  
  longtableOF <- tabla_clusters %>% gather(variable, values, -clusters, -Tiempo, )
  
  ggplot(longtableOF, aes(factor(variable, level = unique(longtableOF$variable)),as.numeric(values), fill=factor(clusters))) +
    geom_boxplot()+
    annotate("text", x = which(unique(longtableOF$variable)=="Sexo"), y = 1.03, label = "Mujer") + 
    annotate("text",x = which(unique(longtableOF$variable)=="Sexo"), y = -0.03, label = "Hombre") +
    annotate("text", x = which(unique(longtableOF$variable)=="Endulzante"), y = 1.03, label = "SU") + 
    annotate("text",x = which(unique(longtableOF$variable)=="Endulzante"), y = -0.03, label = "SA")+
    annotation_custom(grob = tableGrob(tableSexo, rows = c("H", "M"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0.75, ymax=1)+
    annotation_custom(grob = tableGrob(tableEdulcorante, rows=c("SA", "ST","SU"), theme = ttheme_default(base_size = 8)), xmin= 11,xmax=13, ymin=0, ymax=0.25)+
    ggtitle(paste("Boxplot Cluster Analysis ", deparse(substitute(listaTablas))))+
    labs(y = "standarized value", x = "variables/clusters")+
    facet_wrap(~Tiempo)
  
}

