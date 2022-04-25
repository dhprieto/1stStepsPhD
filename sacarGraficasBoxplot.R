source("scripts/preprocess.R")
source("scripts/boxplots.R")

orinaFlav <- removeOutliers(preprocessTables("data/", "tablaOrinaFlav.csv")$tablaFactors)
orinaAnt <- removeOutliers(preprocessTables("data/", "tablaorinaAnt.csv")$tablaFactors)
plasmaAnt <- removeOutliers(preprocessTables("data/", "tablaplasmaAnt.csv")$tablaFactors)
plasmaFlav <- removeOutliers(preprocessTables("data/", "tablaplasmaFlav_adjusted.csv")$tablaFactors)


anthro <- c("Peso", "IMC", "Grasa", "IRCV", 
            "Bpmin", "Bpmax", "Frec")


tableAnth <- tabla %>% select (all_of(anthro), Tiempo, Sexo, Endulzante)
tableMet <- tabla %>% select (-anthro, -numVol, Tiempo, Sexo, Endulzante)


tableMetMelt <- melt(orinaFlav %>% select (ES, HE.G, NG, Tiempo, Sexo, Endulzante), 
                     id = c("Tiempo", "Sexo", "Endulzante"))

bxp(tableMetMelt, "Endulzante")
bxp <- function(longTable, factore){
  
  if (factore == "Tiempo") {
    
    ggplot(longTable, aes(factor(variable, 
                                 level = unique(longTable$variable)),as.numeric(value), 
                          fill=factor(longTable[,factore]))) +
      geom_boxplot()+
      ggtitle(paste("Time Anthocyanin-Urine"))+
      labs(y = "standarized value", x = "variables", fill = "Time")
    
  }
  
  else{
    ggplot(longTable, aes(factor(variable, 
                                 level = unique(longTable$variable)),as.numeric(value), 
                          fill=factor(longTable[,factore]))) +
      geom_boxplot()+
      ggtitle(paste("Sweetener:Time Flavanones-urine"))+
      labs(y = "standarized value", x = "variables", fill = "Sweetener")+
      scale_colour_discrete("Sweetener")+
      facet_wrap(~Tiempo)
    
    
  }
}

print(bxp(tableMetMelt, "Sexo"))
print(bxp(tableMetMelt, "Endulzante"))
print(bxp(tableMetMelt, "Tiempo"))
print(bxp(tableAnthMelt, "Sexo"))
print(bxp(tableAnthMelt, "Tiempo"))
bxp(tableAnthMelt, "Endulzante")
}