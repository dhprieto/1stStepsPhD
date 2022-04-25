source("scripts/preprocess.R")
library(reshape2)

boxplotBias <- function(tabla){

# Selection of anthro featuers
  
anthro <- c("Peso", "IMC", "Grasa", "IRCV", 
               "Bpmin", "Bpmax", "Frec")

tableAnth <- tabla %>% select (all_of(anthro), Tiempo, Sexo, Endulzante)
tableMet <- tabla %>% select (-anthro, -numVol, Tiempo, Sexo, Endulzante)

# long table format

tableMetMelt <- melt(tableMet, id = c("Tiempo", "Sexo", "Endulzante"))
tableAnthMelt <- melt(tableAnth, id = c("Tiempo", "Sexo", "Endulzante"))

# plot anthro-sex

bxp <- function(longTable, factore){

  if (factore == "Tiempo") {
  
      ggplot(longTable, aes(factor(variable, 
                                 level = unique(longTable$variable)),as.numeric(value), 
                          fill=factor(longTable[,factore]))) +
      geom_boxplot()+
      ggtitle(paste("boxplot ", deparse(substitute(longtable))," ", factore))+
      labs(y = "standarized value", x = "variables", legend = factore)
    
  }
  
  else{
  ggplot(longTable, aes(factor(variable, 
                            level = unique(longTable$variable)),as.numeric(value), 
                            fill=factor(longTable[,factore]))) +
  geom_boxplot()+
  ggtitle(paste("boxplot ", deparse(substitute(longtable))," ", factore))+
  labs(y = "standarized value", x = "variables", legend = factore)+
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
