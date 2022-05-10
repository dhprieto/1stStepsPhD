source("scripts/preprocess.R")
source("scripts/boxplots.R")

orinaFlav <- read.csv("data/mainUrineFlav.csv")[-1]
orinaAnt <- read.csv("data/mainUrineAnt.csv")[-1]
plasmaAnt <- read.csv("data/mainPlasmaAnt.csv")[-1]
plasmaFlav <- read.csv("data/mainPlasmaFlav.csv")[-1]

plasmaFlav$Sex[plasmaFlav$Sex == "HOMBRE"] <- "Man"

plasmaFlav$Sex[plasmaFlav$Sex == "MUJER"] <- "Woman"

write.csv(orinaFlav, "data/mainUrineFlav.csv")

anthro <- c("Peso", "IMC", "Grasa", "IRCV", 
            "Bpmin", "Bpmax", "Frec")

tableAnth <- orinaFlav %>% select (all_of(anthro), Time, Sex, Sweetener)
tableMet <- orinaFlav %>% select (-anthro, -numVol, Time, Sex, Sweetener)

tableMetMelt <- melt(plasmaAnt %>% select (DHPAA.G, VA.GG, Time, Sex, Sweetener), 
                     id = c("Time", "Sex", "Sweetener"))

bxp(tableMetMelt, "Sweetener")
bxp <- function(longTable, factore){
  
  if (factore == "Time") {
    
    ggplot(longTable, aes(factor(variable, 
                                 level = unique(longTable$variable)),as.numeric(value), 
                          fill=factor(longTable[,factore], 
                                      levels = c("Initial", "Final")))) +
      geom_boxplot()+
      ggtitle(paste("A.Time Flavanones-Plasma"))+
      labs(y = "standarized value", x = "variables", fill = "Time")
    
  }
  
  else if (factore == "Sex") {
    ggplot(longTable, aes(factor(variable, 
                                 level = unique(longTable$variable)),as.numeric(value), 
                          fill=factor(longTable[,factore]))) +
      geom_boxplot()+
      ggtitle(paste("B. Sex:Time Flavanones-Plasma"))+
      labs(y = "standarized value", x = "variables", fill = "Sex")+
      scale_fill_brewer(palette = "Dark2")+
      
      facet_wrap(~factor(Time, levels = c("Initial", "Final")))
    
    
  }
  
  else{
    ggplot(longTable, aes(factor(variable, 
                                 level = unique(longTable$variable)),as.numeric(value), 
                          fill=factor(longTable[,factore]))) +
      geom_boxplot()+
      ggtitle(paste("C. Sweetener:Time Flavanones-urine"))+
      labs(y = "standarized value", x = "variables", fill = "Sex")+
      facet_wrap(~factor(Time, levels = c("Initial", "Final")))
    
    
  }
}

print(bxp(tableMetMelt, "Sex"))
print(bxp(tableMetMelt, "Sweetener"))
print(bxp(tableMetMelt, "Time"))
print(bxp(tableAnthMelt, "Sex"))
print(bxp(tableAnthMelt, "Time"))
bxp(tableAnthMelt, "Sweetener")
}