source("scripts/preprocess.R")
source("scripts/boxplots.R")

orinaFlav <- read.csv("data/mainUrineFlav.csv")[-1]
orinaAnt <- read.csv("data/mainUrineAnt.csv")[-1]
plasmaAnt <- read.csv("data/mainPlasmaAnt.csv")[-1]
plasmaFlav <- read.csv("data/mainPlasmaFlav.csv")[-1]

anthro <- c("Peso", "IMC", "Grasa", "IRCV", 
            "Bpmin", "Bpmax", "Frec")

tableAnth <- orinaFlav %>% select (all_of(anthro), Time, Sex, Sweetener)
tableMet <- orinaFlav %>% select (-anthro, -numVol, Time, Sex, Sweetener)

tableMetMelt <- melt(plasmaFlav %>% select (ES, Time, Sex, Sweetener), 
                     id = c("Time", "Sex", "Sweetener"))

bxp(tableMetMelt, "Sweetener")
bxp <- function(longTable, factore){
  
  if (factore == "Time") {
    
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
      ggtitle(paste("Sweetener:Time Flavanoids-plasma"))+
      labs(y = "standarized value", x = "variables", fill = "Sweetener")+
      scale_colour_discrete("Sweetener")+
      facet_wrap(~Time)
    
    
  }
}

print(bxp(tableMetMelt, "Sex"))
print(bxp(tableMetMelt, "Sweetener"))
print(bxp(tableMetMelt, "Time"))
print(bxp(tableAnthMelt, "Sex"))
print(bxp(tableAnthMelt, "Time"))
bxp(tableAnthMelt, "Sweetener")
}