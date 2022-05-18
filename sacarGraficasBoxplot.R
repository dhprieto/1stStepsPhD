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

tableMetMelt <- melt(plasmaAnt %>% select (CA, VA.GG, DHPAA, DHPAA.G, Time, Sex, Sweetener), 
                     id = c("Time", "Sex", "Sweetener"))

bxp(tableMetMelt, "Sex")


tableMetMelt <- melt(orinaFlav %>% select (HE.G, NG, NS, Time, Sex, Sweetener), 
                     id = c("Time", "Sex", "Sweetener"))


ggplot(tableMetMelt, aes(factor(variable, 
                             level = unique(tableMetMelt$variable)),as.numeric(value), 
                      fill=factor(Time, levels = c("Initial", "Final")))) +
  geom_boxplot()+
  ggtitle(paste("B. Sex:Time Anthocyanins"))+
  labs(y = "standarized value", x = "Bioactive Anthocyanins", fill = "Time")+
  
  facet_wrap(~factor(Sex))





ggplot(tableMetMelt, aes(factor(variable, 
                                level = unique(tableMetMelt$variable)),as.numeric(value), 
                         fill=factor(Time, levels = c("Initial", "Final")))) +
  geom_boxplot()+
  ggtitle(paste("A. Sex:Time Flavanones"))+
  labs(y = "standarized value", x = "Bioactive Flavanones", fill = "Time")+
  
  facet_wrap(~factor(Sex))





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
        
      facet_wrap(~factor(Time, levels = c("Initial", "Final")))
    
    
  }
  
  else{
    ggplot(longTable, aes(factor(variable, 
                                 level = unique(longTable$variable)),as.numeric(value), 
                          fill=factor(longTable[,factore])), colour = "Sweetener") +
      geom_boxplot()+
      ggtitle(paste("C. Sweetener:Time Flavanones-urine"))+
      labs(y = "standarized value", x = "bioactive", fill = "Sex")+
      facet_wrap(~factor(Time, levels = c("Initial", "Final")))
    
    
  }
}

print(bxp(tableMetMelt, "Sex"))
print(bxp(tableMetMelt, "Sweetener"))
print(bxp(tableMetMelt, "Time"))
print(bxp(tableAnthMelt, "Sex"))
print(bxp(tableAnthMelt, "Time"))
bxp(tableAnthMelt, "Sweetener")


# ----

orinaFlav <- read.csv("data/mainUrineFlav.csv")[-1]
orinaAnt <- read.csv("data/mainUrineAnt.csv")[-1]
plasmaAnt <- read.csv("data/mainPlasmaAnt.csv")[-1]
plasmaFlav <- read.csv("data/mainPlasmaFlav.csv")[-1]




HEGMan <- orinaFlav %>% filter(Sex == "Man") %>% select(HE.G) %>% summarise(mean(HE.G))

HEGWoman <- orinaFlav %>% filter(Sex == "Woman") %>% select(HE.G)

CAMan <- plasmaAnt %>% filter(Sex == "Man")  %>% select(CA) %>% summarise(mean(CA))
VAGGMan <- plasmaAnt %>% filter(Sex == "Man")  %>% select(VA.GG) %>% summarise(mean(VA.GG))
DHPAAMan <- plasmaAnt %>% filter(Sex == "Man")  %>% select(DHPAA) %>% summarise(mean(DHPAA))
DHPAAGMan <- plasmaAnt %>% filter(Sex == "Man")  %>% select(DHPAA.G) %>% summarise(mean(DHPAA.G))

NGWoman <- orinaFlav %>% filter(Sex == "Woman") %>% select(NG)

NSMan <- orinaFlav %>% filter(Sex == "Man") %>% select(NS) %>% summarise(mean(NS))

man_data <- melt(data.frame(HEGMan, CAMan, VAGGMan, DHPAAMan, DHPAAGMan, NSMan))

ggplot(man_data, aes(x = factor(variable), y = value)) + geom_bar(stat = "identity")



flavPlot <- orinaFlav %>% select(HE.G, NG, NS, Sex)

melt(flavPlot)

ggplot(melt(flavPlot), aes(x= variable, y = value, ))+ geom_bar(stat = "identity")


       