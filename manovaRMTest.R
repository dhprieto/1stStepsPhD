## testing paired manova

#install.packages("MANOVA.RM")
library(tidyverse)
library(scales)
library(MANOVA.RM)
source("scripts/reading.R")

tabla1 <- preprocessTables("data/", "tablaOrinaAnt.csv")

tabla1_1 <- tabla1$tablaFactors

counts <- data.frame(table(tabla1_1$numVol))

tabla1_2 <- tabla1_1[tabla1_1$numVol %in% counts$Var1[counts$Freq > 1],]


formula(nombres) <- names(tabla1_1)[-c(10:16)]

as.name(nombres)

[1] "CA.Gluc"             "DHPAA.Gluc"          "TFA.Gluc"            "TFA.Sulfate"         "Ácido.Vanílico..VA."
[6] "Peso"                "IMC"                 "Grasa"               "IRCV"        

fit <- multRM(cbind(CA.Gluc, DHPAA.Gluc, TFA.Gluc, TFA.Sulfate ) ~ Sexo*Endulzante*Tiempo, data= tabla1_2, 
       subject = "numVol", within = "Tiempo", iter=1000)

summary(fit)
