source("scripts/preprocess.R")

orinaFlav <- removeOutliers(preprocessTables("data/", "tablaOrinaFlav.csv")$tablaFactors)
orinaAnt <- removeOutliers(preprocessTables("data/", "tablaorinaAnt.csv")$tablaFactors)
plasmaAnt <- removeOutliers(preprocessTables("data/", "tablaplasmaAnt.csv")$tablaFactors)
plasmaFlav <- removeOutliers(preprocessTables("data/", "tablaplasmaFlav_adjusted.csv")$tablaFactors)

library(mclust)

pruebauwu <- Mclust(orinaFlav %>% select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec,
              Endulzante, Sexo, numVol, Tiempo)))

media <- pruebauwu$parameters$mean
variance <- pruebauwu$parameters$variance$sigma

prueba1 <- orinaFlav %>% filter(Tiempo == "0") %>% 
  select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec,
            Endulzante, Sexo, numVol, Tiempo))
prueba2 <- orinaFlav %>% filter(Tiempo == "Final") %>% 
  select(-c(Peso, IMC, Grasa, IRCV, Bpmin, Bpmax, Frec,
            Endulzante, Sexo, numVol, Tiempo))

IP.dis <- diss(x = prueba1[,1], 
               y = prueba2[,1],"ACF")


IP.hclus <- cutree(hclust(IP.dis), k = 5)
cluster.evaluation(pruebauwu$classification, IP.hclus)
