#----  Descripción datos

## Compuestos fenólicos



### Compuestos fenolicos SA VidaUtil Cronico

### Compuestos fenolicos ST VidaUtil Cronico

### Compuestos fenolicos SU VidaUtil Cronico

## Muestras bio

### Cronico Orina Atipicos

### Cronico plasma Atipicos


#---- Funcion de procesado de tablas
 

procesado2 <- function(tabla, endulzante){
  
  tabla_num <- tabla[-1]
  tabla_1 <- stack(as.data.frame(tabla_num))
  tabla_1$repeticion <- rep(c(1,2), nrow(tabla)/2)
  tabla_1$Condiciones <- rep(tabla$Condiciones,2)
  tabla_1$Endulzante <- rep(endulzante, nrow(tabla))
  
  
  dplyr::rename(tabla_1, mg.100.ml = values, Compuesto = ind)
  
  return(tabla_1)
}



# ---- Lectura y procesado de las tablas

fenSU <- read.csv("data/Compuestos fenolicos SU VidaUtil Cronico_1.csv", sep = ";", dec = ",")
fenSA <- read.csv("data/Compuestos fenolicos SA VidaUtil Cronico_1.csv", sep = ";", dec = ",")
fenST <- read.csv("data/Compuestos fenolicos ST VidaUtil Cronico_1.csv", sep = ";", dec = ",")


fenSU_total <- procesado2(fenSU, "SU")
fenSA_total <- procesado2(fenSA, "SA")
fenST_total <- procesado2(fenST, "ST")

fenFlavTotal <- rbind(fenSU_total, fenSA_total, fenST_total)

fenFlavTotal$Iluminacion <- rep(x = "Luz", nrow(fenFlavTotal))
fenFlavTotal$Dia<- rep(x = "0", nrow(fenFlavTotal))
fenFlavTotal$Temperatura <- rep(x = "25º", nrow(fenFlavTotal))

for (i in seq(1,nrow(fenFlavTotal))){
    
    if (grepl(pattern = "\\s5º", x = fenFlavTotal$Condiciones[i])){
       fenFlavTotal$Temperatura[i] <- "5º"
    
    }
    
    else if (grepl(pattern = "Osc", x = fenFlavTotal$Condiciones[i])){
      fenFlavTotal$Iluminacion[i] <- "Osc"  
    }
}

for (i in seq(1,nrow(fenFlavTotal))){
    if (grepl(pattern = "dia 15", x = fenFlavTotal$Condiciones[i])){
      fenFlavTotal$Dia[i] <- "15"  
      
    }

    else if (grepl(pattern = "dia 30", x = fenFlavTotal$Condiciones[i])){
      fenFlavTotal$Dia[i] <- "30"  
    
    }
  
    else if (grepl(pattern = "dia 45", x = fenFlavTotal$Condiciones[i])){
      fenFlavTotal$Dia[i] <- "45"  
    
    }
    
    else if (grepl(pattern = "dia 60", x = fenFlavTotal$Condiciones[i])){
      fenFlavTotal$Dia[i] <- "60"  
    
      
    }
    else if (grepl(pattern = "dia 90", x = fenFlavTotal$Condiciones[i])){
      fenFlavTotal$Dia[i] <- "90"  
    }
}


write.csv(fenFlavTotal, file= "data/fenFlavTotal.csv")

#----


shapiro.test(fenFlavTotal$values)
shapiro.test(scale(fenFlavTotal$mean, center = T, scale = T))


library("ggplot2")
library("gridExtra")

ggplot(data = as.data.frame(fenFlavTotal), aes(x = Endulzante, y = mean, color = Endulzante)) +
  geom_boxplot() +
  theme_bw()

p1 <- ggplot(data = as.data.frame(fenFlavTotal), aes(x = Endulzante, y = mean)) + 
  geom_boxplot() + theme_bw()
p2 <- ggplot(data = as.data.frame(fenFlavTotal), aes(x = Condiciones, y = mean)) +
  geom_boxplot() + theme_bw()
p3 <- ggplot(data = as.data.frame(fenFlavTotal), aes(x = Endulzante, y = mean, colour = Condiciones)) +
  geom_boxplot() + theme_bw()

grid.arrange(p1, p2, ncol = 2)
p3


var.test(x = fenFlavTotal[fenFlavTotal$Endulzante == "SA", "mean"],
         y = fenFlavTotal[fenFlavTotal$Endulzante == "SU", "mean"] )
    

#---- gráficos de interacción

library(ggplot2)

ggplot(data = fenFlavTotal, aes(x = Compuesto, y = mean, colour = Endulzante,
                         group = Endulzante)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (mg/100mlzumo)') +
  theme_bw()

anova <- aov(values ~ Iluminacion * Dia, data = fenFlavTotal)
summary(anova)
par(mfrow=c(1,2))
plot(anova, which=1:4)



# -----

### usar las dos réplicas
# empezar por la anova y luego ver las condiciones fijando factores diferentes en normalidad
# mirar MANOVA



#---- N-way anova


replications(yield~N*P*K,data=npk)


#---- funcion procesado antigua

procesado <- function(tabla, endulzante){
  
  tabla_num <- tabla[-1]
  
  # Se obtienen las medias de cada medida
  
  tabla_mean <- do.call(rbind, 
                        lapply(seq(1, nrow(tabla_num), 2), function(i){
                          x <- tabla_num[ i:(i + 1), , drop = FALSE]
                          res <- rbind(colSums(x)/2)
                          res
                        }))
  
  rownames(tabla_mean) <- unique(tabla$Condiciones)
  
  # La desviación estándar
  
  tabla_sd <- do.call(rbind, 
                      lapply(seq(1, nrow(tabla_num), 2), function(i){
                        x <- tabla_num[ i:(i + 1), , drop = FALSE]
                        res <- rbind(apply(x, 2, sd))
                        res
                      }))
  
  # Apilamos datos
  
  tabla_1 <- stack(as.data.frame(tabla_mean))
  tabla_1$Condiciones <- rep(unique(tabla$Condiciones),4)
  
  tabla_2 <- stack(as.data.frame(tabla_sd))
  tabla_2$Condiciones <- rep(unique(tabla$Condiciones),4)
  
  tabla_total <- merge(tabla_1, tabla_2, by = c("Condiciones", "ind"))
  
  tabla_total$Endulzante <- rep(endulzante, nrow(tabla_total))
  
  dplyr::rename(tabla_total, mean = values.x , SD = values.y, Compuesto = ind)
  
  
}
