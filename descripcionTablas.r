#----  Descripción datos

## Compuestos fenólicos



### Compuestos fenolicos SA VidaUtil Cronico

### Compuestos fenolicos ST VidaUtil Cronico

### Compuestos fenolicos SU VidaUtil Cronico

## Muestras bio

### Cronico Orina Atipicos

### Cronico plasma Atipicos


fenSU <- read.csv("data/Compuestos fenolicos SU VidaUtil Cronico_1.csv", sep = ";", dec = ",")
#---- Funcion de procesado de tablas
 
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

# ---- Lectura y procesado de las tablas

fenSU <- read.csv("data/Compuestos fenolicos SU VidaUtil Cronico_1.csv", sep = ";", dec = ",")
fenSA <- read.csv("data/Compuestos fenolicos SA VidaUtil Cronico_1.csv", sep = ";", dec = ",")
fenST <- read.csv("data/Compuestos fenolicos ST VidaUtil Cronico_1.csv", sep = ";", dec = ",")


fenSU_total <- procesado(fenSU, "SU")
fenSA_total <- procesado(fenSA, "SA")
fenST_total <- procesado(fenST, "ST")

fenFlavTotal <- rbind(fenSU_total, fenSA_total, fenST_total)

#----


shapiro.test(fenFlavTotal$mean)
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
anova <- aov(mean ~ Compuesto * Endulzante, data = fenFlavTotal)
summary(anova)
par(mfrow=c(1,2))
plot(anova, which=1:4)

