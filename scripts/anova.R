# script anovas ----

library(tidyverse)
library(rstatix)

# Función para realizar la anova de tres vías sobre una variable
# Imprime por pantalla el resultado

aov_test <- function(tabla, variable){
  
  tablaVar <- tabla %>% select(numVol, Endulzante, Sexo, Tiempo, variable)

  tablaVar <- tablaVar[!tablaVar[[5]] %in% boxplot.stats(tablaVar[[5]])$out,]
  
  res.aov <- anova_test(data = ungroup(tablaVar), dv=variable, wid=numVol, 
                        between = c(Sexo, Endulzante), within= Tiempo)
  
  tablaAnova <- get_anova_table(res.aov, correction = "auto")
  
  print(tablaAnova)
}

# Función para hacer en bucle el análisis anova a lo largo de una tabla

aov_loop <- function(tabla){
  
  message(paste("Tabla analizada: ", deparse(substitute(tabla))))
  for (i in seq(1,ncol(tabla))){

    if (is.numeric(tabla[[i]])){
    
      message(paste("Variable analizada: ", names(tabla)[i]))
      aov_test(tabla,
               names(tabla)[i])
    }
    
    
  }
}

# uwu