# Preprocesamiento base ----

preprocess <- function(tablaPath, nasPercentageCol, nasRow){
  tabla <- read.csv(tablaPath, sep = ";", dec = ",")
  for (i in seq(1, nrow(tabla))){
    
    if (grepl(pattern = "A", x = tabla$X[i])){
      tabla$Endulzante[i] <- "ST"
    }
    else if (grepl(pattern = "B", x = tabla$X[i])){
      tabla$Endulzante[i] <- "SU"
    }
    else if (grepl(pattern = "C", x = tabla$X[i])){
      tabla$Endulzante[i] <- "SA"
    }
  }
  
  
  if (grepl("agudo", tablaPath)){agudo = T}
  else {agudo = F}
  
  if(agudo){
    
    for (i in seq(1, nrow(tabla))){
      if (grepl(pattern = "([A-C]) -1", x = tabla$X[i])){
        tabla$Tiempo[i] <- "-1"
        }      
      else if (grepl(pattern = "([A-C]) 3,5", x = tabla$X[i])){
          tabla$Tiempo[i] <- "3,5"
        }    
      else if (grepl(pattern = "([A-C]) 12", x = tabla$X[i])){
          tabla$Tiempo[i] <- "12"
        }
      else if (grepl(pattern = "([A-C]) 24", x = tabla$X[i])){
          tabla$Tiempo[i] <- "24"
        }  
      }
    for (i in seq(1, nrow(tabla))){
      tabla$numVol[i] <- as.integer(gsub("U.*", "", tabla[,1][i]))
      }
    
    }
  
  else {
    for (i in seq(1, nrow(tabla))){
      if (grepl(pattern = "F", x = tabla$X[i])){
        tabla$Tiempo[i] <- "Final"
      }
      if (grepl(pattern = "([A-C])0", x = tabla$X[i])){
        tabla$Tiempo[i] <- "0"
          
      }
    }
    for (i in seq(1, nrow(tabla))){
      tabla$numVol[i] <- as.numeric(gsub("[A-C].", "", tabla[,1][i]))
      if (tabla$Endulzante[i] == "SU"){
        tabla$numVol[i] = tabla$numVol[i] + 50
      }
      else if (tabla$Endulzante[i] == "SA"){
        tabla$numVol[i] = tabla$numVol[i] + 100
      }

    }
  }
    
  tabla <- tabla[,colSums(is.na(tabla))<(nrow(tabla)*nasPercentageCol)]

  addAntro <- function (pathToAntro, tabla) {
    
    antro <- read.csv(pathToAntro, sep = ";", dec = ",")
    
    for (i in seq(1,nrow(antro))){
      if (antro$Nº.Volunt.[i] <= 46){
        antro$Endulzante[i] <- "ST"
        
      }
      if ((antro$Nº.Volunt.[i] >= 51) & (antro$Nº.Volunt.[i] <= 96)){
        antro$Endulzante[i] <- "SU"
        antro$Nº.Volunt.[i] <- antro$Nº.Volunt.[i]
        }
      if ((antro$Nº.Volunt.[i] >= 101) & (antro$Nº.Volunt.[i] <= 146)){
        antro$Endulzante[i] <- "SA"
        antro$Nº.Volunt.[i] <- antro$Nº.Volunt.[i]
      }  
    }
  
  tabla$numVol <- as.integer(tabla$numVol)    
  tabla <- merge(x= tabla, y= antro, by.x = c("numVol","Endulzante"), by.y= c("Nº.Volunt.", "Endulzante"), all=T)
  
  # Añadimos sexo
  
  sexVol <- read.csv("data/sexoVoluntarios.csv", sep = ";")
  
  tabla <- merge(x=tabla, y=sexVol, by.x="numVol", by.y="Voluntario", all=T)
  
  return(tabla)  
  }
  
  tabla <- addAntro("data/datosAntropometricosCardiovasculares.csv",tabla)        
  

  tabla <- tabla[order(tabla$Tiempo,tabla$numVol),]
  
  if (nasRow == T){
    tabla <- na.omit(tabla)
  }

  return(tabla)
    
}

tablaName2 = "cronicoPlasmaFlavLimpio.csv"

rootDir = "data/"

tablaPath = paste0(rootDir,tablaName2)

tabla1 <- preprocess (tablaPath, nasPercentageCol = 0.5, nasRow = T)

write.csv(tabla1, "data/tablaPlasmaFlav_adjusted.csv")
