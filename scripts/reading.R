# lectura y conversion base ----

reading <- function(tablaPath, nasPercentageCol, nasRow){
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

preprocessTables <- function(root, nombreTabla) {
  
  # Getting data ready
  rootTabla <- paste0(root, nombreTabla)
  
  tabla <- read.csv(rootTabla)
  
  
  # Make factors of categorical features
  
  tabla$Endulzante <- factor(tabla$Endulzante, levels = c("SA", "ST", "SU"))
  tabla$Sexo <- factor(tabla$Sexo, levels = c("HOMBRE", "MUJER"))
  tabla$Tiempo <- factor(tabla$Tiempo, levels = c("0", "Final"))
  tabla$numVol <- factor(tabla$numVol)
  
  for (i in seq(1:nrow(tabla))){
    if (tabla$Tiempo[i] == "0"){
      tabla$Peso[i] = tabla$Peso.inicial[i]
      tabla$IMC[i] = tabla$IMC.Inicial[i]
      tabla$Grasa[i] = tabla$Grasa.inicial[i]
      tabla$IRCV[i] = tabla$IRCV.inicial[i]
      tabla$Bpmin[i] = tabla$Bpmin.inicial[i]
      tabla$Bpmax[i] = tabla$Bpmax.inicial[i]
      tabla$Frec[i] = tabla$Frec.inicial[i]
      
    }
    
    else if (tabla$Tiempo[i] == "Final"){
      tabla$Peso[i] = tabla$Peso.final[i]
      tabla$IMC[i] = tabla$IMC.Final[i]
      tabla$Grasa[i] = tabla$Grasa.final[i]
      tabla$IRCV[i] = tabla$IRCV.Final[i]
      tabla$Bpmin[i] = tabla$Bpmin.final[i]
      tabla$Bpmax[i] = tabla$Bpmax.final[i]
      tabla$Frec[i] = tabla$Frec.final[i]
    }
    
    
  }
  
  # Removing of trivial redundant and useless features 
  
  set.A <- subset(tabla, select =-c(X.1, X, Peso.inicial, Peso.final, Delta.Peso, Talla, IMC.Inicial, IMC.Final, 
                                    Delta.IMC, Grasa.inicial, Grasa.final, Delta.Grasa, IRCV.Final, IRCV.inicial, 
                                    Bpmin.final, Bpmin.inicial, Bpmax.final, Bpmax.inicial, Frec.final, Frec.inicial))
  
  # Only numerical features
  
  set.A_num <- subset(set.A, select=-c(Endulzante, Sexo, Tiempo, numVol))
  
  #Rescaling, can use "set.A_rescaled <- scale(set.A_num)" too
  
  set.A_rescaled <- set.A_num %>% mutate_each_(list(~rescale(.) %>% as.vector), 
                                               vars = colnames(set.A_num))
  
  set.A_factors <- cbind(set.A_rescaled, Endulzante = set.A$Endulzante, 
                         Tiempo = set.A$Tiempo, Sexo = set.A$Sexo, numVol = set.A$numVol)
  
  tabla_Tiempo <- subset(set.A_factors, select=-c(Endulzante, Sexo))
  
  tabla_Tiempo0 <- subset(tabla_Tiempo, Tiempo == "0", select = -Tiempo)
  tabla_TiempoF <- subset(tabla_Tiempo, Tiempo == "Final", select = -Tiempo)
  
  tabla_Sexo <- subset(set.A_factors, select=-c(Endulzante, Tiempo))
  
  tabla_SexoM <- subset(tabla_Sexo, Sexo == "HOMBRE", select = -Sexo)
  tabla_SexoF <- subset(tabla_Sexo, Sexo == "MUJER", select = -Sexo)
  
  tabla_Endulzante <- subset(set.A_factors, select=-c(Sexo, Tiempo))
  
  tabla_EndulzanteSA <- subset(tabla_Endulzante, Endulzante == "SA", select = -Endulzante)
  tabla_EndulzanteSU <- subset(tabla_Endulzante, Endulzante == "SU", select = -Endulzante)
  tabla_EndulzanteST <- subset(tabla_Endulzante, Endulzante == "ST", select = -Endulzante)
  
  tabla_0xM <- subset(set.A_factors, Tiempo == "0" & Sexo == "HOMBRE", select =-c(Tiempo, Sexo, Endulzante))
  tabla_0xF <- subset(set.A_factors, Tiempo == "0" & Sexo == "MUJER", select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxM <- subset(set.A_factors, Tiempo == "Final" & Sexo == "HOMBRE", select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxF <- subset(set.A_factors, Tiempo == "Final" & Sexo == "MUJER", select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_MxSAx0 <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="SA" & Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_MxSAxF <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="SA" & Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_FxSAx0 <- subset(set.A_factors, Sexo == "MUJER" & Endulzante =="SA"& Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxSAxF <- subset(set.A_factors, Sexo == "MUJER" & Endulzante =="SA"& Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_MxSTx0 <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="ST" & Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_MxSTxF <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="ST"& Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_FxSTx0 <- subset(set.A_factors, Sexo == "MUJER" & Endulzante =="ST" & Tiempo == "0",
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxSTxF <- subset(tabla, Sexo == "MUJER" & Endulzante =="ST" & Tiempo == "Final",
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_MxSUx0 <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="SU" & Tiempo == "0", 
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_MxSUxF <- subset(set.A_factors, Sexo == "HOMBRE" & Endulzante =="SU" & Tiempo == "Final", 
                         select =-c(Tiempo, Sexo, Endulzante))
  
  tabla_FxSUx0 <- subset(set.A_factors, Sexo == "MUJER" & Endulzante =="SU" & Tiempo == "0",
                         select =-c(Tiempo, Sexo, Endulzante))
  tabla_FxSUxF <- subset(set.A_factors, Sexo == "MUJER" & Endulzante =="SU" & Tiempo == "Final",
                         select =-c(Tiempo, Sexo, Endulzante))
  
  return(list(tablaSinEsc = set.A,
              tablaFactors = set.A_factors, tablaNum = set.A_rescaled,
              tabla_Tiempo = tabla_Tiempo, tabla_Tiempo0 = tabla_Tiempo0,
              tabla_TiempoF = tabla_TiempoF,  tabla_Sexo=tabla_Sexo,
              tabla_SexoM=tabla_SexoM, tabla_SexoF=tabla_SexoF,
              tabla_Endulzante=tabla_Endulzante, 
              tabla_EndulzanteSA=tabla_EndulzanteSA,
              tabla_EndulzanteSU=tabla_EndulzanteSU, 
              tabla_EndulzanteST=tabla_EndulzanteST,
              tabla_0xM=tabla_0xM,tabla_0xF=tabla_0xF, 
              tabla_FxM=tabla_FxM, tabla_FxF=tabla_FxF,
              tabla_MxSAx0=tabla_MxSAx0, tabla_MxSAxF=tabla_MxSAxF,
              tabla_FxSAx0=tabla_FxSAx0, tabla_FxSAxF=tabla_FxSAxF, 
              tabla_MxSTx0=tabla_MxSTx0, tabla_MxSTxF=tabla_MxSTxF,
              tabla_FxSTx0=tabla_FxSTx0, tabla_FxSTxF=tabla_FxSTxF, 
              tabla_MxSUx0=tabla_MxSUx0, tabla_MxSUxF=tabla_MxSUxF, 
              tabla_FxSUx0=tabla_FxSUx0, tabla_FxSUxF=tabla_FxSUxF))
  
}

