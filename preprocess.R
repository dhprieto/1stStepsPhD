library(data.table)
library(scales)

preprocess <- function(tablePath, nasPercentageCol, nasRow){
  table <- read.csv(tablePath, sep = ";", dec = ",")
  
  for (i in seq(1, nrow(table))){
    
    if (grepl(pattern = "A", x = table$X[i])){
      table$Endulzante[i] <- "ST"
    }
    else if (grepl(pattern = "B", x = table$X[i])){
      table$Endulzante[i] <- "SU"
    }
    else if (grepl(pattern = "C", x = table$X[i])){
      table$Endulzante[i] <- "SA"
    }
  }
  
  
  if (grepl("agudo", tablePath)){agudo = T}
  else {agudo = F}
  
  if(agudo){
    
    for (i in seq(1, nrow(table))){
      if (grepl(pattern = "([A-C]) -1", x = table$X[i])){
        table$Tiempo[i] <- "-1"
        }      
      else if (grepl(pattern = "([A-C]) 3,5", x = table$X[i])){
          table$Tiempo[i] <- "3,5"
        }    
      else if (grepl(pattern = "([A-C]) 12", x = table$X[i])){
          table$Tiempo[i] <- "12"
        }
      else if (grepl(pattern = "([A-C]) 24", x = table$X[i])){
          table$Tiempo[i] <- "24"
        }  
      }
    for (i in seq(1, nrow(table))){
      table$numVol[i] <- as.integer(gsub("U.*", "", table[,1][i]))
      }
    
    }
  
  else {
    for (i in seq(1, nrow(table))){
      if (grepl(pattern = "F", x = table$X[i])){
        table$Tiempo[i] <- "Final"
      }
      if (grepl(pattern = "([A-C])0", x = table$X[i])){
        table$Tiempo[i] <- "0"
          
      }
    }
    for (i in seq(1, nrow(table))){
      table$numVol[i] <- gsub("[A-C].", "", table[,1][i])
    }
  }
  table <- table[,colSums(is.na(table))<(nrow(table)*nasPercentageCol)]
  
  if (nasRow == T){
    table <- na.omit(table)
  }
  
  addAntro <- function (pathToAntro, table) {
    
    antro <- read.csv(pathToAntro, sep = ";", dec = ",")
    
    for (i in seq(1,nrow(antro))){
      if (antro$Nº.Volunt.[i] <= 46){
        antro$Endulzante[i] <- "ST"
        
      }
      if ((antro$Nº.Volunt.[i] >= 51) & (antro$Nº.Volunt.[i] <= 96)){
        antro$Endulzante[i] <- "SU"
        antro$Nº.Volunt.[i] <- antro$Nº.Volunt.[i]-50
      }
      if ((antro$Nº.Volunt.[i] >= 101) & (antro$Nº.Volunt.[i] <= 146)){
        antro$Endulzante[i] <- "SA"
        antro$Nº.Volunt.[i] <- antro$Nº.Volunt.[i]-100
      }  
    }
  
  table$numVol <- as.integer(table$numVol)    
  table <- merge(x= table, y= antro, by.x = c("numVol","Endulzante"), by.y= c("Nº.Volunt.", "Endulzante"), all=T)
  return(table)  
  }
  
  table <- addAntro("data/datosAntropometricosCardiovasculares.csv",table)        
  table <- table[order(table$Endulzante, table$Tiempo,table$numVol),]
  return(na.omit(table))
    
}

  
  
tableName2 = "cronicoPlasmaAntLimpio.csv"

rootDir = "data/"

tablePath = paste0(rootDir,tableName2)

cronicoOrinaAnt_Antro <- preprocess (tablePath, 0.05, T)

saveRDS(cronicoOrinaAnt_Antro, "data/cronicoPlasmaAnt_Antro.csv")



