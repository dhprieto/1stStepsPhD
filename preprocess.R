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
  
  if(agudo == T){
    
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
        
      }
      if ((antro$Nº.Volunt.[i] >= 101) & (antro$Nº.Volunt.[i] <= 146)){
        antro$Endulzante[i] <- "SA"
        
      }  
    }
    
    antro_ST <- subset(antro, antro$Endulzante == "ST")
    antro_SU <- subset(antro, antro$Endulzante == "SU")
    antro_SA <- subset(antro, antro$Endulzante == "SA")
    
    
      if (agudo == T) {
        for (i in seq(1, nrow(table))){
          table[,1][i] <- gsub("U.*", "", table[,1][i])
          }
        } else {
        for (i in seq(1, nrow(table))){
          table[,1][i] <- gsub("[A-C].", "", table[,1][i])
        }
      }
    
    table_ST <- subset(table, table$Endulzante == "ST")
    table_SU <- subset(table, table$Endulzante == "SU")
    table_SA <- subset(table, table$Endulzante == "SA")
    
    antro_ST[,1] <- rescale(antro_ST[,1], to=c(min(as.integer(table_ST[,1])),max(as.integer(table_ST[,1]))))
    print(c(antro_ST[,1],table_ST[,1]))
  return(table)
  }
  
  
  table <- addAntro("data/datosAntropometricosCardiovasculares.csv",table)        
  return(table)
    
  
}
  
  

tableName1 = "agudoOrinaAntLimpio.csv"
tableName2 = "cronicoOrinaANtLimpio.csv"

rootDir = "data/"

tablePath = paste0(rootDir,tableName1)

agudoOrinaAnt <- preprocess (tablePath, 0.05, T)



View(agudoOrinaAnt)

round(6.9)
round(rescale(antro_ST[,1], to=c(min(as.integer(agudoOrinaAnt[,1])),max(as.integer(agudoOrinaAnt[,1])))))


# procesado tabla de datosantro
  

antro <- read.csv("data/datosAntropometricosCardiovasculares.csv", sep = ";", dec = ",")

for (i in seq(1,nrow(antro))){
  if (antro$Nº.Volunt.[i] <= 46){
    antro$Endulzante[i] <- "ST"
    
  }
  if ((antro$Nº.Volunt.[i] >= 51) & (antro$Nº.Volunt.[i] <= 96)){
    antro$Endulzante[i] <- "SU"
    
  }
  if ((antro$Nº.Volunt.[i] >= 101) & (antro$Nº.Volunt.[i] <= 146)){
    antro$Endulzante[i] <- "SA"
    
  }  
}


## funcion rescale funciona

x<-seq(1,20)
y <- c(30,31,34,35,37,38,40,42,43,45,47,49,50)
library(scales)
round(rescale (y, to=c(1,20), from=c(30,50)))


    
    
  }
}



