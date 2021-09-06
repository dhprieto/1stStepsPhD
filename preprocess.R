library()

preprocess <- function(tablePath){
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
return(table)
}
  
  

tableName = "cronicoOrinaAntLimpio.csv"

rootDir = "data/"

tablePath = paste0(rootDir,tableName)

cronicoOrinaAnt <- preprocess (tablePath)

View(cronicoOrinaAnt)

