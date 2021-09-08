
'''    
    antro_ST_1 <- subset(antro, antro$Endulzante == "ST" )
    antro_ST_3.5 <- subset(antro, antro$Endulzante == "ST" )
    antro_ST_12 <- subset(antro, antro$Endulzante == "ST" )
    antro_ST_24 <- subset(antro, antro$Endulzante == "ST" )
    antro_SU_1 <- subset(antro, antro$Endulzante == "SU")
    antro_SU_3.5 <- subset(antro, antro$Endulzante == "SU")
    antro_SU_12 <- subset(antro, antro$Endulzante == "SU")
    antro_SU_24 <- subset(antro, antro$Endulzante == "SU")
    antro_SA_1 <- subset(antro, antro$Endulzante == "SA")
    antro_SA_3.5 <- subset(antro, antro$Endulzante == "SA")
    antro_SA_12 <- subset(antro, antro$Endulzante == "SA")
    antro_SA_24 <- subset(antro, antro$Endulzante == "SA")
    
    
      if (agudo == T) {
        for (i in seq(1, nrow(table))){
          table[,1][i] <- gsub("U.*", "", table[,1][i])
          }
        } else {
        for (i in seq(1, nrow(table))){
          table[,1][i] <- gsub("[A-C].", "", table[,1][i])
        }
      }
    
    table_ST_1 <- subset(table, table$Endulzante == "ST"&table$Tiempo == "-1")
    table_ST_3.5 <- subset(table, table$Endulzante == "ST"&table$Tiempo == "3,5")
    table_ST_12 <- subset(table, table$Endulzante == "ST"&table$Tiempo == "12")
    table_ST_24 <- subset(table, table$Endulzante == "ST"&table$Tiempo == "24")
    table_SU_1 <- subset(table, table$Endulzante == "SU"&table$Tiempo == "-1")
    table_SU_3.5 <- subset(table, table$Endulzante == "SU"&table$Tiempo == "3,5")
    table_SU_12 <- subset(table, table$Endulzante == "SU"&table$Tiempo == "12")
    table_SU_24 <- subset(table, table$Endulzante == "SU"&table$Tiempo == "24")
    table_SA_1 <- subset(table, table$Endulzante == "SA"&table$Tiempo == "-1")
    table_SA_3.5 <- subset(table, table$Endulzante == "SA"&table$Tiempo == "3,5")
    table_SA_12 <- subset(table, table$Endulzante == "SA"&table$Tiempo == "12")
    table_SA_24 <- subset(table, table$Endulzante == "SA"&table$Tiempo == "24")
    
    
    
    
    checkReesc <- function(tabla, nColumna){
      first = T
      second = T
      for (i in seq(1, nrow(tabla))){

        if (first){
          first = F

          }
        else if (first == F & second == T & tabla[,nColumna][i] == tabla[,nColumna][i-1]){
          tabla[,nColumna][i] = tabla[,nColumna][i]+1
          second = F

        }
        else if (first == F & second == F & tabla[,nColumna][i] <= tabla[,nColumna][i-2]){
          tabla[,nColumna][i] = tabla[,nColumna][i-2]+2

        }
        else if (first == F & second == F & tabla[,nColumna][i] <= tabla[,nColumna][i-1]){
          tabla[,nColumna][i] = tabla[,nColumna][i-1]+1
        }
        
      
      }
      return(tabla)
      }
    
    antro_ST_1$numEq <- as.integer(rescale(antro_ST_1[,1], to=c(min(as.integer(table_ST_1[,1])),
                                                              max(as.integer(table_ST_1[,1])))))
    print(antro_ST_1$numEq)
    antro_ST_3.5$numEq <- as.integer(rescale(antro_ST_3.5[,1], to=c(min(as.integer(table_ST_3.5[,1])),
                                                                    max(as.integer(table_ST_3.5[,1])))))
    antro_ST_12$numEq<- as.integer(rescale(antro_ST_12[,1], to=c(min(as.integer(table_ST_12[,1])),
                                                                 max(as.integer(table_ST_12[,1])))))
    antro_ST_24$numEq <- as.integer(rescale(antro_ST_24[,1], to=c(min(as.integer(table_ST_24[,1])),
                                                                  max(as.integer(table_ST_24[,1])))))
    antro_SU_1$numEq <- as.integer(rescale(antro_SU_1[,1], to=c(min(as.integer(table_SU_1[,1])),
                                                                max(as.integer(table_SU_1[,1])))))
    antro_SU_3.5$numEq <- as.integer(rescale(antro_SU_3.5[,1], to=c(min(as.integer(table_SU_3.5[,1])),
                                                                    max(as.integer(table_SU_3.5[,1])))))
    antro_SU_12$numEq <- as.integer(rescale(antro_SU_12[,1], to=c(min(as.integer(table_SU_12[,1])),
                                                                  max(as.integer(table_SU_12[,1])))))
    antro_SU_24$numEq <- as.integer(rescale(antro_SU_24[,1], to=c(min(as.integer(table_SU_24[,1])),
                                                                  max(as.integer(table_SU_24[,1])))))
    antro_SA_1$numEq <- as.integer(rescale(antro_SA_1[,1], to=c(min(as.integer(table_SA_1[,1])),
                                                                max(as.integer(table_SA_1[,1])))))
    antro_SA_3.5$numEq <- as.integer(rescale(antro_SA_3.5[,1], to=c(min(as.integer(table_SA_3.5[,1])),
                                                                    max(as.integer(table_SA_3.5[,1])))))
    antro_SA_12$numEq <- as.integer(rescale(antro_SA_12[,1], to=c(min(as.integer(table_SA_12[,1])),
                                                                  max(as.integer(table_SA_12[,1])))))
    antro_SA_24$numEq <- as.integer(rescale(antro_SA_24[,1], to=c(min(as.integer(table_SA_24[,1])),
                                                                  max(as.integer(table_SA_24[,1])))))

    
    antroTotalReesc <- rbind(checkReesc(antro_ST_1,21),checkReesc(antro_ST_3.5,21),checkReesc(antro_ST_12,21),
                             checkReesc(antro_ST_24,21),checkReesc(antro_SU_1,21),checkReesc(antro_SU_3.5,21),
                             checkReesc(antro_SU_12,21),checkReesc(antro_SU_24,21),checkReesc(antro_SA_1,21),
                             checkReesc(antro_SA_3.5,21),checkReesc(antro_SA_12,21),checkReesc(antro_SA_24,21))
      
    print (checkReesc(antro_ST_1,21))
    
    #antroTotalReesc <- rbind(antro_ST, antro_SU, antro_SA)
    return(antroTotalReesc)
  }
'''  