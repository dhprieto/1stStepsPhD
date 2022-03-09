source("scripts/preprocess.R")

tabla1 <- reading("data/cronicoOrinaAntLimpio.csv", nasPercentageCol = 0.3, nasRow = T)
write.csv(tabla1, "data/prueba.csv")
tabla1pp <- preprocessTables("data/", "prueba.csv")

View(tabla1pp$tablaSinEsc)
