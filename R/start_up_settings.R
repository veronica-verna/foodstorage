# sort data, add columns, adapt product names, etc ####

startup.settings <- function(table, importPRODUCTS, reduce = TRUE) {
  
  
  kornumsatz <- table
  Position <- 1:nrow(kornumsatz)
  kornumsatz <- cbind(Position, kornumsatz)
  kornumsatz$Datum <- as.Date(kornumsatz$Datum, format="%d/%m/%Y")
  kornumsatz$Produkt <- as.character(kornumsatz$Produkt)
  VPE <- numeric(nrow(kornumsatz))
  kornumsatz <- cbind(kornumsatz, VPE)
  
  
  ### verschiedene Produktnamen, aber gleiches Produkt, zusammengefasst ####
  starting.csv <- importPRODUCTS
  starting.csv$Produkte_App <- as.character(starting.csv$Produkte_App)
  starting.csv$Produkte_Zusammenfassung <- as.character(starting.csv$Produkte_Zusammenfassung)
  starting.csv$Verpackungseinheit <- as.numeric(starting.csv$Verpackungseinheit)
  for (i in 1:nrow(starting.csv)) {
    kornumsatz[kornumsatz$Produkt == starting.csv[i,1],]$Produkt <- rep(starting.csv[i,2], nrow(kornumsatz[kornumsatz$Produkt == starting.csv[i,1], ]))
    kornumsatz[kornumsatz$Produkt == starting.csv[i,2],]$VPE <- rep(starting.csv[i,6], nrow(kornumsatz[kornumsatz$Produkt == starting.csv[i,2], ]))
  }
  
  # Korrektur Bohnen
  kornumsatz[kornumsatz$Produkt == "Bohnen Borlotti" & kornumsatz$Datum >= as.Date("2016-10-13", origin="1970-01-01"), ]$Produkt <- "Bohnen schwarz"
  
  rm(VPE, i, Position)
  
  kornumsatz$Produkt <- as.factor(kornumsatz$Produkt)
  # one date, two rows -> reduce them to one
  if (reduce == TRUE) kornumsatz <- reduceONE(kornumsatz)
  return(kornumsatz)
}