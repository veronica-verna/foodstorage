# sort data, add columns, adapt product names, etc ####

startup.settings <- function(table, importPRODUCTS, reduce = TRUE) {
  
  
  kornumsatz <- table
  Position <- 1:nrow(kornumsatz)
  kornumsatz <- cbind(Position, kornumsatz)
  kornumsatz$Datum <- as.Date(kornumsatz$Datum, format="%d/%m/%Y")
  kornumsatz$Produkt <- as.factor(kornumsatz$Produkt)
  VPE <- numeric(nrow(kornumsatz))
  kornumsatz <- cbind(kornumsatz, VPE)
  
  
  ### verschiedene Produktnamen, aber gleiches Produkt, zusammengefasst ####
  starting.csv <- importPRODUCTS
  for (i in nrow(starting.csv)) {
    kornumsatz[kornumsatz$Produkt == starting.csv[i,1],]$Produkt <- starting.csv[i,2]
    kornumsatz[kornumsatz$Produkt == starting.csv[i,2],]$VPE <- starting.csv[i,6]
  }
  
  # Korrektur Bohnen
  kornumsatz[kornumsatz$Produkt == "Bohnen Borlotti" & kornumsatz$Datum >= as.Date("2016-10-13", origin="1970-01-01"), ]$Produkt <- "Bohnen"
  
  rm(VPE, i, Position)
  
  # one date, two rows -> reduce them to one
  if (reduce == TRUE) kornumsatz <- reduceONE(kornumsatz)
  return(kornumsatz)
}