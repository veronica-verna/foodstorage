#' @export
# sort data, add columns, adapt product names, etc ####

startup.settings <- function(table, importPRODUCTS, reduce = TRUE) {
  
  
  kornumsatz <- table
  Position <- 1:nrow(kornumsatz)
  kornumsatz <- cbind(Position, kornumsatz)
  kornumsatz$Tag <- as.Date(kornumsatz$Tag, format="%d/%m/%Y")
  kornumsatz$Produkt <- as.character(kornumsatz$Produkt)
  VPE <- numeric(nrow(kornumsatz))
  kornumsatz <- cbind(kornumsatz, VPE)
  
  
  ### verschiedene Produktnamen, aber gleiches Produkt, zusammengefasst ####
  starting_csv <- importPRODUCTS
  starting_csv$Produkte_App <- as.character(starting_csv$Produkte_App)
  starting_csv$Produkte_Zusammenfassung <- as.character(starting_csv$Produkte_Zusammenfassung)
  starting_csv$Verpackungseinheit <- as.numeric(starting_csv$Verpackungseinheit)
  for (i in 1:nrow(starting_csv)) {
    # replace every kornumsatz$Produkt with the summary of this product, 
    # found in starting_csv$Produkt_Zusammenfassung
    kornumsatz[kornumsatz$Produkt == starting_csv[i,1],]$Produkt <- 
      rep(starting_csv[i,2], nrow(kornumsatz[kornumsatz$Produkt == starting_csv[i,1], ]))
    # add VPE, found in starting_csv$Verpackungseinheit
    kornumsatz[kornumsatz$Produkt == starting_csv[i,2],]$VPE <- 
      rep(starting_csv[i,6], nrow(kornumsatz[kornumsatz$Produkt == starting_csv[i,2], ]))
  }
  
  # Korrektur Bohnen
  kornumsatz[kornumsatz$Produkt == "Bohnen Borlotti" & kornumsatz$Tag >= as.Date("2016-10-13", origin="1970-01-01"), ]$Produkt <- "Bohnen schwarz"
  
  rm(VPE, i, Position)
  
  kornumsatz$Produkt <- as.factor(kornumsatz$Produkt)
  # one date, two rows -> reduce them to one
  if (reduce == TRUE) kornumsatz <- reduceONE(get("kornumsatz"))
  return(kornumsatz)
}