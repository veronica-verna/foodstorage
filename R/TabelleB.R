#' @param kornumsatz description for kornumsatz

TabelleB<- function(kornumsatz = kornumsatz, was= "alles"){
  level2nd <- level2ndFUN(kornumsatz, product.group, deliverers)
  cS <- currentStorage(level2nd$ONEprod, rawlist = T)[c(4,5,6,8,10)]
  uniqueStarting <- starting_csv[-which(duplicated(starting_csv$Produkte_Zusammenfassung)==T),-1]
  uniqueStarting$Produkte_Zusammenfassung <- as.character(uniqueStarting$Produkte_Zusammenfassung)
  Info.mit.Bestand <- merge(uniqueStarting, cS, by.x="Produkte_Zusammenfassung", by.y = "Produkt")
  Info.mit.Bestand <- Info.mit.Bestand[,c("Produkte_Zusammenfassung","Bestand.Einheit","Einheit","Produktgruppe","Lieferant","Lieferant2","Preis","Verpackungseinheit")]
  names(Info.mit.Bestand) <- c("Produkte", "Aktueller Bestand", "Einheit", "Produktgruppe", "Lieferant", "Lieferant2", "Preis", "Verpackungseinheit")
  Info.mit.Bestand$Bestand.in.prozent <- Info.mit.Bestand$`Aktueller Bestand`/Info.mit.Bestand$Verpackungseinheit
  alles <- Info.mit.Bestand
  gibts <- Info.mit.Bestand[which(Info.mit.Bestand$`Aktueller Bestand` > 0),]
  leer <- Info.mit.Bestand[which(Info.mit.Bestand$`Aktueller Bestand` == 0),]
  if(was=="alles"){data <- Info.mit.Bestand}
  if(was=="gibts"){data <- gibts}
  if(was=="leer"){data <- leer}
  TabelleB <- datatable(data, 
                        selection = "single",
                        filter = 'top',
            options=list(columnDefs = list(list(visible=FALSE, targets=9))),
            caption = 'Bestand weniger als 20 %: orange; weniger als 10 %: rot')%>%
    formatStyle(2, valueColumns=9,
                color = JS("value < 0.1 ? 'red' : value > 0.1 & value < 0.2 ? 'orange' : 'blue'"))
  return(TabelleB)
}

TabelleB(kornumsatz = kornumsatz, was="leer")
### Funktion schreiben (merge von current storage und starting csv)
### export (roxygen2 doku internet)
### Doku schreiben 
### 