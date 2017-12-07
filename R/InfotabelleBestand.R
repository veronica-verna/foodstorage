
cS <- currentStorage(level2nd$ONEprod, rawlist = T)[c(4,5,6,8,10)]
uniqueStarting <- starting_csv[-which(duplicated(starting_csv$Produkte_Zusammenfassung)==T),-1]
uniqueStarting$Produkte_Zusammenfassung <- as.character(uniqueStarting$Produkte_Zusammenfassung)
Info.mit.Bestand <- merge(uniqueStarting, cS, by.x="Produkte_Zusammenfassung", by.y = "Produkt")
Info.mit.Bestand <- Info.mit.Bestand[,c(1,8,6,4,2,3,7,5)]
names(Info.mit.Bestand) <- c("Produkte", "Aktueller Bestand", "Einheit", "Produktgruppe", "Lieferant", "Lieferant2", "Preis", "Verpackungseinheit")

Info.mit.Bestand$Bestand.in.prozent <- Info.mit.Bestand$`Aktueller Bestand`/Info.mit.Bestand$Verpackungseinheit


datatable(Info.mit.Bestand, selection = "single",filter = 'top',
          options=list(columnDefs = list(list(visible=FALSE, targets=9))),
          caption = 'Bestand weniger als 20 %: orange; weniger als 10 %: rot')%>%
  formatStyle(2, valueColumns=9,
              color = JS("value < 0.1 ? 'red' : value > 0.1 & value < 0.2 ? 'orange' : 'blue'"))

