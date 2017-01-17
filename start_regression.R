## making a regression for each product ###

kornumsatz_80 <- kornumsatz[kornumsatz$MengeKum > 0 & kornumsatz$Produkt == "Sonnenblumenkerne",]
kornumsatz_80[nrow(kornumsatz_80),]
kornumsatz_80 <- kornumsatz[kornumsatz$Produkt == "Sonnenblumenkerne" & kornumsatz$Datum >= kornumsatz_80[nrow(kornumsatz_80),]$Datum,]
range(kornumsatz_80$Datum)
Datum = seq(from=range(kornumsatz_80$Datum)[1], to=range(kornumsatz_80$Datum)[2], by="day")
sortbydays <- data.frame(Tag_Nr = 1:length(Datum), Datum = Datum)
head(sortbydays)
sortbydays <- merge(sortbydays, kornumsatz_80[,c(2,3)], by='Datum', all=T)
sortbydays$MengeKum[is.na(sortbydays$MengeKum)] <- 0
sortbydays
