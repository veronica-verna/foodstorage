library("geosphere")

createFlights <- function(i, tD = FlightDists, pEx = producersExist, pOE = productOriginExist){
  productOriginExist <- pOE
  producersExist <- pEx
  totalDistances <- tD
  pE <- which(producersExist$Lieferant == totalDistances$Lieferant[i])
  if(totalDistances$Lieferantentyp[i] == "Zwischenhaendler" & 
     totalDistances$Lieferant[i] %in% unique(productOriginExist$Lieferant)){
    
    z <- which(productOriginExist$Lieferant == totalDistances$Lieferant[i] &
                 productOriginExist$Produkte_Zusammenfassung == totalDistances$Produkte_Zusammenfassung[i] &
                 productOriginExist$Ort == totalDistances$Ort[i])
    gcIntermediate(coordinates(producersExist)[pE,], coordinates(productOriginExist)[z,], sp = T, addStartEnd = T)
  }
}

createCurves <- function(i, tD = totalDistances, pEx = producersExist, pOE = productOriginExist){
  productOriginExist <- pOE
  producersExist <- pEx
  totalDistances <- tD
  pE <- which(producersExist$Lieferant == totalDistances$Lieferant[i])
  if(totalDistances$Lieferantentyp[i] == "Zwischenhaendler" & 
     totalDistances$Lieferant[i] %in% unique(productOriginExist$Lieferant)){
    
    z <- which(productOriginExist$Lieferant == totalDistances$Lieferant[i] &
                 productOriginExist$Produkte_Zusammenfassung == totalDistances$Produkte_Zusammenfassung[i] &
                 productOriginExist$Ort == totalDistances$Ort[i])
    Lines(list(Line(gcIntermediate(coordinates(Kornkammer), coordinates(producersExist)[pE,], addStartEnd = T)), 
               Line(gcIntermediate(coordinates(producersExist)[pE,], coordinates(productOriginExist)[z,], addStartEnd = T))) , ID = i)
  } else {
    Lines(Line(gcIntermediate(coordinates(Kornkammer), coordinates(producersExist)[pE,], addStartEnd = T)), ID = i)
  }
}

liste <- list()
for(i in 1:nrow(totalDistances)){
  liste[[i]] <- createCurves(i)
}

producers <- SpatialLines(liste, proj4string = crs(producersExist)) #crs(producersExist)
producersInfo <- SpatialLinesDataFrame(producers, totalDistances)


FlightDists <- totalDistances[totalDistances$Lieferantentyp == "Zwischenhaendler" & 
                                totalDistances$Lieferant %in% unique(productOriginExist$Lieferant),]

linepath <- lapply(1:nrow(FlightDists), createFlights)

ll0 <- lapply( linepath , function(x) `@`(x , "lines") )
ll1 <- lapply( unlist( ll0 ) , function(y) `@`(y,"Lines") )
ll2 <- lapply(ll1, function(z) Lines(z, ID = 1))

for(i in 1:length(ll2)){
  ll2[[i]]@ID <- as.character(i)
}

Sl <- SpatialLines(ll2, proj4string = crs(producersExist))

#crs(producersExist)
producersInfoFlight <- SpatialLinesDataFrame(Sl, FlightDists)

