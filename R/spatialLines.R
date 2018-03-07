##' Create straight Lines-objects from the origin, to the supplier and than to the Kornkammer
##' @param i the ID of the product (i.e. the rownumber in the totalDistances-table)
##' @param tD the table totalDistances with the informations "Lieferantentyp", "Lieferant", "Produkte_Zusammenfassung" and "Ort"
##' @param pEx the SpatialPointsDataFrame producersExist with cooridinates and the information "Lieferant"
##' @param pEx the SpatialPointsDataFrame productOriginExist with coordinates and the information "Lieferant", "Produkte_Zusammenfassung" and "Ort"
##' @return a Lines-object where the transport routes for every product are stored in. The Lines are straight lines on the map.
##' @export
# Function to construct a lines-file for every product to its origin:
## With straight lines on the map:
createLines <- function(i, tD = totalDistances, pEx = producersExist, pOE = productOriginExist){
  productOriginExist <- pOE
  producersExist <- pEx
  totalDistances <- tD
  pE <- which(producersExist$Lieferant == totalDistances$Lieferant[i])
  if(totalDistances$Lieferantentyp[i] == "Zwischenhaendler" & 
     totalDistances$Lieferant[i] %in% unique(productOriginExist$Lieferant)){
    
    z <- which(productOriginExist$Lieferant == totalDistances$Lieferant[i] &
                 productOriginExist$Produkte_Zusammenfassung == totalDistances$Produkte_Zusammenfassung[i] &
                 productOriginExist$Ort == totalDistances$Ort[i])
    Li <- Lines(Line(rbind(coordinates(Kornkammer), coordinates(producersExist)[pE,], coordinates(productOriginExist)[z,])), ID = i)
  } else {
    Li <- Lines(Line(rbind(coordinates(Kornkammer), coordinates(producersExist)[pE,])), ID = i)
  }
  return(Li)
}


##' Create Lines-objects from the origin, to the supplier and than to the Kornkammer
##' 
##' @description A Lines-object will be crated with the gcIntermediate-function from the "geosphere" package. The Lines are drawn on a great Circle.
##' @param i the ID of the product (i.e. the rownumber in the totalDistances-table)
##' @param tD the table totalDistances with the informations "Lieferantentyp", "Lieferant", "Produkte_Zusammenfassung" and "Ort"
##' @param pEx the SpatialPointsDataFrame producersExist with cooridinates and the information "Lieferant"
##' @param pEx the SpatialPointsDataFrame productOriginExist with coordinates and the information "Lieferant", "Produkte_Zusammenfassung" and "Ort"
##' @return a Lines-object where the transport routes for every product are stored in.
##' @export
## With curved lines on the map:
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
    Li <- Lines(list(Line(gcIntermediate(coordinates(Kornkammer), coordinates(producersExist)[pE,], addStartEnd = T)), 
               Line(gcIntermediate(coordinates(producersExist)[pE,], coordinates(productOriginExist)[z,], addStartEnd = T))) , ID = i)
  } else {
    Li <- Lines(Line(gcIntermediate(coordinates(Kornkammer), coordinates(producersExist)[pE,], addStartEnd = T)), ID = i)
  }
  return(Li)
}


