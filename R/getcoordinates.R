#' @title add coordinates to table 
#' @description This function adds the x and y coordinates to a table with a given country names 
#' @param productOrigin table with column "Ort" (used to get the coordinates) and two empty columns "xCoord" & "yCoord"
#' @details the function have to be executet several times to get all coordinates, due to a high failure rate of the integrated function geocode()
#' @export productOrigin returns a table with fulfilld x and y coordinates

getCoordinates <- function(productOrigin) {
# add x coordinates to table
for (i in 1:dim(productOrigin)[1]) {
  if ( is.na(productOrigin$Ort[i]) == F & is.na(productOrigin$xCoord[i]) == T) {
    productOrigin$xCoord[i] <- geocode(productOrigin$Ort[i])[1]
  }
}
  
# add y coordinates to table
for (i in 1:dim(productOrigin)[1]) {
  if ( is.na(productOrigin$Ort[i]) == F & is.na(productOrigin$yCoord[i]) == T) {
      productOrigin$yCoord[i] <- geocode(productOrigin$Ort[i])[2]
  }
}
  return(productOrigin)
}


