#' @title Create Distance Category
#' @description To bulid a barplot with x = Distanz the Variabe have to be discrete. This function groups the distance in categories.
#' @param totalDistance datatable with column Gesamtentfernung
#' @export

createDistanceCategory <- function(totalDistances) {
  
  newtotalDistances <- mutate(totalDistances, "Kategorie" = NA)
  
  newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung <= 100] <- "0-100"
  newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 100 & newtotalDistances$Gesamtentfernung <= 200] <- "100-200"
  newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 200 & newtotalDistances$Gesamtentfernung <= 400] <- "200-400"
  newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 400 & newtotalDistances$Gesamtentfernung <= 800] <- "400-800"
  newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 800 & newtotalDistances$Gesamtentfernung <= 1600] <- "800-1600"
  newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 1600 & newtotalDistances$Gesamtentfernung <= 3200] <- "1600-3200"
  newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 3200 & newtotalDistances$Gesamtentfernung <= 6400] <- "3200-6400"
  newtotalDistances$Kategorie[newtotalDistances$Gesamtentfernung > 6400 & newtotalDistances$Gesamtentfernung <= 12800] <- "6400-12800"
  newtotalDistances$Kategorie[is.na(newtotalDistances$Gesamtentfernung) == TRUE] <- "NA"
  
  return(newtotalDistances)
}