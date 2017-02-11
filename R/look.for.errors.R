### looking for errors in prepare ###

look.for.errors <- function(date.vec, table, warnings = TRUE) {
  dates <- seq(from = range(date.vec)[1], to = range(date.vec)[2], by = 'day')
  if (nrow(table) != length(dates)) {
    # create an empty vector
    double_date <- as.Date(numeric(nrow(table)-length(dates)), origin = "1970-01-01")
    for (i in 2:nrow(table)) {
      # get the next position where to write the "double_date"
      z <- which(double_date %in% as.Date("1970-01-01"))[1]
      if (date.vec[i] != date.vec[i-1] + 1) {
        double_date[z] <- date.vec[i]
        if (is.na(z)) stop("there is another double date, but double date vector is full")
      }
    } 
    if (warnings == TRUE) warning("Length differ: at least for one product exists more than one date")
    return(double_date)
  }
}

## no more necessary, but the following code was part of prepare
# looking for an error - each row should represent one date ##
# two same dates behind each other are not allowed ###
if(class(look.for.errors(sortbydays$Datum, sortbydays, warnings = FALSE)) == "Date") {
  double.date <- look.for.errors(sortbydays$Datum, sortbydays, warnings = FALSE) 
  return(double.date)
  days.to.correct <- unique(double.date)
  for (i in 1:length(days.to.correct)) {
    sortbydays[sortbydays$Datum == days.to.correct[i],]$MengeKum[1] <- sum(sortbydays[sortbydays$Datum == days.to.correct[i],]$MengeKum) # replace storage changing
    nr <- nrow(sortbydays[sortbydays$Datum == days.to.correct[i],])
    sortbydays <- sortbydays[-which(sortbydays$Datum == days.to.correct[i])[(2:nr)], ]
    sortbydays[sortbydays$Datum == days.to.correct[i],]$Bestand_Einheit <- sortbydays[sortbydays$Datum == days.to.correct[i],]$MengeKum + sortbydays[sortbydays$Datum == days.to.correct[i] - 1,]$Bestand_Einheit # replace food.storage
  }
}