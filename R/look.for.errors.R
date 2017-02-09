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