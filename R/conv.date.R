### convert date to german format ###

conv.date <- function (date) {
  library(lubridate)
  if (is.Date(date) == FALSE) stop ("date must be as.Date format")
  
  day_nr <- as.character(day(date))
  mon_word <- month.abb[month(date)]
  year_nr <- year(date)
  
  return(paste(paste(day_nr, mon_word, sep = ". "),year_nr))
}