# a function to test prepare functions
testprepare <- function(vec.of.Dates = kornumsatz$Datum, 
                        vec.of.products = kornumsatz$Produkt,
                        name.of.product = Getreide,
                        ) {
  
  # for is.Date; cheeck if vec.of.Dates is a Date ####
  require(lubridate) 
  if (is.Date(vec.of.Dates) == FALSE) 
    stop("Your vector of Dates has to be in format as.Date()")
  
  # create sortbydays ####
  Datum <- seq(from=range(vec.of.Dates)[1], 
               to=range(vec.of.Dates)[2], 
               by="day") 
  Tag_Nr <- 1:length(Datum)
  sortbydays <- data.frame(Tag_Nr, Datum)
  
  if (is.factor(vec.of.products) == FALSE) 
    stop("Vector of products has to be a factor")
  
  if (length(vec.of.Dates) != length(vec.of.products)) stop("Length differ: dates and products")
  
  # cbind "Produkt" with "Position" - create sortbypos ####
  Position <- 1:length(vec.of.products)
  sortbypos <- data.frame(Position = Position, Produkt = vec.of.products)
  sortbypos <- cbind(sortbypos, Datum = vec.of.Dates[sortbypos$Position])
  
  if (length(name.of.product) == 1) {
    # start here: sortbypos only with name.of.product
    sortbypos <- sortbypos[sortbypos$Produkt == name.of.product, ]  
    
    # join comulative change to sortbypos
    if (what.plotting == "Verzehr") {
      # security checks:
      #if (is.na(plotting.options$comul.change) == TRUE) 
      #stop("To plot consumption, comulative change is necessary")
      #if (length(plotting.options$comul.change) != nrow(sortbypos)) 
      #stop("length differ! Probably you didn't specify the product for comulative change")
      sortbypos <- cbind(sortbypos, 
                         MengeKum = plotting.options$comul.change[sortbypos$Position])
      
      # merge sortbypos and sortbydays, by days of course
      require(data.table) # v1.9.5+
      sortbydays <- merge(sortbydays, sortbypos, by='Datum', all=T)
      
      # fill NAs with numbers
      for (i in sortbydays$Tag_Nr) {
        if(is.na(sortbydays$MengeKum[i])==TRUE) sortbydays$MengeKum[i] <- 0
      }
      
      # Create the consumption
      MengeKonsum <- numeric(nrow(sortbydays))
      for (i in sortbydays$Tag_Nr) {
        if(sortbydays$MengeKum[i] <= 0) {
          MengeKonsum[i] <- abs(sortbydays$MengeKum[i])
        } else MengeKonsum[i] <- 0
      }
      sortbydays <- cbind(sortbydays, MengeKonsum)
      
      # and here is the result
      table <- data.frame(Datum = sortbydays$Datum, 
                          Tag_Nr = sortbydays$Tag_Nr, 
                          Verzehr = sortbydays$MengeKonsum)
      result <- list(data = table, name.of.product = name.of.product)
      return(result)
    }
    
    # vectors that later on you want to plot join "sortbypos"
    # case: plot the food storage ####
    if (what.plotting == "Warenbestand") {
      # security checks:
      if (is.na(plotting.options$comul.change) == TRUE | 
          is.na(plotting.options$food.storage) == TRUE) 
        stop("If you want to plott food storage, you need comulative change and a vector of storage. Both same length")
      if (length(plotting.options$comul.change) != length(vec.of.Dates) | 
          length(plotting.options$food.storage) != length(vec.of.Dates)) 
        stop("length differ! Probably you didn't specify the product for comulative change")
      
      # join comulative change and food storage to sortbypos
      sortbypos <- cbind(sortbypos, 
                         MengeKum = plotting.options$comul.change[sortbypos$Position], 
                         Bestand_Einheit=plotting.options$food.storage[sortbypos$Position])
      
      # merge sortbypos and sortbydays, by days of course
      require(data.table) # v1.9.5+
      sortbydays <- merge(sortbydays, sortbypos, by='Datum', all=T)
      
      # fill NAs with numbers
      for (i in 1:nrow(sortbydays)) {
        if(is.na(sortbydays$Bestand_Einheit[i])==TRUE) {
          sortbydays$MengeKum[i] <- 0
          sortbydays$Bestand_Einheit[i] <- sum(sortbydays$MengeKum[1:i])
        }
      }
      
      table <- data.frame(Datum = sortbydays$Datum,
                          Tag_Nr = sortbydays$Tag_Nr, 
                          Warenbestand = sortbydays$Bestand_Einheit)
      result <- list(data = table, name.of.product = name.of.product)
      return(result)
    }
    
    # food storage & consumption in one data frame
    if (what.plotting == "all") {
      # join comulative change and food storage to sortbypos
      sortbypos <- cbind(sortbypos, 
                         MengeKum = plotting.options$comul.change[sortbypos$Position],
                         Bestand_Einheit=plotting.options$food.storage[sortbypos$Position])
      
      # merge sortbypos and sortbydays, by days of course
      require(data.table) # v1.9.5+
      sortbydays <- merge(sortbydays, sortbypos, by='Datum', all=T)
      
      # fill NAs with numbers
      for (i in 1:nrow(sortbydays)) {
        if(is.na(sortbydays$Bestand_Einheit[i])==TRUE) {
          sortbydays$MengeKum[i] <- 0
          sortbydays$Bestand_Einheit[i] <- sum(sortbydays$MengeKum[1:i])
        }
      }
      
      # Create the consumption
      MengeKonsum <- numeric(nrow(sortbydays))
      for (i in sortbydays$Tag_Nr) {
        if(sortbydays$MengeKum[i] <= 0) {
          MengeKonsum[i] <- abs(sortbydays$MengeKum[i])
        } else MengeKonsum[i] <- 0
      }
      sortbydays <- cbind(sortbydays, MengeKonsum)
      
      # and here is the result
      table <- data.frame(Datum = sortbydays$Datum, 
                          Tag_Nr = sortbydays$Tag_Nr, 
                          Warenbestand = sortbydays$Bestand_Einheit,
                          Verzehr = sortbydays$MengeKonsum)
      result <- list(data = table, name.of.product = name.of.product)
      return(result)
      
    }
  } 
  else {
    group_size <- length(name.of.product)
    for (i in 1:group_size) {
      # start here: nrow(sortbypos) == length('MengeKum')
      assign(x = name.of.product[1], value = sortbypos[sortbypos$Produkt == name.of.product[i], ]) 
      
}