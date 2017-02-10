### one date, two rows -> reduce them to one row

reduceONE <- function(table) {
  #naming vectors
  if (is.data.frame(table) == FALSE) stop("table has to be a data frame")
  dates <- table$Datum
  if (is.Date(dates) == FALSE) stop("dates must be of class 'Date'")
  if (is.factor(table$Produkt) == FALSE) stop("products must be of class factor")
  dif.products <- levels(table$Produkt)
  len <- length(dif.products)
  
  # look for each product, if there exist more than one row for one date
  for (pro in 1:len) {
    tf <- table[table$Produkt == dif.products[pro], ]
    # create a table which contains duplicates
    double.dates <- tf[duplicated(tf$Datum), ]$Datum
    if (length(double.dates) == 0) next
    dd.df <- tf[tf$Datum %in% double.dates, ]
    dd.df <- cbind(dd.df, dif.dates = as.numeric(c(1,diff(tf[tf$Datum %in% double.dates, ]$Datum))))
    pos.to.replace <- dd.df[dd.df$dif.dates != 0, ]$Position
    pos.to.delete <- dd.df[dd.df$dif.dates == 0, ]$Position
    #return(pos.to.replace)
    # calculate the values which are needed for replacement
    new.tf <- aggregate(tf['MengeKum'], tf['Datum'], FUN = sum)
    new.tf <- cbind(new.tf, Umsatz = aggregate(tf['Umsatz'], tf['Datum'], FUN = sum)$Umsatz)
    #return(str(new.tf))
    # as default: price is calculated by mean
    Preis <- round(new.tf$Umsatz / new.tf$MengeKum, 2)
    new.tf <- cbind(new.tf[,1:2], Preis = Preis, Umsatz = new.tf$Umsatz)
    new.tf[is.na(new.tf$Preis),]$Preis <- new.tf$Preis[which(is.na(new.tf$Preis)) - 1]
    #return(new.tf[new.tf$Datum %in% double.dates, c(2,3,4)])
    # and now let's replace 'MengeKum', 'Preis' and 'Umsatz' ...
    table[table$Position %in% pos.to.replace, c(4,6,7)] <- new.tf[new.tf$Datum %in% double.dates, c(2,3,4)]
    # ... and delete the duplicates
    table <- table[!(table$Position %in% pos.to.delete), ]
    #return(nrow(table))
    # at least: calculate 'Bestand_Preis' and 'Bestand_Einheit'
    tf <- table[table$Produkt %in% dif.products[pro], ]
    rows.replacement <- which(tf$Datum %in% double.dates)
    storage.unit <- round(tf[rows.replacement,]$MengeKum + tf[rows.replacement - 1,]$Bestand_Einheit, 3)
    storage.price <- round(storage.unit * tf[rows.replacement,]$Preis, 2)
    table[table$Position %in% pos.to.replace, c(8,9)] <- cbind(storage.unit, storage.price)
    # everything worked for Basmati, but not every storage value is correct because of different prices for the same product
  }
  
}