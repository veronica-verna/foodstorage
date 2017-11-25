### one date, two rows -> reduce them to one row

reduceONE <- function(table, pro) {
  require(lubridate)
  #naming vectors
  if (!is.data.frame(table)) stop("table has to be a data frame")
  dates <- table$Tag
  if (!is.Date(dates)) stop("dates must be of class 'Date'")
  if (!is.factor(table$Produkt)) stop("products must be of class factor")
  dif.products <- levels(table$Produkt)
  len <- length(dif.products)
  
  # look for each product, if there exists more than one row for one date
  for (pro in 1:len) {
    tf <- table[table$Produkt == dif.products[pro], ]
    # create a table which contains duplicates
    double.dates <- tf[duplicated(tf$Tag), ]$Tag
    if (length(double.dates) == 0) next
    #return(dif.products[pro])
    dd.df <- tf[tf$Tag %in% double.dates, ]
    dd.df <- cbind(dd.df, dif.dates = as.numeric(c(1,diff(tf[tf$Tag %in% double.dates, ]$Tag))))
    #return(dd.df)
    pos.to.replace <- dd.df[dd.df$dif.dates != 0, ]$Position
    pos.to.delete <- dd.df[dd.df$dif.dates == 0, ]$Position
    #return(list(pos.to.delete, pos.to.replace))
    # calculate the values which are needed for replacement
    new.tf <- aggregate(tf['Menge'], tf['Tag'], FUN = sum)
    new.tf <- cbind(new.tf, Summe = aggregate(tf['Summe'], tf['Tag'], FUN = sum)$Summe)
    #return(new.tf)
    # as default: price is calculated by mean
    Preis <- round(new.tf$Summe / new.tf$Menge, 2)
    new.tf <- cbind(new.tf[,1:2], Preis = Preis, Summe = new.tf$Summe)
    new.tf[is.na(new.tf$Preis),]$Preis <- new.tf$Preis[which(is.na(new.tf$Preis)) - 1]
    #return(new.tf)
    # and now let's replace 'Menge', 'Preis' and 'Summe' ...
    table[table$Position %in% pos.to.replace, c(3,5,7)] <- new.tf[new.tf$Tag %in% double.dates, c(2,3,4)]
    #return(table)
    # ... and delete the duplicates
    table <- table[!(table$Position %in% pos.to.delete), ]
    #return(table)
    # at the end: calculate 'Bestand_Preis' and 'Bestand_Einheit'
    tf <- table[table$Produkt %in% dif.products[pro], ]
    #return(tf)
    rows.replacement <- which(tf$Tag %in% double.dates)
    #return(rows.replacement)
    if (rows.replacement[1] == 1) {
      day <- tf[rows.replacement[1],]$Tag
      sum.of.day <- round(sum(tf[tf$Tag %in% day, ]$Bestand.Einheit), 3)
      storage.unit <- round(tf[rows.replacement[-1],]$Menge + tf[rows.replacement[-1] - 1,]$Bestand.Einheit, 3)
      storage.unit <- c(sum.of.day, storage.unit)
    } else storage.unit <- round(tf[rows.replacement,]$Menge + tf[rows.replacement - 1,]$Bestand.Einheit, 3)
    storage.price <- round(storage.unit * tf[rows.replacement,]$Preis, 2)
    table[table$Position %in% pos.to.replace, c(8,9)] <- cbind(storage.unit, storage.price)
    # everything worked for Basmati, but not every storage value is correct because of different prices for the same product
  }
  return(table)
  
}